module TheNutcracker where

import Data.Foldable
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B

type Pulse      = Float
type Hz         = Float
type Seconds    = Float
type Semitones  = Float
type Beats      = Float
type Volume     = Float
type SampleRate = Float
type BPM        = Beats
type Wave       = [Pulse]
type Tone       = (String,Float)

beatDuration :: BPM -> Seconds
beatDuration bpm = 60.0 / bpm

getStepFromSampleRate :: SampleRate -> Hz -> Pulse
getStepFromSampleRate sr freq = (freq * 2 * pi) / sr

applyVolume :: Volume -> Wave -> Wave
applyVolume vol = map (* vol)

mapApplyVolume :: Volume -> [Wave] -> [Wave]
mapApplyVolume vol = map (applyVolume vol)

applyCompression :: Float -> Wave -> Wave
applyCompression amt out = zipWith3 (\x y z -> x * y * z) atk out rel
  where
    atk = map (min 1.0) [0.0, amt ..]
    rel = reverse $ take (length out) atk

mapApplyCompression :: Float -> [Wave] -> [Wave]
mapApplyCompression amt = map (applyCompression amt)

wave :: Hz -> Seconds -> SampleRate -> Wave
wave freq dur sr = map (sin . (* stp)) [0.0..sr * dur]
  where
    stp = getStepFromSampleRate sr freq

waveSequenceWithSampleRate :: SampleRate -> [(Hz,Seconds)] -> [Wave]
waveSequenceWithSampleRate sr waves
                         | null waves = []
                         | otherwise  = wave freq dur sr : waveSequenceWithSampleRate sr rem
                         where
                           freq = fst $ head waves
                           dur  = snd $ head waves
                           rem  = tail waves

semitonesFromTune :: Semitones -> Hz -> Hz
semitonesFromTune n tune = tune * (2 ** (1.0 / 12.0)) ** n

matchToneName :: Tone -> Semitones
matchToneName tone
            | t == "A"               =        (pos - 4) * 12
            | t == "A#" || t == "Bb" = 1    + (pos - 4) * 12
            | t == "B"               = 2    + (pos - 4) * 12
            | t == "C"               = (-9) + (pos - 4) * 12
            | t == "C#" || t == "Db" = (-8) + (pos - 4) * 12
            | t == "D"               = (-7) + (pos - 4) * 12
            | t == "D#" || t == "Eb" = (-6) + (pos - 4) * 12
            | t == "E"               = (-5) + (pos - 4) * 12
            | t == "F"               = (-4) + (pos - 4) * 12
            | t == "F#" || t == "Gb" = (-3) + (pos - 4) * 12
            | t == "G"               = (-2) + (pos - 4) * 12
            | t == "G#" || t == "Ab" = (-1) + (pos - 4) * 12
            | otherwise = 0
            where t   = fst tone
                  pos = snd tone

tuneSequence :: Hz -> [(Semitones,Seconds)] -> [(Hz,Seconds)]
tuneSequence tune notes
           | null notes = []
           | otherwise  = (semitonesFromTune n tune, secs) : tuneSequence tune rem
           where n    = fst $ head notes
                 secs = snd $ head notes
                 rem  = tail notes

quickMusic :: Volume -> Float -> SampleRate -> Hz -> Tone -> [(Semitones,Seconds)] -> Wave
quickMusic vol comp sr tune tone notes =
  concat $
    mapApplyVolume vol $
      mapApplyCompression comp $
        waveSequenceWithSampleRate sr $ 
          tuneSequence baseFreq notes
  where baseFreq = semitonesFromTune (matchToneName tone) tune

saveWaveToRawFile :: String -> Wave -> IO ()
saveWaveToRawFile path wave = B.writeFile path $ B.toLazyByteString $ foldMap B.floatLE wave
