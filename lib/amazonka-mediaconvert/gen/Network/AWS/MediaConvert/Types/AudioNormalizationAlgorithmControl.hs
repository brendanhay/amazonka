-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
  ( AudioNormalizationAlgorithmControl
      ( AudioNormalizationAlgorithmControl',
        CorrectAudio,
        MeasureOnly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CorrectAudio :: AudioNormalizationAlgorithmControl
pattern CorrectAudio = AudioNormalizationAlgorithmControl' "CORRECT_AUDIO"

pattern MeasureOnly :: AudioNormalizationAlgorithmControl
pattern MeasureOnly = AudioNormalizationAlgorithmControl' "MEASURE_ONLY"

{-# COMPLETE
  CorrectAudio,
  MeasureOnly,
  AudioNormalizationAlgorithmControl'
  #-}
