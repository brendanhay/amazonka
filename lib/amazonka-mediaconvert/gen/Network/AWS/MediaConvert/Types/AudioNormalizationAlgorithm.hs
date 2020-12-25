{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
  ( AudioNormalizationAlgorithm
      ( AudioNormalizationAlgorithm',
        AudioNormalizationAlgorithmItuBs17701,
        AudioNormalizationAlgorithmItuBs17702,
        AudioNormalizationAlgorithmItuBs17703,
        AudioNormalizationAlgorithmItuBs17704,
        fromAudioNormalizationAlgorithm
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm'
  { fromAudioNormalizationAlgorithm ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AudioNormalizationAlgorithmItuBs17701 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithmItuBs17701 = AudioNormalizationAlgorithm' "ITU_BS_1770_1"

pattern AudioNormalizationAlgorithmItuBs17702 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithmItuBs17702 = AudioNormalizationAlgorithm' "ITU_BS_1770_2"

pattern AudioNormalizationAlgorithmItuBs17703 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithmItuBs17703 = AudioNormalizationAlgorithm' "ITU_BS_1770_3"

pattern AudioNormalizationAlgorithmItuBs17704 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithmItuBs17704 = AudioNormalizationAlgorithm' "ITU_BS_1770_4"

{-# COMPLETE
  AudioNormalizationAlgorithmItuBs17701,
  AudioNormalizationAlgorithmItuBs17702,
  AudioNormalizationAlgorithmItuBs17703,
  AudioNormalizationAlgorithmItuBs17704,
  AudioNormalizationAlgorithm'
  #-}
