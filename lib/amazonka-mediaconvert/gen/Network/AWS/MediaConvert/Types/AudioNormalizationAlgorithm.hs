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
        ItuBs17701,
        ItuBs17702,
        ItuBs17703,
        ItuBs17704
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm' Lude.Text
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

pattern ItuBs17701 :: AudioNormalizationAlgorithm
pattern ItuBs17701 = AudioNormalizationAlgorithm' "ITU_BS_1770_1"

pattern ItuBs17702 :: AudioNormalizationAlgorithm
pattern ItuBs17702 = AudioNormalizationAlgorithm' "ITU_BS_1770_2"

pattern ItuBs17703 :: AudioNormalizationAlgorithm
pattern ItuBs17703 = AudioNormalizationAlgorithm' "ITU_BS_1770_3"

pattern ItuBs17704 :: AudioNormalizationAlgorithm
pattern ItuBs17704 = AudioNormalizationAlgorithm' "ITU_BS_1770_4"

{-# COMPLETE
  ItuBs17701,
  ItuBs17702,
  ItuBs17703,
  ItuBs17704,
  AudioNormalizationAlgorithm'
  #-}
