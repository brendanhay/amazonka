{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264QualityTuningLevel
  ( H264QualityTuningLevel
      ( H264QualityTuningLevel',
        HQTLSinglePass,
        HQTLSinglePassHq,
        HQTLMultiPassHq
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
newtype H264QualityTuningLevel = H264QualityTuningLevel' Lude.Text
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

pattern HQTLSinglePass :: H264QualityTuningLevel
pattern HQTLSinglePass = H264QualityTuningLevel' "SINGLE_PASS"

pattern HQTLSinglePassHq :: H264QualityTuningLevel
pattern HQTLSinglePassHq = H264QualityTuningLevel' "SINGLE_PASS_HQ"

pattern HQTLMultiPassHq :: H264QualityTuningLevel
pattern HQTLMultiPassHq = H264QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  HQTLSinglePass,
  HQTLSinglePassHq,
  HQTLMultiPassHq,
  H264QualityTuningLevel'
  #-}
