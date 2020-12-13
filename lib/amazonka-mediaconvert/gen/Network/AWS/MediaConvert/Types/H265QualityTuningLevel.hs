{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265QualityTuningLevel
  ( H265QualityTuningLevel
      ( H265QualityTuningLevel',
        SinglePass,
        SinglePassHq,
        MultiPassHq
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
newtype H265QualityTuningLevel = H265QualityTuningLevel' Lude.Text
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

pattern SinglePass :: H265QualityTuningLevel
pattern SinglePass = H265QualityTuningLevel' "SINGLE_PASS"

pattern SinglePassHq :: H265QualityTuningLevel
pattern SinglePassHq = H265QualityTuningLevel' "SINGLE_PASS_HQ"

pattern MultiPassHq :: H265QualityTuningLevel
pattern MultiPassHq = H265QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  SinglePass,
  SinglePassHq,
  MultiPassHq,
  H265QualityTuningLevel'
  #-}
