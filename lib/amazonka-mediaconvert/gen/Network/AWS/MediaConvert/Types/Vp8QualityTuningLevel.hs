{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
  ( Vp8QualityTuningLevel
      ( Vp8QualityTuningLevel',
        VMultiPass,
        VMultiPassHq
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
newtype Vp8QualityTuningLevel = Vp8QualityTuningLevel' Lude.Text
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

pattern VMultiPass :: Vp8QualityTuningLevel
pattern VMultiPass = Vp8QualityTuningLevel' "MULTI_PASS"

pattern VMultiPassHq :: Vp8QualityTuningLevel
pattern VMultiPassHq = Vp8QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  VMultiPass,
  VMultiPassHq,
  Vp8QualityTuningLevel'
  #-}
