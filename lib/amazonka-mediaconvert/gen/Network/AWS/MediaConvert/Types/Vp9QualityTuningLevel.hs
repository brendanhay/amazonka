-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
  ( Vp9QualityTuningLevel
      ( Vp9QualityTuningLevel',
        VQTLMultiPass,
        VQTLMultiPassHq
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
newtype Vp9QualityTuningLevel = Vp9QualityTuningLevel' Lude.Text
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

pattern VQTLMultiPass :: Vp9QualityTuningLevel
pattern VQTLMultiPass = Vp9QualityTuningLevel' "MULTI_PASS"

pattern VQTLMultiPassHq :: Vp9QualityTuningLevel
pattern VQTLMultiPassHq = Vp9QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  VQTLMultiPass,
  VQTLMultiPassHq,
  Vp9QualityTuningLevel'
  #-}
