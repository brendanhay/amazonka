{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
  ( NoiseFilterPostTemporalSharpening
      ( NoiseFilterPostTemporalSharpening',
        NFPTSDisabled,
        NFPTSEnabled,
        NFPTSAuto
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
newtype NoiseFilterPostTemporalSharpening = NoiseFilterPostTemporalSharpening' Lude.Text
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

pattern NFPTSDisabled :: NoiseFilterPostTemporalSharpening
pattern NFPTSDisabled = NoiseFilterPostTemporalSharpening' "DISABLED"

pattern NFPTSEnabled :: NoiseFilterPostTemporalSharpening
pattern NFPTSEnabled = NoiseFilterPostTemporalSharpening' "ENABLED"

pattern NFPTSAuto :: NoiseFilterPostTemporalSharpening
pattern NFPTSAuto = NoiseFilterPostTemporalSharpening' "AUTO"

{-# COMPLETE
  NFPTSDisabled,
  NFPTSEnabled,
  NFPTSAuto,
  NoiseFilterPostTemporalSharpening'
  #-}
