{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
  ( NoiseFilterPostTemporalSharpening
      ( ..,
        NoiseFilterPostTemporalSharpening_AUTO,
        NoiseFilterPostTemporalSharpening_DISABLED,
        NoiseFilterPostTemporalSharpening_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal
-- (TEMPORAL), you can use this setting to apply sharpening. The default
-- behavior, Auto (AUTO), allows the transcoder to determine whether to
-- apply filtering, depending on input type and quality. When you set Noise
-- reducer to Temporal, your output bandwidth is reduced. When Post
-- temporal sharpening is also enabled, that bandwidth reduction is
-- smaller.
newtype NoiseFilterPostTemporalSharpening = NoiseFilterPostTemporalSharpening'
  { fromNoiseFilterPostTemporalSharpening ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern NoiseFilterPostTemporalSharpening_AUTO :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpening_AUTO = NoiseFilterPostTemporalSharpening' "AUTO"

pattern NoiseFilterPostTemporalSharpening_DISABLED :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpening_DISABLED = NoiseFilterPostTemporalSharpening' "DISABLED"

pattern NoiseFilterPostTemporalSharpening_ENABLED :: NoiseFilterPostTemporalSharpening
pattern NoiseFilterPostTemporalSharpening_ENABLED = NoiseFilterPostTemporalSharpening' "ENABLED"

{-# COMPLETE
  NoiseFilterPostTemporalSharpening_AUTO,
  NoiseFilterPostTemporalSharpening_DISABLED,
  NoiseFilterPostTemporalSharpening_ENABLED,
  NoiseFilterPostTemporalSharpening'
  #-}
