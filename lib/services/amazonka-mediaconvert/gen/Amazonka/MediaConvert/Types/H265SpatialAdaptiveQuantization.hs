{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.H265SpatialAdaptiveQuantization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265SpatialAdaptiveQuantization
  ( H265SpatialAdaptiveQuantization
      ( ..,
        H265SpatialAdaptiveQuantization_DISABLED,
        H265SpatialAdaptiveQuantization_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on spatial variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas that can
-- sustain more distortion with no noticeable visual degradation and uses
-- more bits on areas where any small distortion will be noticeable. For
-- example, complex textured blocks are encoded with fewer bits and smooth
-- textured blocks are encoded with more bits. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen with a lot of complex texture, you might choose to
-- disable this feature. Related setting: When you enable spatial adaptive
-- quantization, set the value for Adaptive quantization
-- (adaptiveQuantization) depending on your content. For homogeneous
-- content, such as cartoons and video games, set it to Low. For content
-- with a wider variety of textures, set it to High or Higher.
newtype H265SpatialAdaptiveQuantization = H265SpatialAdaptiveQuantization'
  { fromH265SpatialAdaptiveQuantization ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern H265SpatialAdaptiveQuantization_DISABLED :: H265SpatialAdaptiveQuantization
pattern H265SpatialAdaptiveQuantization_DISABLED = H265SpatialAdaptiveQuantization' "DISABLED"

pattern H265SpatialAdaptiveQuantization_ENABLED :: H265SpatialAdaptiveQuantization
pattern H265SpatialAdaptiveQuantization_ENABLED = H265SpatialAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H265SpatialAdaptiveQuantization_DISABLED,
  H265SpatialAdaptiveQuantization_ENABLED,
  H265SpatialAdaptiveQuantization'
  #-}
