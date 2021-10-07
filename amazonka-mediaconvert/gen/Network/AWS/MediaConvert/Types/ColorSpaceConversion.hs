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
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceConversion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpaceConversion
  ( ColorSpaceConversion
      ( ..,
        ColorSpaceConversion_FORCE_601,
        ColorSpaceConversion_FORCE_709,
        ColorSpaceConversion_FORCE_HDR10,
        ColorSpaceConversion_FORCE_HLG_2020,
        ColorSpaceConversion_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the color space you want for this output. The service supports
-- conversion between HDR formats, between SDR formats, from SDR to HDR,
-- and from HDR to SDR. SDR to HDR conversion doesn\'t upgrade the dynamic
-- range. The converted video has an HDR format, but visually appears the
-- same as an unconverted output. HDR to SDR conversion uses Elemental tone
-- mapping technology to approximate the outcome of manually regrading from
-- HDR to SDR.
newtype ColorSpaceConversion = ColorSpaceConversion'
  { fromColorSpaceConversion ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ColorSpaceConversion_FORCE_601 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_601 = ColorSpaceConversion' "FORCE_601"

pattern ColorSpaceConversion_FORCE_709 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_709 = ColorSpaceConversion' "FORCE_709"

pattern ColorSpaceConversion_FORCE_HDR10 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_HDR10 = ColorSpaceConversion' "FORCE_HDR10"

pattern ColorSpaceConversion_FORCE_HLG_2020 :: ColorSpaceConversion
pattern ColorSpaceConversion_FORCE_HLG_2020 = ColorSpaceConversion' "FORCE_HLG_2020"

pattern ColorSpaceConversion_NONE :: ColorSpaceConversion
pattern ColorSpaceConversion_NONE = ColorSpaceConversion' "NONE"

{-# COMPLETE
  ColorSpaceConversion_FORCE_601,
  ColorSpaceConversion_FORCE_709,
  ColorSpaceConversion_FORCE_HDR10,
  ColorSpaceConversion_FORCE_HLG_2020,
  ColorSpaceConversion_NONE,
  ColorSpaceConversion'
  #-}
