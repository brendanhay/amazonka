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
-- Module      : Network.AWS.MediaConvert.Types.ColorSpace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpace
  ( ColorSpace
      ( ..,
        ColorSpace_FOLLOW,
        ColorSpace_HDR10,
        ColorSpace_HLG_2020,
        ColorSpace_REC_601,
        ColorSpace_REC_709
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | If your input video has accurate color space metadata, or if you don\'t
-- know about color space, leave this set to the default value Follow
-- (FOLLOW). The service will automatically detect your input color space.
-- If your input video has metadata indicating the wrong color space,
-- specify the accurate color space here. If your input video is HDR 10 and
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
-- present in your video stream, or if that metadata is present but not
-- accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct
-- values in the input HDR 10 metadata (Hdr10Metadata) settings. For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
newtype ColorSpace = ColorSpace'
  { fromColorSpace ::
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

pattern ColorSpace_FOLLOW :: ColorSpace
pattern ColorSpace_FOLLOW = ColorSpace' "FOLLOW"

pattern ColorSpace_HDR10 :: ColorSpace
pattern ColorSpace_HDR10 = ColorSpace' "HDR10"

pattern ColorSpace_HLG_2020 :: ColorSpace
pattern ColorSpace_HLG_2020 = ColorSpace' "HLG_2020"

pattern ColorSpace_REC_601 :: ColorSpace
pattern ColorSpace_REC_601 = ColorSpace' "REC_601"

pattern ColorSpace_REC_709 :: ColorSpace
pattern ColorSpace_REC_709 = ColorSpace' "REC_709"

{-# COMPLETE
  ColorSpace_FOLLOW,
  ColorSpace_HDR10,
  ColorSpace_HLG_2020,
  ColorSpace_REC_601,
  ColorSpace_REC_709,
  ColorSpace'
  #-}
