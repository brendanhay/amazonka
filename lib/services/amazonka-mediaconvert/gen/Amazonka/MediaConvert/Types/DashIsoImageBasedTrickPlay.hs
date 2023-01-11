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
-- Module      : Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlay
  ( DashIsoImageBasedTrickPlay
      ( ..,
        DashIsoImageBasedTrickPlay_ADVANCED,
        DashIsoImageBasedTrickPlay_NONE,
        DashIsoImageBasedTrickPlay_THUMBNAIL,
        DashIsoImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether MediaConvert generates images for trick play. Keep the
-- default value, None (NONE), to not generate any images. Choose Thumbnail
-- (THUMBNAIL) to generate tiled thumbnails. Choose Thumbnail and full
-- frame (THUMBNAIL_AND_FULLFRAME) to generate tiled thumbnails and
-- full-resolution images of single frames. MediaConvert adds an entry in
-- the .mpd manifest for each set of images that you generate. A common
-- application for these images is Roku trick mode. The thumbnails and
-- full-frame images that MediaConvert creates with this feature are
-- compatible with this Roku specification:
-- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
newtype DashIsoImageBasedTrickPlay = DashIsoImageBasedTrickPlay'
  { fromDashIsoImageBasedTrickPlay ::
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

pattern DashIsoImageBasedTrickPlay_ADVANCED :: DashIsoImageBasedTrickPlay
pattern DashIsoImageBasedTrickPlay_ADVANCED = DashIsoImageBasedTrickPlay' "ADVANCED"

pattern DashIsoImageBasedTrickPlay_NONE :: DashIsoImageBasedTrickPlay
pattern DashIsoImageBasedTrickPlay_NONE = DashIsoImageBasedTrickPlay' "NONE"

pattern DashIsoImageBasedTrickPlay_THUMBNAIL :: DashIsoImageBasedTrickPlay
pattern DashIsoImageBasedTrickPlay_THUMBNAIL = DashIsoImageBasedTrickPlay' "THUMBNAIL"

pattern DashIsoImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME :: DashIsoImageBasedTrickPlay
pattern DashIsoImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME = DashIsoImageBasedTrickPlay' "THUMBNAIL_AND_FULLFRAME"

{-# COMPLETE
  DashIsoImageBasedTrickPlay_ADVANCED,
  DashIsoImageBasedTrickPlay_NONE,
  DashIsoImageBasedTrickPlay_THUMBNAIL,
  DashIsoImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME,
  DashIsoImageBasedTrickPlay'
  #-}
