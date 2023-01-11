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
-- Module      : Amazonka.MediaConvert.Types.HlsImageBasedTrickPlay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsImageBasedTrickPlay
  ( HlsImageBasedTrickPlay
      ( ..,
        HlsImageBasedTrickPlay_ADVANCED,
        HlsImageBasedTrickPlay_NONE,
        HlsImageBasedTrickPlay_THUMBNAIL,
        HlsImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME
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
-- full-resolution images of single frames. MediaConvert creates a child
-- manifest for each set of images that you generate and adds corresponding
-- entries to the parent manifest. A common application for these images is
-- Roku trick mode. The thumbnails and full-frame images that MediaConvert
-- creates with this feature are compatible with this Roku specification:
-- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
newtype HlsImageBasedTrickPlay = HlsImageBasedTrickPlay'
  { fromHlsImageBasedTrickPlay ::
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

pattern HlsImageBasedTrickPlay_ADVANCED :: HlsImageBasedTrickPlay
pattern HlsImageBasedTrickPlay_ADVANCED = HlsImageBasedTrickPlay' "ADVANCED"

pattern HlsImageBasedTrickPlay_NONE :: HlsImageBasedTrickPlay
pattern HlsImageBasedTrickPlay_NONE = HlsImageBasedTrickPlay' "NONE"

pattern HlsImageBasedTrickPlay_THUMBNAIL :: HlsImageBasedTrickPlay
pattern HlsImageBasedTrickPlay_THUMBNAIL = HlsImageBasedTrickPlay' "THUMBNAIL"

pattern HlsImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME :: HlsImageBasedTrickPlay
pattern HlsImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME = HlsImageBasedTrickPlay' "THUMBNAIL_AND_FULLFRAME"

{-# COMPLETE
  HlsImageBasedTrickPlay_ADVANCED,
  HlsImageBasedTrickPlay_NONE,
  HlsImageBasedTrickPlay_THUMBNAIL,
  HlsImageBasedTrickPlay_THUMBNAIL_AND_FULLFRAME,
  HlsImageBasedTrickPlay'
  #-}
