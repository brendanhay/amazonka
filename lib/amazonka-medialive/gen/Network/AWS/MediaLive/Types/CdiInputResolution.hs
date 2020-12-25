{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CdiInputResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputResolution
  ( CdiInputResolution
      ( CdiInputResolution',
        CdiInputResolutionSD,
        CdiInputResolutionHD,
        CdiInputResolutionFhd,
        CdiInputResolutionUhd,
        fromCdiInputResolution
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Maximum CDI input resolution; SD is 480i and 576i up to 30 frames-per-second (fps), HD is 720p up to 60 fps / 1080i up to 30 fps, FHD is 1080p up to 60 fps, UHD is 2160p up to 60 fps
newtype CdiInputResolution = CdiInputResolution'
  { fromCdiInputResolution ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CdiInputResolutionSD :: CdiInputResolution
pattern CdiInputResolutionSD = CdiInputResolution' "SD"

pattern CdiInputResolutionHD :: CdiInputResolution
pattern CdiInputResolutionHD = CdiInputResolution' "HD"

pattern CdiInputResolutionFhd :: CdiInputResolution
pattern CdiInputResolutionFhd = CdiInputResolution' "FHD"

pattern CdiInputResolutionUhd :: CdiInputResolution
pattern CdiInputResolutionUhd = CdiInputResolution' "UHD"

{-# COMPLETE
  CdiInputResolutionSD,
  CdiInputResolutionHD,
  CdiInputResolutionFhd,
  CdiInputResolutionUhd,
  CdiInputResolution'
  #-}
