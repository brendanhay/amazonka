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
        CIRFhd,
        CIRHD,
        CIRSD,
        CIRUhd
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Maximum CDI input resolution; SD is 480i and 576i up to 30 frames-per-second (fps), HD is 720p up to 60 fps / 1080i up to 30 fps, FHD is 1080p up to 60 fps, UHD is 2160p up to 60 fps
newtype CdiInputResolution = CdiInputResolution' Lude.Text
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

pattern CIRFhd :: CdiInputResolution
pattern CIRFhd = CdiInputResolution' "FHD"

pattern CIRHD :: CdiInputResolution
pattern CIRHD = CdiInputResolution' "HD"

pattern CIRSD :: CdiInputResolution
pattern CIRSD = CdiInputResolution' "SD"

pattern CIRUhd :: CdiInputResolution
pattern CIRUhd = CdiInputResolution' "UHD"

{-# COMPLETE
  CIRFhd,
  CIRHD,
  CIRSD,
  CIRUhd,
  CdiInputResolution'
  #-}
