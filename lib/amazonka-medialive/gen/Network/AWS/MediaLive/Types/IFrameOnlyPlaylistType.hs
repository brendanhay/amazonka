{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
  ( IFrameOnlyPlaylistType
    ( IFrameOnlyPlaylistType'
    , IFrameOnlyPlaylistTypeDisabled
    , IFrameOnlyPlaylistTypeStandard
    , fromIFrameOnlyPlaylistType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When set to "standard", an I-Frame only playlist will be written out for each video output in the output group. This I-Frame only playlist will contain byte range offsets pointing to the I-frame(s) in each segment.
newtype IFrameOnlyPlaylistType = IFrameOnlyPlaylistType'{fromIFrameOnlyPlaylistType
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern IFrameOnlyPlaylistTypeDisabled :: IFrameOnlyPlaylistType
pattern IFrameOnlyPlaylistTypeDisabled = IFrameOnlyPlaylistType' "DISABLED"

pattern IFrameOnlyPlaylistTypeStandard :: IFrameOnlyPlaylistType
pattern IFrameOnlyPlaylistTypeStandard = IFrameOnlyPlaylistType' "STANDARD"

{-# COMPLETE 
  IFrameOnlyPlaylistTypeDisabled,

  IFrameOnlyPlaylistTypeStandard,
  IFrameOnlyPlaylistType'
  #-}
