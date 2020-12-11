-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
  ( IFrameOnlyPlaylistType
      ( IFrameOnlyPlaylistType',
        IFOPTDisabled,
        IFOPTStandard
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to "standard", an I-Frame only playlist will be written out for each video output in the output group. This I-Frame only playlist will contain byte range offsets pointing to the I-frame(s) in each segment.
newtype IFrameOnlyPlaylistType = IFrameOnlyPlaylistType' Lude.Text
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

pattern IFOPTDisabled :: IFrameOnlyPlaylistType
pattern IFOPTDisabled = IFrameOnlyPlaylistType' "DISABLED"

pattern IFOPTStandard :: IFrameOnlyPlaylistType
pattern IFOPTStandard = IFrameOnlyPlaylistType' "STANDARD"

{-# COMPLETE
  IFOPTDisabled,
  IFOPTStandard,
  IFrameOnlyPlaylistType'
  #-}
