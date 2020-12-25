{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame
  ( HlsTimedMetadataId3Frame
      ( HlsTimedMetadataId3Frame',
        HlsTimedMetadataId3FrameNone,
        HlsTimedMetadataId3FramePriv,
        HlsTimedMetadataId3FrameTdrl,
        fromHlsTimedMetadataId3Frame
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Hls Timed Metadata Id3 Frame
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame'
  { fromHlsTimedMetadataId3Frame ::
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

pattern HlsTimedMetadataId3FrameNone :: HlsTimedMetadataId3Frame
pattern HlsTimedMetadataId3FrameNone = HlsTimedMetadataId3Frame' "NONE"

pattern HlsTimedMetadataId3FramePriv :: HlsTimedMetadataId3Frame
pattern HlsTimedMetadataId3FramePriv = HlsTimedMetadataId3Frame' "PRIV"

pattern HlsTimedMetadataId3FrameTdrl :: HlsTimedMetadataId3Frame
pattern HlsTimedMetadataId3FrameTdrl = HlsTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  HlsTimedMetadataId3FrameNone,
  HlsTimedMetadataId3FramePriv,
  HlsTimedMetadataId3FrameTdrl,
  HlsTimedMetadataId3Frame'
  #-}
