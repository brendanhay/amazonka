{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
  ( UdpTimedMetadataId3Frame
      ( UdpTimedMetadataId3Frame',
        UdpTimedMetadataId3FrameNone,
        UdpTimedMetadataId3FramePriv,
        UdpTimedMetadataId3FrameTdrl,
        fromUdpTimedMetadataId3Frame
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Udp Timed Metadata Id3 Frame
newtype UdpTimedMetadataId3Frame = UdpTimedMetadataId3Frame'
  { fromUdpTimedMetadataId3Frame ::
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

pattern UdpTimedMetadataId3FrameNone :: UdpTimedMetadataId3Frame
pattern UdpTimedMetadataId3FrameNone = UdpTimedMetadataId3Frame' "NONE"

pattern UdpTimedMetadataId3FramePriv :: UdpTimedMetadataId3Frame
pattern UdpTimedMetadataId3FramePriv = UdpTimedMetadataId3Frame' "PRIV"

pattern UdpTimedMetadataId3FrameTdrl :: UdpTimedMetadataId3Frame
pattern UdpTimedMetadataId3FrameTdrl = UdpTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  UdpTimedMetadataId3FrameNone,
  UdpTimedMetadataId3FramePriv,
  UdpTimedMetadataId3FrameTdrl,
  UdpTimedMetadataId3Frame'
  #-}
