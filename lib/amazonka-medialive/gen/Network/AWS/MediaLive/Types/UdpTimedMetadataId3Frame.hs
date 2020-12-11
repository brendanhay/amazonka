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
        UTMIFNone,
        UTMIFPriv,
        UTMIFTdrl
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Udp Timed Metadata Id3 Frame
newtype UdpTimedMetadataId3Frame = UdpTimedMetadataId3Frame' Lude.Text
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

pattern UTMIFNone :: UdpTimedMetadataId3Frame
pattern UTMIFNone = UdpTimedMetadataId3Frame' "NONE"

pattern UTMIFPriv :: UdpTimedMetadataId3Frame
pattern UTMIFPriv = UdpTimedMetadataId3Frame' "PRIV"

pattern UTMIFTdrl :: UdpTimedMetadataId3Frame
pattern UTMIFTdrl = UdpTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  UTMIFNone,
  UTMIFPriv,
  UTMIFTdrl,
  UdpTimedMetadataId3Frame'
  #-}
