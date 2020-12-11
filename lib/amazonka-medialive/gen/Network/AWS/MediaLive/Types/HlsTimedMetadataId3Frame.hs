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
        HTMIFNone,
        HTMIFPriv,
        HTMIFTdrl
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Hls Timed Metadata Id3 Frame
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame' Lude.Text
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

pattern HTMIFNone :: HlsTimedMetadataId3Frame
pattern HTMIFNone = HlsTimedMetadataId3Frame' "NONE"

pattern HTMIFPriv :: HlsTimedMetadataId3Frame
pattern HTMIFPriv = HlsTimedMetadataId3Frame' "PRIV"

pattern HTMIFTdrl :: HlsTimedMetadataId3Frame
pattern HTMIFTdrl = HlsTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  HTMIFNone,
  HTMIFPriv,
  HTMIFTdrl,
  HlsTimedMetadataId3Frame'
  #-}
