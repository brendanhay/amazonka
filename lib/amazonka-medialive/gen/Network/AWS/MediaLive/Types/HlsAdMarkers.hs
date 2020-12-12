{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsAdMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsAdMarkers
  ( HlsAdMarkers
      ( HlsAdMarkers',
        Adobe,
        Elemental,
        ElementalSCTE35
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Hls Ad Markers
newtype HlsAdMarkers = HlsAdMarkers' Lude.Text
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

pattern Adobe :: HlsAdMarkers
pattern Adobe = HlsAdMarkers' "ADOBE"

pattern Elemental :: HlsAdMarkers
pattern Elemental = HlsAdMarkers' "ELEMENTAL"

pattern ElementalSCTE35 :: HlsAdMarkers
pattern ElementalSCTE35 = HlsAdMarkers' "ELEMENTAL_SCTE35"

{-# COMPLETE
  Adobe,
  Elemental,
  ElementalSCTE35,
  HlsAdMarkers'
  #-}
