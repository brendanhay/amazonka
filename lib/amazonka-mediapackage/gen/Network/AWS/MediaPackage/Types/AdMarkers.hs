{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.AdMarkers
  ( AdMarkers
    ( AdMarkers'
    , AdMarkersNone
    , AdMarkersSCTE35Enhanced
    , AdMarkersPassthrough
    , AdMarkersDaterange
    , fromAdMarkers
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AdMarkers = AdMarkers'{fromAdMarkers :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern AdMarkersNone :: AdMarkers
pattern AdMarkersNone = AdMarkers' "NONE"

pattern AdMarkersSCTE35Enhanced :: AdMarkers
pattern AdMarkersSCTE35Enhanced = AdMarkers' "SCTE35_ENHANCED"

pattern AdMarkersPassthrough :: AdMarkers
pattern AdMarkersPassthrough = AdMarkers' "PASSTHROUGH"

pattern AdMarkersDaterange :: AdMarkers
pattern AdMarkersDaterange = AdMarkers' "DATERANGE"

{-# COMPLETE 
  AdMarkersNone,

  AdMarkersSCTE35Enhanced,

  AdMarkersPassthrough,

  AdMarkersDaterange,
  AdMarkers'
  #-}
