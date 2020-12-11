-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.AdMarkers
  ( AdMarkers
      ( AdMarkers',
        AMDaterange,
        AMNone,
        AMPassthrough,
        AMSCTE35Enhanced
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AdMarkers = AdMarkers' Lude.Text
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

pattern AMDaterange :: AdMarkers
pattern AMDaterange = AdMarkers' "DATERANGE"

pattern AMNone :: AdMarkers
pattern AMNone = AdMarkers' "NONE"

pattern AMPassthrough :: AdMarkers
pattern AMPassthrough = AdMarkers' "PASSTHROUGH"

pattern AMSCTE35Enhanced :: AdMarkers
pattern AMSCTE35Enhanced = AdMarkers' "SCTE35_ENHANCED"

{-# COMPLETE
  AMDaterange,
  AMNone,
  AMPassthrough,
  AMSCTE35Enhanced,
  AdMarkers'
  #-}
