{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        AMNone,
        AMSCTE35Enhanced,
        AMPassthrough,
        AMDaterange
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

pattern AMNone :: AdMarkers
pattern AMNone = AdMarkers' "NONE"

pattern AMSCTE35Enhanced :: AdMarkers
pattern AMSCTE35Enhanced = AdMarkers' "SCTE35_ENHANCED"

pattern AMPassthrough :: AdMarkers
pattern AMPassthrough = AdMarkers' "PASSTHROUGH"

pattern AMDaterange :: AdMarkers
pattern AMDaterange = AdMarkers' "DATERANGE"

{-# COMPLETE
  AMNone,
  AMSCTE35Enhanced,
  AMPassthrough,
  AMDaterange,
  AdMarkers'
  #-}
