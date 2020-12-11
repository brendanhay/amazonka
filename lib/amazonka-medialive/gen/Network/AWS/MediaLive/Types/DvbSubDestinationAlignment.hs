-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationAlignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationAlignment
  ( DvbSubDestinationAlignment
      ( DvbSubDestinationAlignment',
        Centered,
        Left,
        Smart
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dvb Sub Destination Alignment
newtype DvbSubDestinationAlignment = DvbSubDestinationAlignment' Lude.Text
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

pattern Centered :: DvbSubDestinationAlignment
pattern Centered = DvbSubDestinationAlignment' "CENTERED"

pattern Left :: DvbSubDestinationAlignment
pattern Left = DvbSubDestinationAlignment' "LEFT"

pattern Smart :: DvbSubDestinationAlignment
pattern Smart = DvbSubDestinationAlignment' "SMART"

{-# COMPLETE
  Centered,
  Left,
  Smart,
  DvbSubDestinationAlignment'
  #-}
