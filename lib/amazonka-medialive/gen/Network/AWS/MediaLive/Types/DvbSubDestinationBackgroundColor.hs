-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor
  ( DvbSubDestinationBackgroundColor
      ( DvbSubDestinationBackgroundColor',
        DSDBCBlack,
        DSDBCNone,
        DSDBCWhite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dvb Sub Destination Background Color
newtype DvbSubDestinationBackgroundColor = DvbSubDestinationBackgroundColor' Lude.Text
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

pattern DSDBCBlack :: DvbSubDestinationBackgroundColor
pattern DSDBCBlack = DvbSubDestinationBackgroundColor' "BLACK"

pattern DSDBCNone :: DvbSubDestinationBackgroundColor
pattern DSDBCNone = DvbSubDestinationBackgroundColor' "NONE"

pattern DSDBCWhite :: DvbSubDestinationBackgroundColor
pattern DSDBCWhite = DvbSubDestinationBackgroundColor' "WHITE"

{-# COMPLETE
  DSDBCBlack,
  DSDBCNone,
  DSDBCWhite,
  DvbSubDestinationBackgroundColor'
  #-}
