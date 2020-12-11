-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
  ( DvbSubDestinationShadowColor
      ( DvbSubDestinationShadowColor',
        DSDSCBlack,
        DSDSCNone,
        DSDSCWhite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dvb Sub Destination Shadow Color
newtype DvbSubDestinationShadowColor = DvbSubDestinationShadowColor' Lude.Text
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

pattern DSDSCBlack :: DvbSubDestinationShadowColor
pattern DSDSCBlack = DvbSubDestinationShadowColor' "BLACK"

pattern DSDSCNone :: DvbSubDestinationShadowColor
pattern DSDSCNone = DvbSubDestinationShadowColor' "NONE"

pattern DSDSCWhite :: DvbSubDestinationShadowColor
pattern DSDSCWhite = DvbSubDestinationShadowColor' "WHITE"

{-# COMPLETE
  DSDSCBlack,
  DSDSCNone,
  DSDSCWhite,
  DvbSubDestinationShadowColor'
  #-}
