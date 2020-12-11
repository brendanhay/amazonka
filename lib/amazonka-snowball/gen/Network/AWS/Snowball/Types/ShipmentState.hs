-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShipmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShipmentState
  ( ShipmentState
      ( ShipmentState',
        Received,
        Returned
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ShipmentState = ShipmentState' Lude.Text
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

pattern Received :: ShipmentState
pattern Received = ShipmentState' "RECEIVED"

pattern Returned :: ShipmentState
pattern Returned = ShipmentState' "RETURNED"

{-# COMPLETE
  Received,
  Returned,
  ShipmentState'
  #-}
