{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTableState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTableState
  ( TransitGatewayRouteTableState
      ( ..,
        TransitGatewayRouteTableState_Available,
        TransitGatewayRouteTableState_Deleted,
        TransitGatewayRouteTableState_Deleting,
        TransitGatewayRouteTableState_Pending
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype TransitGatewayRouteTableState = TransitGatewayRouteTableState'
  { fromTransitGatewayRouteTableState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern TransitGatewayRouteTableState_Available :: TransitGatewayRouteTableState
pattern TransitGatewayRouteTableState_Available = TransitGatewayRouteTableState' "available"

pattern TransitGatewayRouteTableState_Deleted :: TransitGatewayRouteTableState
pattern TransitGatewayRouteTableState_Deleted = TransitGatewayRouteTableState' "deleted"

pattern TransitGatewayRouteTableState_Deleting :: TransitGatewayRouteTableState
pattern TransitGatewayRouteTableState_Deleting = TransitGatewayRouteTableState' "deleting"

pattern TransitGatewayRouteTableState_Pending :: TransitGatewayRouteTableState
pattern TransitGatewayRouteTableState_Pending = TransitGatewayRouteTableState' "pending"

{-# COMPLETE
  TransitGatewayRouteTableState_Available,
  TransitGatewayRouteTableState_Deleted,
  TransitGatewayRouteTableState_Deleting,
  TransitGatewayRouteTableState_Pending,
  TransitGatewayRouteTableState'
  #-}
