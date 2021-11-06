{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteTableState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteTableState
  ( TransitGatewayRouteTableState
      ( ..,
        TransitGatewayRouteTableState_Available,
        TransitGatewayRouteTableState_Deleted,
        TransitGatewayRouteTableState_Deleting,
        TransitGatewayRouteTableState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayRouteTableState = TransitGatewayRouteTableState'
  { fromTransitGatewayRouteTableState ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
