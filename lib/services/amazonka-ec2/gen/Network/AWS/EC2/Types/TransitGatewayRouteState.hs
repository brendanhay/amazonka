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
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteState
  ( TransitGatewayRouteState
      ( ..,
        TransitGatewayRouteState_Active,
        TransitGatewayRouteState_Blackhole,
        TransitGatewayRouteState_Deleted,
        TransitGatewayRouteState_Deleting,
        TransitGatewayRouteState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype TransitGatewayRouteState = TransitGatewayRouteState'
  { fromTransitGatewayRouteState ::
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

pattern TransitGatewayRouteState_Active :: TransitGatewayRouteState
pattern TransitGatewayRouteState_Active = TransitGatewayRouteState' "active"

pattern TransitGatewayRouteState_Blackhole :: TransitGatewayRouteState
pattern TransitGatewayRouteState_Blackhole = TransitGatewayRouteState' "blackhole"

pattern TransitGatewayRouteState_Deleted :: TransitGatewayRouteState
pattern TransitGatewayRouteState_Deleted = TransitGatewayRouteState' "deleted"

pattern TransitGatewayRouteState_Deleting :: TransitGatewayRouteState
pattern TransitGatewayRouteState_Deleting = TransitGatewayRouteState' "deleting"

pattern TransitGatewayRouteState_Pending :: TransitGatewayRouteState
pattern TransitGatewayRouteState_Pending = TransitGatewayRouteState' "pending"

{-# COMPLETE
  TransitGatewayRouteState_Active,
  TransitGatewayRouteState_Blackhole,
  TransitGatewayRouteState_Deleted,
  TransitGatewayRouteState_Deleting,
  TransitGatewayRouteState_Pending,
  TransitGatewayRouteState'
  #-}
