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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayRouteState = TransitGatewayRouteState'
  { fromTransitGatewayRouteState ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
