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
-- Module      : Amazonka.NetworkManager.Types.RouteTableType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteTableType
  ( RouteTableType
      ( ..,
        RouteTableType_CORE_NETWORK_SEGMENT,
        RouteTableType_TRANSIT_GATEWAY_ROUTE_TABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RouteTableType = RouteTableType'
  { fromRouteTableType ::
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

pattern RouteTableType_CORE_NETWORK_SEGMENT :: RouteTableType
pattern RouteTableType_CORE_NETWORK_SEGMENT = RouteTableType' "CORE_NETWORK_SEGMENT"

pattern RouteTableType_TRANSIT_GATEWAY_ROUTE_TABLE :: RouteTableType
pattern RouteTableType_TRANSIT_GATEWAY_ROUTE_TABLE = RouteTableType' "TRANSIT_GATEWAY_ROUTE_TABLE"

{-# COMPLETE
  RouteTableType_CORE_NETWORK_SEGMENT,
  RouteTableType_TRANSIT_GATEWAY_ROUTE_TABLE,
  RouteTableType'
  #-}
