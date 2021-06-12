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
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteState
  ( LocalGatewayRouteState
      ( ..,
        LocalGatewayRouteState_Active,
        LocalGatewayRouteState_Blackhole,
        LocalGatewayRouteState_Deleted,
        LocalGatewayRouteState_Deleting,
        LocalGatewayRouteState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype LocalGatewayRouteState = LocalGatewayRouteState'
  { fromLocalGatewayRouteState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern LocalGatewayRouteState_Active :: LocalGatewayRouteState
pattern LocalGatewayRouteState_Active = LocalGatewayRouteState' "active"

pattern LocalGatewayRouteState_Blackhole :: LocalGatewayRouteState
pattern LocalGatewayRouteState_Blackhole = LocalGatewayRouteState' "blackhole"

pattern LocalGatewayRouteState_Deleted :: LocalGatewayRouteState
pattern LocalGatewayRouteState_Deleted = LocalGatewayRouteState' "deleted"

pattern LocalGatewayRouteState_Deleting :: LocalGatewayRouteState
pattern LocalGatewayRouteState_Deleting = LocalGatewayRouteState' "deleting"

pattern LocalGatewayRouteState_Pending :: LocalGatewayRouteState
pattern LocalGatewayRouteState_Pending = LocalGatewayRouteState' "pending"

{-# COMPLETE
  LocalGatewayRouteState_Active,
  LocalGatewayRouteState_Blackhole,
  LocalGatewayRouteState_Deleted,
  LocalGatewayRouteState_Deleting,
  LocalGatewayRouteState_Pending,
  LocalGatewayRouteState'
  #-}
