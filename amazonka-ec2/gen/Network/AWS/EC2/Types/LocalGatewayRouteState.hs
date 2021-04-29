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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype LocalGatewayRouteState = LocalGatewayRouteState'
  { fromLocalGatewayRouteState ::
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
