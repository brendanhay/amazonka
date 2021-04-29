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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnectPeerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnectPeerState
  ( TransitGatewayConnectPeerState
      ( ..,
        TransitGatewayConnectPeerState_Available,
        TransitGatewayConnectPeerState_Deleted,
        TransitGatewayConnectPeerState_Deleting,
        TransitGatewayConnectPeerState_Pending
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype TransitGatewayConnectPeerState = TransitGatewayConnectPeerState'
  { fromTransitGatewayConnectPeerState ::
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

pattern TransitGatewayConnectPeerState_Available :: TransitGatewayConnectPeerState
pattern TransitGatewayConnectPeerState_Available = TransitGatewayConnectPeerState' "available"

pattern TransitGatewayConnectPeerState_Deleted :: TransitGatewayConnectPeerState
pattern TransitGatewayConnectPeerState_Deleted = TransitGatewayConnectPeerState' "deleted"

pattern TransitGatewayConnectPeerState_Deleting :: TransitGatewayConnectPeerState
pattern TransitGatewayConnectPeerState_Deleting = TransitGatewayConnectPeerState' "deleting"

pattern TransitGatewayConnectPeerState_Pending :: TransitGatewayConnectPeerState
pattern TransitGatewayConnectPeerState_Pending = TransitGatewayConnectPeerState' "pending"

{-# COMPLETE
  TransitGatewayConnectPeerState_Available,
  TransitGatewayConnectPeerState_Deleted,
  TransitGatewayConnectPeerState_Deleting,
  TransitGatewayConnectPeerState_Pending,
  TransitGatewayConnectPeerState'
  #-}
