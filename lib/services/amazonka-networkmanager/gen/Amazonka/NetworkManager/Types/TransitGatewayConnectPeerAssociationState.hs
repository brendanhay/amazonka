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
-- Module      : Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociationState
  ( TransitGatewayConnectPeerAssociationState
      ( ..,
        TransitGatewayConnectPeerAssociationState_AVAILABLE,
        TransitGatewayConnectPeerAssociationState_DELETED,
        TransitGatewayConnectPeerAssociationState_DELETING,
        TransitGatewayConnectPeerAssociationState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayConnectPeerAssociationState = TransitGatewayConnectPeerAssociationState'
  { fromTransitGatewayConnectPeerAssociationState ::
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

pattern TransitGatewayConnectPeerAssociationState_AVAILABLE :: TransitGatewayConnectPeerAssociationState
pattern TransitGatewayConnectPeerAssociationState_AVAILABLE = TransitGatewayConnectPeerAssociationState' "AVAILABLE"

pattern TransitGatewayConnectPeerAssociationState_DELETED :: TransitGatewayConnectPeerAssociationState
pattern TransitGatewayConnectPeerAssociationState_DELETED = TransitGatewayConnectPeerAssociationState' "DELETED"

pattern TransitGatewayConnectPeerAssociationState_DELETING :: TransitGatewayConnectPeerAssociationState
pattern TransitGatewayConnectPeerAssociationState_DELETING = TransitGatewayConnectPeerAssociationState' "DELETING"

pattern TransitGatewayConnectPeerAssociationState_PENDING :: TransitGatewayConnectPeerAssociationState
pattern TransitGatewayConnectPeerAssociationState_PENDING = TransitGatewayConnectPeerAssociationState' "PENDING"

{-# COMPLETE
  TransitGatewayConnectPeerAssociationState_AVAILABLE,
  TransitGatewayConnectPeerAssociationState_DELETED,
  TransitGatewayConnectPeerAssociationState_DELETING,
  TransitGatewayConnectPeerAssociationState_PENDING,
  TransitGatewayConnectPeerAssociationState'
  #-}
