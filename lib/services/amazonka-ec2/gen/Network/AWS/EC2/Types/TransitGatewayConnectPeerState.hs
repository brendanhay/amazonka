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
-- Module      : Amazonka.EC2.Types.TransitGatewayConnectPeerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayConnectPeerState
  ( TransitGatewayConnectPeerState
      ( ..,
        TransitGatewayConnectPeerState_Available,
        TransitGatewayConnectPeerState_Deleted,
        TransitGatewayConnectPeerState_Deleting,
        TransitGatewayConnectPeerState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayConnectPeerState = TransitGatewayConnectPeerState'
  { fromTransitGatewayConnectPeerState ::
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
