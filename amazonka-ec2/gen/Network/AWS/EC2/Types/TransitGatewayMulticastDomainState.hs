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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
  ( TransitGatewayMulticastDomainState
      ( ..,
        TransitGatewayMulticastDomainState_Available,
        TransitGatewayMulticastDomainState_Deleted,
        TransitGatewayMulticastDomainState_Deleting,
        TransitGatewayMulticastDomainState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype TransitGatewayMulticastDomainState = TransitGatewayMulticastDomainState'
  { fromTransitGatewayMulticastDomainState ::
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

pattern TransitGatewayMulticastDomainState_Available :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Available = TransitGatewayMulticastDomainState' "available"

pattern TransitGatewayMulticastDomainState_Deleted :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Deleted = TransitGatewayMulticastDomainState' "deleted"

pattern TransitGatewayMulticastDomainState_Deleting :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Deleting = TransitGatewayMulticastDomainState' "deleting"

pattern TransitGatewayMulticastDomainState_Pending :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Pending = TransitGatewayMulticastDomainState' "pending"

{-# COMPLETE
  TransitGatewayMulticastDomainState_Available,
  TransitGatewayMulticastDomainState_Deleted,
  TransitGatewayMulticastDomainState_Deleting,
  TransitGatewayMulticastDomainState_Pending,
  TransitGatewayMulticastDomainState'
  #-}
