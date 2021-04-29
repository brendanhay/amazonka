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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype TransitGatewayMulticastDomainState = TransitGatewayMulticastDomainState'
  { fromTransitGatewayMulticastDomainState ::
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
