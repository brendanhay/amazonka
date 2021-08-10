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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
  ( TransitGatewayMulitcastDomainAssociationState
      ( ..,
        TransitGatewayMulitcastDomainAssociationState_Associated,
        TransitGatewayMulitcastDomainAssociationState_Associating,
        TransitGatewayMulitcastDomainAssociationState_Disassociated,
        TransitGatewayMulitcastDomainAssociationState_Disassociating,
        TransitGatewayMulitcastDomainAssociationState_Failed,
        TransitGatewayMulitcastDomainAssociationState_PendingAcceptance,
        TransitGatewayMulitcastDomainAssociationState_Rejected
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype TransitGatewayMulitcastDomainAssociationState = TransitGatewayMulitcastDomainAssociationState'
  { fromTransitGatewayMulitcastDomainAssociationState ::
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

pattern TransitGatewayMulitcastDomainAssociationState_Associated :: TransitGatewayMulitcastDomainAssociationState
pattern TransitGatewayMulitcastDomainAssociationState_Associated = TransitGatewayMulitcastDomainAssociationState' "associated"

pattern TransitGatewayMulitcastDomainAssociationState_Associating :: TransitGatewayMulitcastDomainAssociationState
pattern TransitGatewayMulitcastDomainAssociationState_Associating = TransitGatewayMulitcastDomainAssociationState' "associating"

pattern TransitGatewayMulitcastDomainAssociationState_Disassociated :: TransitGatewayMulitcastDomainAssociationState
pattern TransitGatewayMulitcastDomainAssociationState_Disassociated = TransitGatewayMulitcastDomainAssociationState' "disassociated"

pattern TransitGatewayMulitcastDomainAssociationState_Disassociating :: TransitGatewayMulitcastDomainAssociationState
pattern TransitGatewayMulitcastDomainAssociationState_Disassociating = TransitGatewayMulitcastDomainAssociationState' "disassociating"

pattern TransitGatewayMulitcastDomainAssociationState_Failed :: TransitGatewayMulitcastDomainAssociationState
pattern TransitGatewayMulitcastDomainAssociationState_Failed = TransitGatewayMulitcastDomainAssociationState' "failed"

pattern TransitGatewayMulitcastDomainAssociationState_PendingAcceptance :: TransitGatewayMulitcastDomainAssociationState
pattern TransitGatewayMulitcastDomainAssociationState_PendingAcceptance = TransitGatewayMulitcastDomainAssociationState' "pendingAcceptance"

pattern TransitGatewayMulitcastDomainAssociationState_Rejected :: TransitGatewayMulitcastDomainAssociationState
pattern TransitGatewayMulitcastDomainAssociationState_Rejected = TransitGatewayMulitcastDomainAssociationState' "rejected"

{-# COMPLETE
  TransitGatewayMulitcastDomainAssociationState_Associated,
  TransitGatewayMulitcastDomainAssociationState_Associating,
  TransitGatewayMulitcastDomainAssociationState_Disassociated,
  TransitGatewayMulitcastDomainAssociationState_Disassociating,
  TransitGatewayMulitcastDomainAssociationState_Failed,
  TransitGatewayMulitcastDomainAssociationState_PendingAcceptance,
  TransitGatewayMulitcastDomainAssociationState_Rejected,
  TransitGatewayMulitcastDomainAssociationState'
  #-}
