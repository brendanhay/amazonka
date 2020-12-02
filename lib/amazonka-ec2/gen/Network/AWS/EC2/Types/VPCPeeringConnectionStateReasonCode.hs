{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCPeeringConnectionStateReasonCode
  = VPCSRCActive
  | VPCSRCDeleted
  | VPCSRCDeleting
  | VPCSRCExpired
  | VPCSRCFailed
  | VPCSRCInitiatingRequest
  | VPCSRCPendingAcceptance
  | VPCSRCProvisioning
  | VPCSRCRejected
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText VPCPeeringConnectionStateReasonCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure VPCSRCActive
      "deleted" -> pure VPCSRCDeleted
      "deleting" -> pure VPCSRCDeleting
      "expired" -> pure VPCSRCExpired
      "failed" -> pure VPCSRCFailed
      "initiating-request" -> pure VPCSRCInitiatingRequest
      "pending-acceptance" -> pure VPCSRCPendingAcceptance
      "provisioning" -> pure VPCSRCProvisioning
      "rejected" -> pure VPCSRCRejected
      e ->
        fromTextError $
          "Failure parsing VPCPeeringConnectionStateReasonCode from value: '" <> e
            <> "'. Accepted values: active, deleted, deleting, expired, failed, initiating-request, pending-acceptance, provisioning, rejected"

instance ToText VPCPeeringConnectionStateReasonCode where
  toText = \case
    VPCSRCActive -> "active"
    VPCSRCDeleted -> "deleted"
    VPCSRCDeleting -> "deleting"
    VPCSRCExpired -> "expired"
    VPCSRCFailed -> "failed"
    VPCSRCInitiatingRequest -> "initiating-request"
    VPCSRCPendingAcceptance -> "pending-acceptance"
    VPCSRCProvisioning -> "provisioning"
    VPCSRCRejected -> "rejected"

instance Hashable VPCPeeringConnectionStateReasonCode

instance NFData VPCPeeringConnectionStateReasonCode

instance ToByteString VPCPeeringConnectionStateReasonCode

instance ToQuery VPCPeeringConnectionStateReasonCode

instance ToHeader VPCPeeringConnectionStateReasonCode

instance FromXML VPCPeeringConnectionStateReasonCode where
  parseXML = parseXMLText "VPCPeeringConnectionStateReasonCode"
