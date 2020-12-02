{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayAttachmentState
  = TGASAvailable
  | TGASDeleted
  | TGASDeleting
  | TGASFailed
  | TGASFailing
  | TGASInitiating
  | TGASInitiatingRequest
  | TGASModifying
  | TGASPending
  | TGASPendingAcceptance
  | TGASRejected
  | TGASRejecting
  | TGASRollingBack
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

instance FromText TransitGatewayAttachmentState where
  parser =
    takeLowerText >>= \case
      "available" -> pure TGASAvailable
      "deleted" -> pure TGASDeleted
      "deleting" -> pure TGASDeleting
      "failed" -> pure TGASFailed
      "failing" -> pure TGASFailing
      "initiating" -> pure TGASInitiating
      "initiatingrequest" -> pure TGASInitiatingRequest
      "modifying" -> pure TGASModifying
      "pending" -> pure TGASPending
      "pendingacceptance" -> pure TGASPendingAcceptance
      "rejected" -> pure TGASRejected
      "rejecting" -> pure TGASRejecting
      "rollingback" -> pure TGASRollingBack
      e ->
        fromTextError $
          "Failure parsing TransitGatewayAttachmentState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, failed, failing, initiating, initiatingrequest, modifying, pending, pendingacceptance, rejected, rejecting, rollingback"

instance ToText TransitGatewayAttachmentState where
  toText = \case
    TGASAvailable -> "available"
    TGASDeleted -> "deleted"
    TGASDeleting -> "deleting"
    TGASFailed -> "failed"
    TGASFailing -> "failing"
    TGASInitiating -> "initiating"
    TGASInitiatingRequest -> "initiatingRequest"
    TGASModifying -> "modifying"
    TGASPending -> "pending"
    TGASPendingAcceptance -> "pendingAcceptance"
    TGASRejected -> "rejected"
    TGASRejecting -> "rejecting"
    TGASRollingBack -> "rollingBack"

instance Hashable TransitGatewayAttachmentState

instance NFData TransitGatewayAttachmentState

instance ToByteString TransitGatewayAttachmentState

instance ToQuery TransitGatewayAttachmentState

instance ToHeader TransitGatewayAttachmentState

instance FromXML TransitGatewayAttachmentState where
  parseXML = parseXMLText "TransitGatewayAttachmentState"
