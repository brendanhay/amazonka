{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode where

import Network.AWS.Prelude

data OutboundCrossClusterSearchConnectionStatusCode
  = OCCSCSCActive
  | OCCSCSCDeleted
  | OCCSCSCDeleting
  | OCCSCSCPendingAcceptance
  | OCCSCSCProvisioning
  | OCCSCSCRejected
  | OCCSCSCValidating
  | OCCSCSCValidationFailed
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

instance FromText OutboundCrossClusterSearchConnectionStatusCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure OCCSCSCActive
      "deleted" -> pure OCCSCSCDeleted
      "deleting" -> pure OCCSCSCDeleting
      "pending_acceptance" -> pure OCCSCSCPendingAcceptance
      "provisioning" -> pure OCCSCSCProvisioning
      "rejected" -> pure OCCSCSCRejected
      "validating" -> pure OCCSCSCValidating
      "validation_failed" -> pure OCCSCSCValidationFailed
      e ->
        fromTextError $
          "Failure parsing OutboundCrossClusterSearchConnectionStatusCode from value: '" <> e
            <> "'. Accepted values: active, deleted, deleting, pending_acceptance, provisioning, rejected, validating, validation_failed"

instance ToText OutboundCrossClusterSearchConnectionStatusCode where
  toText = \case
    OCCSCSCActive -> "ACTIVE"
    OCCSCSCDeleted -> "DELETED"
    OCCSCSCDeleting -> "DELETING"
    OCCSCSCPendingAcceptance -> "PENDING_ACCEPTANCE"
    OCCSCSCProvisioning -> "PROVISIONING"
    OCCSCSCRejected -> "REJECTED"
    OCCSCSCValidating -> "VALIDATING"
    OCCSCSCValidationFailed -> "VALIDATION_FAILED"

instance Hashable OutboundCrossClusterSearchConnectionStatusCode

instance NFData OutboundCrossClusterSearchConnectionStatusCode

instance ToByteString OutboundCrossClusterSearchConnectionStatusCode

instance ToQuery OutboundCrossClusterSearchConnectionStatusCode

instance ToHeader OutboundCrossClusterSearchConnectionStatusCode

instance FromJSON OutboundCrossClusterSearchConnectionStatusCode where
  parseJSON = parseJSONText "OutboundCrossClusterSearchConnectionStatusCode"
