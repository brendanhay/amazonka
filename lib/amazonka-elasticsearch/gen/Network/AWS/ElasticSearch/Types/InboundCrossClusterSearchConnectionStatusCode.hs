{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode where

import Network.AWS.Prelude

data InboundCrossClusterSearchConnectionStatusCode
  = Approved
  | Deleted
  | Deleting
  | PendingAcceptance
  | Rejected
  | Rejecting
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

instance FromText InboundCrossClusterSearchConnectionStatusCode where
  parser =
    takeLowerText >>= \case
      "approved" -> pure Approved
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "pending_acceptance" -> pure PendingAcceptance
      "rejected" -> pure Rejected
      "rejecting" -> pure Rejecting
      e ->
        fromTextError $
          "Failure parsing InboundCrossClusterSearchConnectionStatusCode from value: '" <> e
            <> "'. Accepted values: approved, deleted, deleting, pending_acceptance, rejected, rejecting"

instance ToText InboundCrossClusterSearchConnectionStatusCode where
  toText = \case
    Approved -> "APPROVED"
    Deleted -> "DELETED"
    Deleting -> "DELETING"
    PendingAcceptance -> "PENDING_ACCEPTANCE"
    Rejected -> "REJECTED"
    Rejecting -> "REJECTING"

instance Hashable InboundCrossClusterSearchConnectionStatusCode

instance NFData InboundCrossClusterSearchConnectionStatusCode

instance ToByteString InboundCrossClusterSearchConnectionStatusCode

instance ToQuery InboundCrossClusterSearchConnectionStatusCode

instance ToHeader InboundCrossClusterSearchConnectionStatusCode

instance FromJSON InboundCrossClusterSearchConnectionStatusCode where
  parseJSON = parseJSONText "InboundCrossClusterSearchConnectionStatusCode"
