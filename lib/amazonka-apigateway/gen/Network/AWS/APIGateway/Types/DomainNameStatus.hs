{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DomainNameStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DomainNameStatus where

import Network.AWS.Prelude

data DomainNameStatus
  = DNSAvailable
  | DNSPending
  | DNSUpdating
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

instance FromText DomainNameStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure DNSAvailable
      "pending" -> pure DNSPending
      "updating" -> pure DNSUpdating
      e ->
        fromTextError $
          "Failure parsing DomainNameStatus from value: '" <> e
            <> "'. Accepted values: available, pending, updating"

instance ToText DomainNameStatus where
  toText = \case
    DNSAvailable -> "AVAILABLE"
    DNSPending -> "PENDING"
    DNSUpdating -> "UPDATING"

instance Hashable DomainNameStatus

instance NFData DomainNameStatus

instance ToByteString DomainNameStatus

instance ToQuery DomainNameStatus

instance ToHeader DomainNameStatus

instance FromJSON DomainNameStatus where
  parseJSON = parseJSONText "DomainNameStatus"
