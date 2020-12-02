{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ReachabilityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ReachabilityStatus where

import Network.AWS.Prelude

data ReachabilityStatus
  = Done
  | Expired
  | Pending
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

instance FromText ReachabilityStatus where
  parser =
    takeLowerText >>= \case
      "done" -> pure Done
      "expired" -> pure Expired
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing ReachabilityStatus from value: '" <> e
            <> "'. Accepted values: done, expired, pending"

instance ToText ReachabilityStatus where
  toText = \case
    Done -> "DONE"
    Expired -> "EXPIRED"
    Pending -> "PENDING"

instance Hashable ReachabilityStatus

instance NFData ReachabilityStatus

instance ToByteString ReachabilityStatus

instance ToQuery ReachabilityStatus

instance ToHeader ReachabilityStatus

instance FromJSON ReachabilityStatus where
  parseJSON = parseJSONText "ReachabilityStatus"
