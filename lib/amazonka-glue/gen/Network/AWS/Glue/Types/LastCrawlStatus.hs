{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LastCrawlStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LastCrawlStatus where

import Network.AWS.Prelude

data LastCrawlStatus
  = LCSCancelled
  | LCSFailed
  | LCSSucceeded
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

instance FromText LastCrawlStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure LCSCancelled
      "failed" -> pure LCSFailed
      "succeeded" -> pure LCSSucceeded
      e ->
        fromTextError $
          "Failure parsing LastCrawlStatus from value: '" <> e
            <> "'. Accepted values: cancelled, failed, succeeded"

instance ToText LastCrawlStatus where
  toText = \case
    LCSCancelled -> "CANCELLED"
    LCSFailed -> "FAILED"
    LCSSucceeded -> "SUCCEEDED"

instance Hashable LastCrawlStatus

instance NFData LastCrawlStatus

instance ToByteString LastCrawlStatus

instance ToQuery LastCrawlStatus

instance ToHeader LastCrawlStatus

instance FromJSON LastCrawlStatus where
  parseJSON = parseJSONText "LastCrawlStatus"
