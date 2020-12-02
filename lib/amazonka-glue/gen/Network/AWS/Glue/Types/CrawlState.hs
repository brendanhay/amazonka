{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlState where

import Network.AWS.Prelude

data CrawlState
  = CSCancelled
  | CSCancelling
  | CSFailed
  | CSRunning
  | CSSucceeded
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

instance FromText CrawlState where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure CSCancelled
      "cancelling" -> pure CSCancelling
      "failed" -> pure CSFailed
      "running" -> pure CSRunning
      "succeeded" -> pure CSSucceeded
      e ->
        fromTextError $
          "Failure parsing CrawlState from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, failed, running, succeeded"

instance ToText CrawlState where
  toText = \case
    CSCancelled -> "CANCELLED"
    CSCancelling -> "CANCELLING"
    CSFailed -> "FAILED"
    CSRunning -> "RUNNING"
    CSSucceeded -> "SUCCEEDED"

instance Hashable CrawlState

instance NFData CrawlState

instance ToByteString CrawlState

instance ToQuery CrawlState

instance ToHeader CrawlState

instance ToJSON CrawlState where
  toJSON = toJSONText

instance FromJSON CrawlState where
  parseJSON = parseJSONText "CrawlState"
