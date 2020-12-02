{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus where

import Network.AWS.Prelude

data ActionHistoryStatus
  = AHSCompleted
  | AHSFailed
  | AHSUnknown
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

instance FromText ActionHistoryStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure AHSCompleted
      "failed" -> pure AHSFailed
      "unknown" -> pure AHSUnknown
      e ->
        fromTextError $
          "Failure parsing ActionHistoryStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, unknown"

instance ToText ActionHistoryStatus where
  toText = \case
    AHSCompleted -> "Completed"
    AHSFailed -> "Failed"
    AHSUnknown -> "Unknown"

instance Hashable ActionHistoryStatus

instance NFData ActionHistoryStatus

instance ToByteString ActionHistoryStatus

instance ToQuery ActionHistoryStatus

instance ToHeader ActionHistoryStatus

instance FromXML ActionHistoryStatus where
  parseXML = parseXMLText "ActionHistoryStatus"
