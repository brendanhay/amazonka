{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionFailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionFailureType where

import Network.AWS.Prelude

data JobExecutionFailureType
  = JEFTAll
  | JEFTFailed
  | JEFTRejected
  | JEFTTimedOut
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

instance FromText JobExecutionFailureType where
  parser =
    takeLowerText >>= \case
      "all" -> pure JEFTAll
      "failed" -> pure JEFTFailed
      "rejected" -> pure JEFTRejected
      "timed_out" -> pure JEFTTimedOut
      e ->
        fromTextError $
          "Failure parsing JobExecutionFailureType from value: '" <> e
            <> "'. Accepted values: all, failed, rejected, timed_out"

instance ToText JobExecutionFailureType where
  toText = \case
    JEFTAll -> "ALL"
    JEFTFailed -> "FAILED"
    JEFTRejected -> "REJECTED"
    JEFTTimedOut -> "TIMED_OUT"

instance Hashable JobExecutionFailureType

instance NFData JobExecutionFailureType

instance ToByteString JobExecutionFailureType

instance ToQuery JobExecutionFailureType

instance ToHeader JobExecutionFailureType

instance ToJSON JobExecutionFailureType where
  toJSON = toJSONText

instance FromJSON JobExecutionFailureType where
  parseJSON = parseJSONText "JobExecutionFailureType"
