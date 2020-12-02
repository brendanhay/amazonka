{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus where

import Network.AWS.Prelude

data StackSetDriftDetectionStatus
  = SSDDSCompleted
  | SSDDSFailed
  | SSDDSInProgress
  | SSDDSPartialSuccess
  | SSDDSStopped
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

instance FromText StackSetDriftDetectionStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure SSDDSCompleted
      "failed" -> pure SSDDSFailed
      "in_progress" -> pure SSDDSInProgress
      "partial_success" -> pure SSDDSPartialSuccess
      "stopped" -> pure SSDDSStopped
      e ->
        fromTextError $
          "Failure parsing StackSetDriftDetectionStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, in_progress, partial_success, stopped"

instance ToText StackSetDriftDetectionStatus where
  toText = \case
    SSDDSCompleted -> "COMPLETED"
    SSDDSFailed -> "FAILED"
    SSDDSInProgress -> "IN_PROGRESS"
    SSDDSPartialSuccess -> "PARTIAL_SUCCESS"
    SSDDSStopped -> "STOPPED"

instance Hashable StackSetDriftDetectionStatus

instance NFData StackSetDriftDetectionStatus

instance ToByteString StackSetDriftDetectionStatus

instance ToQuery StackSetDriftDetectionStatus

instance ToHeader StackSetDriftDetectionStatus

instance FromXML StackSetDriftDetectionStatus where
  parseXML = parseXMLText "StackSetDriftDetectionStatus"
