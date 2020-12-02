{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftDetectionStatus where

import Network.AWS.Prelude

data StackDriftDetectionStatus
  = DetectionComplete
  | DetectionFailed
  | DetectionInProgress
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

instance FromText StackDriftDetectionStatus where
  parser =
    takeLowerText >>= \case
      "detection_complete" -> pure DetectionComplete
      "detection_failed" -> pure DetectionFailed
      "detection_in_progress" -> pure DetectionInProgress
      e ->
        fromTextError $
          "Failure parsing StackDriftDetectionStatus from value: '" <> e
            <> "'. Accepted values: detection_complete, detection_failed, detection_in_progress"

instance ToText StackDriftDetectionStatus where
  toText = \case
    DetectionComplete -> "DETECTION_COMPLETE"
    DetectionFailed -> "DETECTION_FAILED"
    DetectionInProgress -> "DETECTION_IN_PROGRESS"

instance Hashable StackDriftDetectionStatus

instance NFData StackDriftDetectionStatus

instance ToByteString StackDriftDetectionStatus

instance ToQuery StackDriftDetectionStatus

instance ToHeader StackDriftDetectionStatus

instance FromXML StackDriftDetectionStatus where
  parseXML = parseXMLText "StackDriftDetectionStatus"
