{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTJobsData.Types.Sum where

import Network.AWS.Prelude

data JobExecutionStatus
  = Canceled
  | Failed
  | InProgress
  | Queued
  | Rejected
  | Removed
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobExecutionStatus where
    parser = takeLowerText >>= \case
        "canceled" -> pure Canceled
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "queued" -> pure Queued
        "rejected" -> pure Rejected
        "removed" -> pure Removed
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing JobExecutionStatus from value: '" <> e
           <> "'. Accepted values: canceled, failed, in_progress, queued, rejected, removed, succeeded"

instance ToText JobExecutionStatus where
    toText = \case
        Canceled -> "CANCELED"
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Queued -> "QUEUED"
        Rejected -> "REJECTED"
        Removed -> "REMOVED"
        Succeeded -> "SUCCEEDED"

instance Hashable     JobExecutionStatus
instance NFData       JobExecutionStatus
instance ToByteString JobExecutionStatus
instance ToQuery      JobExecutionStatus
instance ToHeader     JobExecutionStatus

instance ToJSON JobExecutionStatus where
    toJSON = toJSONText

instance FromJSON JobExecutionStatus where
    parseJSON = parseJSONText "JobExecutionStatus"
