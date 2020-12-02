{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Sum where

import Network.AWS.Prelude

data MessageAttribute
  = All
  | ApproximateFirstReceiveTimestamp
  | ApproximateReceiveCount
  | SenderId
  | SentTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MessageAttribute where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "approximatefirstreceivetimestamp" -> pure ApproximateFirstReceiveTimestamp
        "approximatereceivecount" -> pure ApproximateReceiveCount
        "senderid" -> pure SenderId
        "senttimestamp" -> pure SentTimestamp
        e -> fromTextError $ "Failure parsing MessageAttribute from value: '" <> e
           <> "'. Accepted values: all, approximatefirstreceivetimestamp, approximatereceivecount, senderid, senttimestamp"

instance ToText MessageAttribute where
    toText = \case
        All -> "All"
        ApproximateFirstReceiveTimestamp -> "ApproximateFirstReceiveTimestamp"
        ApproximateReceiveCount -> "ApproximateReceiveCount"
        SenderId -> "SenderId"
        SentTimestamp -> "SentTimestamp"

instance Hashable     MessageAttribute
instance NFData       MessageAttribute
instance ToByteString MessageAttribute
instance ToQuery      MessageAttribute
instance ToHeader     MessageAttribute

instance FromXML MessageAttribute where
    parseXML = parseXMLText "MessageAttribute"

data QueueAttributeName
  = QANAll
  | QANApproximateNumberOfMessages
  | QANApproximateNumberOfMessagesDelayed
  | QANApproximateNumberOfMessagesNotVisible
  | QANContentBasedDeduplication
  | QANCreatedTimestamp
  | QANDelaySeconds
  | QANFifoQueue
  | QANKMSDataKeyReusePeriodSeconds
  | QANKMSMasterKeyId
  | QANLastModifiedTimestamp
  | QANMaximumMessageSize
  | QANMessageRetentionPeriod
  | QANPolicy
  | QANQueueARN
  | QANReceiveMessageWaitTimeSeconds
  | QANRedrivePolicy
  | QANVisibilityTimeout
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QueueAttributeName where
    parser = takeLowerText >>= \case
        "all" -> pure QANAll
        "approximatenumberofmessages" -> pure QANApproximateNumberOfMessages
        "approximatenumberofmessagesdelayed" -> pure QANApproximateNumberOfMessagesDelayed
        "approximatenumberofmessagesnotvisible" -> pure QANApproximateNumberOfMessagesNotVisible
        "contentbaseddeduplication" -> pure QANContentBasedDeduplication
        "createdtimestamp" -> pure QANCreatedTimestamp
        "delayseconds" -> pure QANDelaySeconds
        "fifoqueue" -> pure QANFifoQueue
        "kmsdatakeyreuseperiodseconds" -> pure QANKMSDataKeyReusePeriodSeconds
        "kmsmasterkeyid" -> pure QANKMSMasterKeyId
        "lastmodifiedtimestamp" -> pure QANLastModifiedTimestamp
        "maximummessagesize" -> pure QANMaximumMessageSize
        "messageretentionperiod" -> pure QANMessageRetentionPeriod
        "policy" -> pure QANPolicy
        "queuearn" -> pure QANQueueARN
        "receivemessagewaittimeseconds" -> pure QANReceiveMessageWaitTimeSeconds
        "redrivepolicy" -> pure QANRedrivePolicy
        "visibilitytimeout" -> pure QANVisibilityTimeout
        e -> fromTextError $ "Failure parsing QueueAttributeName from value: '" <> e
           <> "'. Accepted values: all, approximatenumberofmessages, approximatenumberofmessagesdelayed, approximatenumberofmessagesnotvisible, contentbaseddeduplication, createdtimestamp, delayseconds, fifoqueue, kmsdatakeyreuseperiodseconds, kmsmasterkeyid, lastmodifiedtimestamp, maximummessagesize, messageretentionperiod, policy, queuearn, receivemessagewaittimeseconds, redrivepolicy, visibilitytimeout"

instance ToText QueueAttributeName where
    toText = \case
        QANAll -> "All"
        QANApproximateNumberOfMessages -> "ApproximateNumberOfMessages"
        QANApproximateNumberOfMessagesDelayed -> "ApproximateNumberOfMessagesDelayed"
        QANApproximateNumberOfMessagesNotVisible -> "ApproximateNumberOfMessagesNotVisible"
        QANContentBasedDeduplication -> "ContentBasedDeduplication"
        QANCreatedTimestamp -> "CreatedTimestamp"
        QANDelaySeconds -> "DelaySeconds"
        QANFifoQueue -> "FifoQueue"
        QANKMSDataKeyReusePeriodSeconds -> "KmsDataKeyReusePeriodSeconds"
        QANKMSMasterKeyId -> "KmsMasterKeyId"
        QANLastModifiedTimestamp -> "LastModifiedTimestamp"
        QANMaximumMessageSize -> "MaximumMessageSize"
        QANMessageRetentionPeriod -> "MessageRetentionPeriod"
        QANPolicy -> "Policy"
        QANQueueARN -> "QueueArn"
        QANReceiveMessageWaitTimeSeconds -> "ReceiveMessageWaitTimeSeconds"
        QANRedrivePolicy -> "RedrivePolicy"
        QANVisibilityTimeout -> "VisibilityTimeout"

instance Hashable     QueueAttributeName
instance NFData       QueueAttributeName
instance ToByteString QueueAttributeName
instance ToQuery      QueueAttributeName
instance ToHeader     QueueAttributeName

instance FromXML QueueAttributeName where
    parseXML = parseXMLText "QueueAttributeName"
