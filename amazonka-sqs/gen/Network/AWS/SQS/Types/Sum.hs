{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Sum where

import           Network.AWS.Prelude

data MessageAttribute
    = All
    | ApproximateFirstReceiveTimestamp
    | ApproximateReceiveCount
    | SenderId
    | SentTimestamp
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MessageAttribute where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "approximatefirstreceivetimestamp" -> pure ApproximateFirstReceiveTimestamp
        "approximatereceivecount" -> pure ApproximateReceiveCount
        "senderid" -> pure SenderId
        "senttimestamp" -> pure SentTimestamp
        e -> fromTextError $ "Failure parsing MessageAttribute from value: '" <> e
           <> "'. Accepted values: All, ApproximateFirstReceiveTimestamp, ApproximateReceiveCount, SenderId, SentTimestamp"

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
    = ApproximateNumberOfMessages
    | ApproximateNumberOfMessagesDelayed
    | ApproximateNumberOfMessagesNotVisible
    | CreatedTimestamp
    | DelaySeconds
    | LastModifiedTimestamp
    | MaximumMessageSize
    | MessageRetentionPeriod
    | Policy
    | QueueARN
    | ReceiveMessageWaitTimeSeconds
    | RedrivePolicy
    | VisibilityTimeout
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText QueueAttributeName where
    parser = takeLowerText >>= \case
        "approximatenumberofmessages" -> pure ApproximateNumberOfMessages
        "approximatenumberofmessagesdelayed" -> pure ApproximateNumberOfMessagesDelayed
        "approximatenumberofmessagesnotvisible" -> pure ApproximateNumberOfMessagesNotVisible
        "createdtimestamp" -> pure CreatedTimestamp
        "delayseconds" -> pure DelaySeconds
        "lastmodifiedtimestamp" -> pure LastModifiedTimestamp
        "maximummessagesize" -> pure MaximumMessageSize
        "messageretentionperiod" -> pure MessageRetentionPeriod
        "policy" -> pure Policy
        "queuearn" -> pure QueueARN
        "receivemessagewaittimeseconds" -> pure ReceiveMessageWaitTimeSeconds
        "redrivepolicy" -> pure RedrivePolicy
        "visibilitytimeout" -> pure VisibilityTimeout
        e -> fromTextError $ "Failure parsing QueueAttributeName from value: '" <> e
           <> "'. Accepted values: ApproximateNumberOfMessages, ApproximateNumberOfMessagesDelayed, ApproximateNumberOfMessagesNotVisible, CreatedTimestamp, DelaySeconds, LastModifiedTimestamp, MaximumMessageSize, MessageRetentionPeriod, Policy, QueueArn, ReceiveMessageWaitTimeSeconds, RedrivePolicy, VisibilityTimeout"

instance ToText QueueAttributeName where
    toText = \case
        ApproximateNumberOfMessages -> "ApproximateNumberOfMessages"
        ApproximateNumberOfMessagesDelayed -> "ApproximateNumberOfMessagesDelayed"
        ApproximateNumberOfMessagesNotVisible -> "ApproximateNumberOfMessagesNotVisible"
        CreatedTimestamp -> "CreatedTimestamp"
        DelaySeconds -> "DelaySeconds"
        LastModifiedTimestamp -> "LastModifiedTimestamp"
        MaximumMessageSize -> "MaximumMessageSize"
        MessageRetentionPeriod -> "MessageRetentionPeriod"
        Policy -> "Policy"
        QueueARN -> "QueueArn"
        ReceiveMessageWaitTimeSeconds -> "ReceiveMessageWaitTimeSeconds"
        RedrivePolicy -> "RedrivePolicy"
        VisibilityTimeout -> "VisibilityTimeout"

instance Hashable     QueueAttributeName
instance NFData       QueueAttributeName
instance ToByteString QueueAttributeName
instance ToQuery      QueueAttributeName
instance ToHeader     QueueAttributeName

instance FromXML QueueAttributeName where
    parseXML = parseXMLText "QueueAttributeName"
