{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Sum where

import           Network.AWS.Prelude

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
instance ToByteString QueueAttributeName
instance ToQuery      QueueAttributeName
instance ToHeader     QueueAttributeName

instance FromXML QueueAttributeName where
    parseXML = parseXMLText "QueueAttributeName"
