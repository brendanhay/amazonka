{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Sum where

import           Network.AWS.Prelude

data QueueAttributeName
    = MessageRetentionPeriod
    | LastModifiedTimestamp
    | VisibilityTimeout
    | RedrivePolicy
    | ApproximateNumberOfMessagesDelayed
    | MaximumMessageSize
    | DelaySeconds
    | QueueARN
    | ApproximateNumberOfMessages
    | ReceiveMessageWaitTimeSeconds
    | Policy
    | CreatedTimestamp
    | ApproximateNumberOfMessagesNotVisible
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
           <> "'. Accepted values: approximatenumberofmessages, approximatenumberofmessagesdelayed, approximatenumberofmessagesnotvisible, createdtimestamp, delayseconds, lastmodifiedtimestamp, maximummessagesize, messageretentionperiod, policy, queuearn, receivemessagewaittimeseconds, redrivepolicy, visibilitytimeout"

instance ToText QueueAttributeName where
    toText = \case
        ApproximateNumberOfMessages -> "approximatenumberofmessages"
        ApproximateNumberOfMessagesDelayed -> "approximatenumberofmessagesdelayed"
        ApproximateNumberOfMessagesNotVisible -> "approximatenumberofmessagesnotvisible"
        CreatedTimestamp -> "createdtimestamp"
        DelaySeconds -> "delayseconds"
        LastModifiedTimestamp -> "lastmodifiedtimestamp"
        MaximumMessageSize -> "maximummessagesize"
        MessageRetentionPeriod -> "messageretentionperiod"
        Policy -> "policy"
        QueueARN -> "queuearn"
        ReceiveMessageWaitTimeSeconds -> "receivemessagewaittimeseconds"
        RedrivePolicy -> "redrivepolicy"
        VisibilityTimeout -> "visibilitytimeout"

instance Hashable QueueAttributeName
instance ToQuery QueueAttributeName
instance ToHeader QueueAttributeName

instance FromXML QueueAttributeName where
    parseXML = parseXMLText "QueueAttributeName"
