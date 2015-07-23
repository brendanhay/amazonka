{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutSubscriptionFilter
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a subscription filter and associates it with the
-- specified log group. Subscription filters allow you to subscribe to a
-- real-time stream of log events ingested through @PutLogEvents@ requests
-- and have them delivered to a specific destination. Currently the only
-- supported destination is an Amazon Kinesis stream belonging to the same
-- account as the subscription filter.
--
-- Currently there can only be one subscription filter associated with a
-- log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutSubscriptionFilter.html>
module Network.AWS.CloudWatchLogs.PutSubscriptionFilter
    (
    -- * Request
      PutSubscriptionFilter
    -- ** Request constructor
    , putSubscriptionFilter
    -- ** Request lenses
    , psfrqLogGroupName
    , psfrqFilterName
    , psfrqFilterPattern
    , psfrqDestinationARN
    , psfrqRoleARN

    -- * Response
    , PutSubscriptionFilterResponse
    -- ** Response constructor
    , putSubscriptionFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putSubscriptionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psfrqLogGroupName'
--
-- * 'psfrqFilterName'
--
-- * 'psfrqFilterPattern'
--
-- * 'psfrqDestinationARN'
--
-- * 'psfrqRoleARN'
data PutSubscriptionFilter = PutSubscriptionFilter'
    { _psfrqLogGroupName   :: !Text
    , _psfrqFilterName     :: !Text
    , _psfrqFilterPattern  :: !Text
    , _psfrqDestinationARN :: !Text
    , _psfrqRoleARN        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutSubscriptionFilter' smart constructor.
putSubscriptionFilter :: Text -> Text -> Text -> Text -> Text -> PutSubscriptionFilter
putSubscriptionFilter pLogGroupName_ pFilterName_ pFilterPattern_ pDestinationARN_ pRoleARN_ =
    PutSubscriptionFilter'
    { _psfrqLogGroupName = pLogGroupName_
    , _psfrqFilterName = pFilterName_
    , _psfrqFilterPattern = pFilterPattern_
    , _psfrqDestinationARN = pDestinationARN_
    , _psfrqRoleARN = pRoleARN_
    }

-- | The name of the log group to associate the subscription filter with.
psfrqLogGroupName :: Lens' PutSubscriptionFilter Text
psfrqLogGroupName = lens _psfrqLogGroupName (\ s a -> s{_psfrqLogGroupName = a});

-- | A name for the subscription filter.
psfrqFilterName :: Lens' PutSubscriptionFilter Text
psfrqFilterName = lens _psfrqFilterName (\ s a -> s{_psfrqFilterName = a});

-- | A valid CloudWatch Logs filter pattern for subscribing to a filtered
-- stream of log events.
psfrqFilterPattern :: Lens' PutSubscriptionFilter Text
psfrqFilterPattern = lens _psfrqFilterPattern (\ s a -> s{_psfrqFilterPattern = a});

-- | The ARN of an Amazon Kinesis stream to deliver matching log events to.
psfrqDestinationARN :: Lens' PutSubscriptionFilter Text
psfrqDestinationARN = lens _psfrqDestinationARN (\ s a -> s{_psfrqDestinationARN = a});

-- | The ARN of an IAM role that grants Amazon CloudWatch Logs permissions to
-- do Amazon Kinesis PutRecord requests on the desitnation stream.
psfrqRoleARN :: Lens' PutSubscriptionFilter Text
psfrqRoleARN = lens _psfrqRoleARN (\ s a -> s{_psfrqRoleARN = a});

instance AWSRequest PutSubscriptionFilter where
        type Sv PutSubscriptionFilter = CloudWatchLogs
        type Rs PutSubscriptionFilter =
             PutSubscriptionFilterResponse
        request = postJSON
        response = receiveNull PutSubscriptionFilterResponse'

instance ToHeaders PutSubscriptionFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutSubscriptionFilter" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutSubscriptionFilter where
        toJSON PutSubscriptionFilter'{..}
          = object
              ["logGroupName" .= _psfrqLogGroupName,
               "filterName" .= _psfrqFilterName,
               "filterPattern" .= _psfrqFilterPattern,
               "destinationArn" .= _psfrqDestinationARN,
               "roleArn" .= _psfrqRoleARN]

instance ToPath PutSubscriptionFilter where
        toPath = const "/"

instance ToQuery PutSubscriptionFilter where
        toQuery = const mempty

-- | /See:/ 'putSubscriptionFilterResponse' smart constructor.
data PutSubscriptionFilterResponse =
    PutSubscriptionFilterResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutSubscriptionFilterResponse' smart constructor.
putSubscriptionFilterResponse :: PutSubscriptionFilterResponse
putSubscriptionFilterResponse = PutSubscriptionFilterResponse'
