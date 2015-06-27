{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatchLogs.PutSubscriptionFilter
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates or updates a subscription filter and associates it with the
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
    , psfLogGroupName
    , psfFilterName
    , psfFilterPattern
    , psfDestinationARN
    , psfRoleARN

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
-- * 'psfLogGroupName'
--
-- * 'psfFilterName'
--
-- * 'psfFilterPattern'
--
-- * 'psfDestinationARN'
--
-- * 'psfRoleARN'
data PutSubscriptionFilter = PutSubscriptionFilter'
    { _psfLogGroupName   :: !Text
    , _psfFilterName     :: !Text
    , _psfFilterPattern  :: !Text
    , _psfDestinationARN :: !Text
    , _psfRoleARN        :: !Text
    } deriving (Eq,Read,Show)

-- | 'PutSubscriptionFilter' smart constructor.
putSubscriptionFilter :: Text -> Text -> Text -> Text -> Text -> PutSubscriptionFilter
putSubscriptionFilter pLogGroupName pFilterName pFilterPattern pDestinationARN pRoleARN =
    PutSubscriptionFilter'
    { _psfLogGroupName = pLogGroupName
    , _psfFilterName = pFilterName
    , _psfFilterPattern = pFilterPattern
    , _psfDestinationARN = pDestinationARN
    , _psfRoleARN = pRoleARN
    }

-- | The name of the log group to associate the subscription filter with.
psfLogGroupName :: Lens' PutSubscriptionFilter Text
psfLogGroupName = lens _psfLogGroupName (\ s a -> s{_psfLogGroupName = a});

-- | A name for the subscription filter.
psfFilterName :: Lens' PutSubscriptionFilter Text
psfFilterName = lens _psfFilterName (\ s a -> s{_psfFilterName = a});

-- | A valid CloudWatch Logs filter pattern for subscribing to a filtered
-- stream of log events.
psfFilterPattern :: Lens' PutSubscriptionFilter Text
psfFilterPattern = lens _psfFilterPattern (\ s a -> s{_psfFilterPattern = a});

-- | The ARN of an Amazon Kinesis stream to deliver matching log events to.
psfDestinationARN :: Lens' PutSubscriptionFilter Text
psfDestinationARN = lens _psfDestinationARN (\ s a -> s{_psfDestinationARN = a});

-- | The ARN of an IAM role that grants Amazon CloudWatch Logs permissions to
-- do Amazon Kinesis PutRecord requests on the desitnation stream.
psfRoleARN :: Lens' PutSubscriptionFilter Text
psfRoleARN = lens _psfRoleARN (\ s a -> s{_psfRoleARN = a});

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
              ["logGroupName" .= _psfLogGroupName,
               "filterName" .= _psfFilterName,
               "filterPattern" .= _psfFilterPattern,
               "destinationArn" .= _psfDestinationARN,
               "roleArn" .= _psfRoleARN]

instance ToPath PutSubscriptionFilter where
        toPath = const "/"

instance ToQuery PutSubscriptionFilter where
        toQuery = const mempty

-- | /See:/ 'putSubscriptionFilterResponse' smart constructor.
data PutSubscriptionFilterResponse =
    PutSubscriptionFilterResponse'
    deriving (Eq,Read,Show)

-- | 'PutSubscriptionFilterResponse' smart constructor.
putSubscriptionFilterResponse :: PutSubscriptionFilterResponse
putSubscriptionFilterResponse = PutSubscriptionFilterResponse'
