{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutSubscriptionFilter
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a subscription filter and associates it with the
-- specified log group. Subscription filters allow you to subscribe to a
-- real-time stream of log events ingested through @PutLogEvents@ requests
-- and have them delivered to a specific destination. Currently, the
-- supported destinations are:
--
-- -   A Amazon Kinesis stream belonging to the same account as the
--     subscription filter, for same-account delivery.
-- -   A logical destination (used via an ARN of @Destination@) belonging
--     to a different account, for cross-account delivery.
--
-- Currently there can only be one subscription filter associated with a
-- log group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutSubscriptionFilter.html AWS API Reference> for PutSubscriptionFilter.
module Network.AWS.CloudWatchLogs.PutSubscriptionFilter
    (
    -- * Creating a Request
      PutSubscriptionFilter
    , putSubscriptionFilter
    -- * Request Lenses
    , psfRoleARN
    , psfLogGroupName
    , psfFilterName
    , psfFilterPattern
    , psfDestinationARN

    -- * Destructuring the Response
    , PutSubscriptionFilterResponse
    , putSubscriptionFilterResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSubscriptionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psfRoleARN'
--
-- * 'psfLogGroupName'
--
-- * 'psfFilterName'
--
-- * 'psfFilterPattern'
--
-- * 'psfDestinationARN'
data PutSubscriptionFilter = PutSubscriptionFilter'
    { _psfRoleARN :: !(Maybe Text)
    , _psfLogGroupName :: !Text
    , _psfFilterName :: !Text
    , _psfFilterPattern :: !Text
    , _psfDestinationARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutSubscriptionFilter' smart constructor.
putSubscriptionFilter :: Text -> Text -> Text -> Text -> PutSubscriptionFilter
putSubscriptionFilter pLogGroupName_ pFilterName_ pFilterPattern_ pDestinationARN_ = 
    PutSubscriptionFilter'
    { _psfRoleARN = Nothing
    , _psfLogGroupName = pLogGroupName_
    , _psfFilterName = pFilterName_
    , _psfFilterPattern = pFilterPattern_
    , _psfDestinationARN = pDestinationARN_
    }

-- | The ARN of an IAM role that grants Amazon CloudWatch Logs permissions to
-- deliver ingested log events to the destination stream. You don\'t need
-- to provide the ARN when you are working with a logical destination (used
-- via an ARN of @Destination@) for cross-account delivery.
psfRoleARN :: Lens' PutSubscriptionFilter (Maybe Text)
psfRoleARN = lens _psfRoleARN (\ s a -> s{_psfRoleARN = a});

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

-- | The ARN of the destination to deliver matching log events to. Currently,
-- the supported destinations are:
--
-- -   A Amazon Kinesis stream belonging to the same account as the
--     subscription filter, for same-account delivery.
-- -   A logical destination (used via an ARN of @Destination@) belonging
--     to a different account, for cross-account delivery.
psfDestinationARN :: Lens' PutSubscriptionFilter Text
psfDestinationARN = lens _psfDestinationARN (\ s a -> s{_psfDestinationARN = a});

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
              ["roleArn" .= _psfRoleARN,
               "logGroupName" .= _psfLogGroupName,
               "filterName" .= _psfFilterName,
               "filterPattern" .= _psfFilterPattern,
               "destinationArn" .= _psfDestinationARN]

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
