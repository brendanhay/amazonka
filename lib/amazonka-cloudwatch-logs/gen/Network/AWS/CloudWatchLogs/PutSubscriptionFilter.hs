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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a subscription filter and associates it with the specified log group. Subscription filters allow you to subscribe to a real-time stream of log events ingested through 'PutLogEvents' and have them delivered to a specific destination. Currently, the supported destinations are:
--
--
--     * An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.
--
--     * A logical destination that belongs to a different account, for cross-account delivery.
--
--     * An Amazon Kinesis Firehose delivery stream that belongs to the same account as the subscription filter, for same-account delivery.
--
--     * An AWS Lambda function that belongs to the same account as the subscription filter, for same-account delivery.
--
--
--
-- There can only be one subscription filter associated with a log group. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group.
--
module Network.AWS.CloudWatchLogs.PutSubscriptionFilter
    (
    -- * Creating a Request
      putSubscriptionFilter
    , PutSubscriptionFilter
    -- * Request Lenses
    , psfDistribution
    , psfRoleARN
    , psfLogGroupName
    , psfFilterName
    , psfFilterPattern
    , psfDestinationARN

    -- * Destructuring the Response
    , putSubscriptionFilterResponse
    , PutSubscriptionFilterResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSubscriptionFilter' smart constructor.
data PutSubscriptionFilter = PutSubscriptionFilter'
  { _psfDistribution   :: !(Maybe Distribution)
  , _psfRoleARN        :: !(Maybe Text)
  , _psfLogGroupName   :: !Text
  , _psfFilterName     :: !Text
  , _psfFilterPattern  :: !Text
  , _psfDestinationARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutSubscriptionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psfDistribution' - The method used to distribute log data to the destination. By default log data is grouped by log stream, but the grouping can be set to random for a more even distribution. This property is only applicable when the destination is an Amazon Kinesis stream.
--
-- * 'psfRoleARN' - The ARN of an IAM role that grants CloudWatch Logs permissions to deliver ingested log events to the destination stream. You don't need to provide the ARN when you are working with a logical destination for cross-account delivery.
--
-- * 'psfLogGroupName' - The name of the log group.
--
-- * 'psfFilterName' - A name for the subscription filter. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group. To find the name of the filter currently associated with a log group, use 'DescribeSubscriptionFilters' .
--
-- * 'psfFilterPattern' - A filter pattern for subscribing to a filtered stream of log events.
--
-- * 'psfDestinationARN' - The ARN of the destination to deliver matching log events to. Currently, the supported destinations are:     * An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.     * A logical destination (specified using an ARN) belonging to a different account, for cross-account delivery.     * An Amazon Kinesis Firehose delivery stream belonging to the same account as the subscription filter, for same-account delivery.     * An AWS Lambda function belonging to the same account as the subscription filter, for same-account delivery.
putSubscriptionFilter
    :: Text -- ^ 'psfLogGroupName'
    -> Text -- ^ 'psfFilterName'
    -> Text -- ^ 'psfFilterPattern'
    -> Text -- ^ 'psfDestinationARN'
    -> PutSubscriptionFilter
putSubscriptionFilter pLogGroupName_ pFilterName_ pFilterPattern_ pDestinationARN_ =
  PutSubscriptionFilter'
    { _psfDistribution = Nothing
    , _psfRoleARN = Nothing
    , _psfLogGroupName = pLogGroupName_
    , _psfFilterName = pFilterName_
    , _psfFilterPattern = pFilterPattern_
    , _psfDestinationARN = pDestinationARN_
    }


-- | The method used to distribute log data to the destination. By default log data is grouped by log stream, but the grouping can be set to random for a more even distribution. This property is only applicable when the destination is an Amazon Kinesis stream.
psfDistribution :: Lens' PutSubscriptionFilter (Maybe Distribution)
psfDistribution = lens _psfDistribution (\ s a -> s{_psfDistribution = a})

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to deliver ingested log events to the destination stream. You don't need to provide the ARN when you are working with a logical destination for cross-account delivery.
psfRoleARN :: Lens' PutSubscriptionFilter (Maybe Text)
psfRoleARN = lens _psfRoleARN (\ s a -> s{_psfRoleARN = a})

-- | The name of the log group.
psfLogGroupName :: Lens' PutSubscriptionFilter Text
psfLogGroupName = lens _psfLogGroupName (\ s a -> s{_psfLogGroupName = a})

-- | A name for the subscription filter. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group. To find the name of the filter currently associated with a log group, use 'DescribeSubscriptionFilters' .
psfFilterName :: Lens' PutSubscriptionFilter Text
psfFilterName = lens _psfFilterName (\ s a -> s{_psfFilterName = a})

-- | A filter pattern for subscribing to a filtered stream of log events.
psfFilterPattern :: Lens' PutSubscriptionFilter Text
psfFilterPattern = lens _psfFilterPattern (\ s a -> s{_psfFilterPattern = a})

-- | The ARN of the destination to deliver matching log events to. Currently, the supported destinations are:     * An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.     * A logical destination (specified using an ARN) belonging to a different account, for cross-account delivery.     * An Amazon Kinesis Firehose delivery stream belonging to the same account as the subscription filter, for same-account delivery.     * An AWS Lambda function belonging to the same account as the subscription filter, for same-account delivery.
psfDestinationARN :: Lens' PutSubscriptionFilter Text
psfDestinationARN = lens _psfDestinationARN (\ s a -> s{_psfDestinationARN = a})

instance AWSRequest PutSubscriptionFilter where
        type Rs PutSubscriptionFilter =
             PutSubscriptionFilterResponse
        request = postJSON cloudWatchLogs
        response = receiveNull PutSubscriptionFilterResponse'

instance Hashable PutSubscriptionFilter where

instance NFData PutSubscriptionFilter where

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
              (catMaybes
                 [("distribution" .=) <$> _psfDistribution,
                  ("roleArn" .=) <$> _psfRoleARN,
                  Just ("logGroupName" .= _psfLogGroupName),
                  Just ("filterName" .= _psfFilterName),
                  Just ("filterPattern" .= _psfFilterPattern),
                  Just ("destinationArn" .= _psfDestinationARN)])

instance ToPath PutSubscriptionFilter where
        toPath = const "/"

instance ToQuery PutSubscriptionFilter where
        toQuery = const mempty

-- | /See:/ 'putSubscriptionFilterResponse' smart constructor.
data PutSubscriptionFilterResponse =
  PutSubscriptionFilterResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutSubscriptionFilterResponse' with the minimum fields required to make a request.
--
putSubscriptionFilterResponse
    :: PutSubscriptionFilterResponse
putSubscriptionFilterResponse = PutSubscriptionFilterResponse'


instance NFData PutSubscriptionFilterResponse where
