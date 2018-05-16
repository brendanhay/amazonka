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
-- Module      : Network.AWS.AWSHealth.DescribeEntityAggregates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of entities that are affected by each of the specified events. If no events are specified, the counts of all affected entities are returned.
--
--
module Network.AWS.AWSHealth.DescribeEntityAggregates
    (
    -- * Creating a Request
      describeEntityAggregates
    , DescribeEntityAggregates
    -- * Request Lenses
    , deaEventARNs

    -- * Destructuring the Response
    , describeEntityAggregatesResponse
    , DescribeEntityAggregatesResponse
    -- * Response Lenses
    , dearsEntityAggregates
    , dearsResponseStatus
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEntityAggregates' smart constructor.
newtype DescribeEntityAggregates = DescribeEntityAggregates'
  { _deaEventARNs :: Maybe (List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEntityAggregates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deaEventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331", "arn:aws:health:us-west-1::event/AWS_EBS_LOST_VOLUME_xyz"@
describeEntityAggregates
    :: DescribeEntityAggregates
describeEntityAggregates = DescribeEntityAggregates' {_deaEventARNs = Nothing}


-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/AWS_EC2_MAINTENANCE_5331", "arn:aws:health:us-west-1::event/AWS_EBS_LOST_VOLUME_xyz"@
deaEventARNs :: Lens' DescribeEntityAggregates (Maybe (NonEmpty Text))
deaEventARNs = lens _deaEventARNs (\ s a -> s{_deaEventARNs = a}) . mapping _List1

instance AWSRequest DescribeEntityAggregates where
        type Rs DescribeEntityAggregates =
             DescribeEntityAggregatesResponse
        request = postJSON awsHealth
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEntityAggregatesResponse' <$>
                   (x .?> "entityAggregates" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeEntityAggregates where

instance NFData DescribeEntityAggregates where

instance ToHeaders DescribeEntityAggregates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSHealth_20160804.DescribeEntityAggregates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEntityAggregates where
        toJSON DescribeEntityAggregates'{..}
          = object
              (catMaybes [("eventArns" .=) <$> _deaEventARNs])

instance ToPath DescribeEntityAggregates where
        toPath = const "/"

instance ToQuery DescribeEntityAggregates where
        toQuery = const mempty

-- | /See:/ 'describeEntityAggregatesResponse' smart constructor.
data DescribeEntityAggregatesResponse = DescribeEntityAggregatesResponse'
  { _dearsEntityAggregates :: !(Maybe [EntityAggregate])
  , _dearsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEntityAggregatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dearsEntityAggregates' - The number of entities that are affected by each of the specified events.
--
-- * 'dearsResponseStatus' - -- | The response status code.
describeEntityAggregatesResponse
    :: Int -- ^ 'dearsResponseStatus'
    -> DescribeEntityAggregatesResponse
describeEntityAggregatesResponse pResponseStatus_ =
  DescribeEntityAggregatesResponse'
    {_dearsEntityAggregates = Nothing, _dearsResponseStatus = pResponseStatus_}


-- | The number of entities that are affected by each of the specified events.
dearsEntityAggregates :: Lens' DescribeEntityAggregatesResponse [EntityAggregate]
dearsEntityAggregates = lens _dearsEntityAggregates (\ s a -> s{_dearsEntityAggregates = a}) . _Default . _Coerce

-- | -- | The response status code.
dearsResponseStatus :: Lens' DescribeEntityAggregatesResponse Int
dearsResponseStatus = lens _dearsResponseStatus (\ s a -> s{_dearsResponseStatus = a})

instance NFData DescribeEntityAggregatesResponse
         where
