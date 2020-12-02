{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeInstanceRefreshes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more instance refreshes.
--
--
-- You can determine the status of a request by looking at the @Status@ parameter. The following are the possible statuses:
--
--     * @Pending@ - The request was created, but the operation has not started.
--
--     * @InProgress@ - The operation is in progress.
--
--     * @Successful@ - The operation completed successfully.
--
--     * @Failed@ - The operation failed to complete. You can troubleshoot using the status reason and the scaling activities.
--
--     * @Cancelling@ - An ongoing operation is being cancelled. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started.
--
--     * @Cancelled@ - The operation is cancelled.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh> .
module Network.AWS.AutoScaling.DescribeInstanceRefreshes
  ( -- * Creating a Request
    describeInstanceRefreshes,
    DescribeInstanceRefreshes,

    -- * Request Lenses
    dirNextToken,
    dirMaxRecords,
    dirInstanceRefreshIds,
    dirAutoScalingGroupName,

    -- * Destructuring the Response
    describeInstanceRefreshesResponse,
    DescribeInstanceRefreshesResponse,

    -- * Response Lenses
    dirrsNextToken,
    dirrsInstanceRefreshes,
    dirrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstanceRefreshes' smart constructor.
data DescribeInstanceRefreshes = DescribeInstanceRefreshes'
  { _dirNextToken ::
      !(Maybe Text),
    _dirMaxRecords :: !(Maybe Int),
    _dirInstanceRefreshIds ::
      !(Maybe [Text]),
    _dirAutoScalingGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceRefreshes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dirMaxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- * 'dirInstanceRefreshIds' - One or more instance refresh IDs.
--
-- * 'dirAutoScalingGroupName' - The name of the Auto Scaling group.
describeInstanceRefreshes ::
  -- | 'dirAutoScalingGroupName'
  Text ->
  DescribeInstanceRefreshes
describeInstanceRefreshes pAutoScalingGroupName_ =
  DescribeInstanceRefreshes'
    { _dirNextToken = Nothing,
      _dirMaxRecords = Nothing,
      _dirInstanceRefreshIds = Nothing,
      _dirAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
dirNextToken :: Lens' DescribeInstanceRefreshes (Maybe Text)
dirNextToken = lens _dirNextToken (\s a -> s {_dirNextToken = a})

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
dirMaxRecords :: Lens' DescribeInstanceRefreshes (Maybe Int)
dirMaxRecords = lens _dirMaxRecords (\s a -> s {_dirMaxRecords = a})

-- | One or more instance refresh IDs.
dirInstanceRefreshIds :: Lens' DescribeInstanceRefreshes [Text]
dirInstanceRefreshIds = lens _dirInstanceRefreshIds (\s a -> s {_dirInstanceRefreshIds = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
dirAutoScalingGroupName :: Lens' DescribeInstanceRefreshes Text
dirAutoScalingGroupName = lens _dirAutoScalingGroupName (\s a -> s {_dirAutoScalingGroupName = a})

instance AWSRequest DescribeInstanceRefreshes where
  type
    Rs DescribeInstanceRefreshes =
      DescribeInstanceRefreshesResponse
  request = postQuery autoScaling
  response =
    receiveXMLWrapper
      "DescribeInstanceRefreshesResult"
      ( \s h x ->
          DescribeInstanceRefreshesResponse'
            <$> (x .@? "NextToken")
            <*> ( x .@? "InstanceRefreshes" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeInstanceRefreshes

instance NFData DescribeInstanceRefreshes

instance ToHeaders DescribeInstanceRefreshes where
  toHeaders = const mempty

instance ToPath DescribeInstanceRefreshes where
  toPath = const "/"

instance ToQuery DescribeInstanceRefreshes where
  toQuery DescribeInstanceRefreshes' {..} =
    mconcat
      [ "Action" =: ("DescribeInstanceRefreshes" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "NextToken" =: _dirNextToken,
        "MaxRecords" =: _dirMaxRecords,
        "InstanceRefreshIds"
          =: toQuery (toQueryList "member" <$> _dirInstanceRefreshIds),
        "AutoScalingGroupName" =: _dirAutoScalingGroupName
      ]

-- | /See:/ 'describeInstanceRefreshesResponse' smart constructor.
data DescribeInstanceRefreshesResponse = DescribeInstanceRefreshesResponse'
  { _dirrsNextToken ::
      !(Maybe Text),
    _dirrsInstanceRefreshes ::
      !( Maybe
           [InstanceRefresh]
       ),
    _dirrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceRefreshesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirrsNextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- * 'dirrsInstanceRefreshes' - The instance refreshes for the specified group.
--
-- * 'dirrsResponseStatus' - -- | The response status code.
describeInstanceRefreshesResponse ::
  -- | 'dirrsResponseStatus'
  Int ->
  DescribeInstanceRefreshesResponse
describeInstanceRefreshesResponse pResponseStatus_ =
  DescribeInstanceRefreshesResponse'
    { _dirrsNextToken = Nothing,
      _dirrsInstanceRefreshes = Nothing,
      _dirrsResponseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
dirrsNextToken :: Lens' DescribeInstanceRefreshesResponse (Maybe Text)
dirrsNextToken = lens _dirrsNextToken (\s a -> s {_dirrsNextToken = a})

-- | The instance refreshes for the specified group.
dirrsInstanceRefreshes :: Lens' DescribeInstanceRefreshesResponse [InstanceRefresh]
dirrsInstanceRefreshes = lens _dirrsInstanceRefreshes (\s a -> s {_dirrsInstanceRefreshes = a}) . _Default . _Coerce

-- | -- | The response status code.
dirrsResponseStatus :: Lens' DescribeInstanceRefreshesResponse Int
dirrsResponseStatus = lens _dirrsResponseStatus (\s a -> s {_dirrsResponseStatus = a})

instance NFData DescribeInstanceRefreshesResponse
