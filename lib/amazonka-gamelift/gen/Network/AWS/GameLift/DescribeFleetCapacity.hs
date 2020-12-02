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
-- Module      : Network.AWS.GameLift.DescribeFleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current capacity statistics for one or more fleets. These statistics present a snapshot of the fleet's instances and provide insight on current or imminent scaling activity. To get statistics on game hosting activity in the fleet, see 'DescribeFleetUtilization' .
--
--
-- You can request capacity for all fleets or specify a list of one or more fleet identifiers. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetCapacity' object is returned for each requested fleet ID. When a list of fleet IDs is provided, attribute objects are returned only for fleets that currently exist.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html#gamelift-metrics-fleet GameLift Metrics for Fleets>
--
-- __Related operations__
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--     * 'DescribeFleetCapacity'
--
--     * 'DescribeFleetPortSettings'
--
--     * 'DescribeFleetUtilization'
--
--     * 'DescribeRuntimeConfiguration'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * 'DescribeFleetEvents'
--
--
--
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetCapacity
  ( -- * Creating a Request
    describeFleetCapacity,
    DescribeFleetCapacity,

    -- * Request Lenses
    dfcNextToken,
    dfcLimit,
    dfcFleetIds,

    -- * Destructuring the Response
    describeFleetCapacityResponse,
    DescribeFleetCapacityResponse,

    -- * Response Lenses
    dfcrsNextToken,
    dfcrsFleetCapacity,
    dfcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'describeFleetCapacity' smart constructor.
data DescribeFleetCapacity = DescribeFleetCapacity'
  { _dfcNextToken ::
      !(Maybe Text),
    _dfcLimit :: !(Maybe Nat),
    _dfcFleetIds :: !(Maybe (List1 Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleetCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- * 'dfcLimit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- * 'dfcFleetIds' - A unique identifier for a fleet(s) to retrieve capacity information for. You can use either the fleet ID or ARN value.
describeFleetCapacity ::
  DescribeFleetCapacity
describeFleetCapacity =
  DescribeFleetCapacity'
    { _dfcNextToken = Nothing,
      _dfcLimit = Nothing,
      _dfcFleetIds = Nothing
    }

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
dfcNextToken :: Lens' DescribeFleetCapacity (Maybe Text)
dfcNextToken = lens _dfcNextToken (\s a -> s {_dfcNextToken = a})

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
dfcLimit :: Lens' DescribeFleetCapacity (Maybe Natural)
dfcLimit = lens _dfcLimit (\s a -> s {_dfcLimit = a}) . mapping _Nat

-- | A unique identifier for a fleet(s) to retrieve capacity information for. You can use either the fleet ID or ARN value.
dfcFleetIds :: Lens' DescribeFleetCapacity (Maybe (NonEmpty Text))
dfcFleetIds = lens _dfcFleetIds (\s a -> s {_dfcFleetIds = a}) . mapping _List1

instance AWSPager DescribeFleetCapacity where
  page rq rs
    | stop (rs ^. dfcrsNextToken) = Nothing
    | stop (rs ^. dfcrsFleetCapacity) = Nothing
    | otherwise = Just $ rq & dfcNextToken .~ rs ^. dfcrsNextToken

instance AWSRequest DescribeFleetCapacity where
  type Rs DescribeFleetCapacity = DescribeFleetCapacityResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          DescribeFleetCapacityResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "FleetCapacity" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeFleetCapacity

instance NFData DescribeFleetCapacity

instance ToHeaders DescribeFleetCapacity where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.DescribeFleetCapacity" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeFleetCapacity where
  toJSON DescribeFleetCapacity' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dfcNextToken,
            ("Limit" .=) <$> _dfcLimit,
            ("FleetIds" .=) <$> _dfcFleetIds
          ]
      )

instance ToPath DescribeFleetCapacity where
  toPath = const "/"

instance ToQuery DescribeFleetCapacity where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'describeFleetCapacityResponse' smart constructor.
data DescribeFleetCapacityResponse = DescribeFleetCapacityResponse'
  { _dfcrsNextToken ::
      !(Maybe Text),
    _dfcrsFleetCapacity ::
      !(Maybe [FleetCapacity]),
    _dfcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleetCapacityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcrsNextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- * 'dfcrsFleetCapacity' - A collection of objects containing capacity information for each requested fleet ID. Leave this parameter empty to retrieve capacity information for all fleets.
--
-- * 'dfcrsResponseStatus' - -- | The response status code.
describeFleetCapacityResponse ::
  -- | 'dfcrsResponseStatus'
  Int ->
  DescribeFleetCapacityResponse
describeFleetCapacityResponse pResponseStatus_ =
  DescribeFleetCapacityResponse'
    { _dfcrsNextToken = Nothing,
      _dfcrsFleetCapacity = Nothing,
      _dfcrsResponseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
dfcrsNextToken :: Lens' DescribeFleetCapacityResponse (Maybe Text)
dfcrsNextToken = lens _dfcrsNextToken (\s a -> s {_dfcrsNextToken = a})

-- | A collection of objects containing capacity information for each requested fleet ID. Leave this parameter empty to retrieve capacity information for all fleets.
dfcrsFleetCapacity :: Lens' DescribeFleetCapacityResponse [FleetCapacity]
dfcrsFleetCapacity = lens _dfcrsFleetCapacity (\s a -> s {_dfcrsFleetCapacity = a}) . _Default . _Coerce

-- | -- | The response status code.
dfcrsResponseStatus :: Lens' DescribeFleetCapacityResponse Int
dfcrsResponseStatus = lens _dfcrsResponseStatus (\s a -> s {_dfcrsResponseStatus = a})

instance NFData DescribeFleetCapacityResponse
