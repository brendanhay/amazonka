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
-- Module      : Network.AWS.GameLift.DescribeFleetEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves entries from the specified fleet's event log. You can specify a time range to limit the result set. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a collection of event log entries matching the request are returned.
--
--
-- Fleet-related operations include:
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
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
--
--
module Network.AWS.GameLift.DescribeFleetEvents
    (
    -- * Creating a Request
      describeFleetEvents
    , DescribeFleetEvents
    -- * Request Lenses
    , dfeStartTime
    , dfeNextToken
    , dfeEndTime
    , dfeLimit
    , dfeFleetId

    -- * Destructuring the Response
    , describeFleetEventsResponse
    , DescribeFleetEventsResponse
    -- * Response Lenses
    , dfersNextToken
    , dfersEvents
    , dfersResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'describeFleetEvents' smart constructor.
data DescribeFleetEvents = DescribeFleetEvents'
  { _dfeStartTime :: !(Maybe POSIX)
  , _dfeNextToken :: !(Maybe Text)
  , _dfeEndTime   :: !(Maybe POSIX)
  , _dfeLimit     :: !(Maybe Nat)
  , _dfeFleetId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeStartTime' - Earliest date to retrieve event logs for. If no start time is specified, this call returns entries starting from when the fleet was created to the specified end time. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
--
-- * 'dfeNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'dfeEndTime' - Most recent date to retrieve event logs for. If no end time is specified, this call returns entries from the specified start time up to the present. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
--
-- * 'dfeLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- * 'dfeFleetId' - Unique identifier for a fleet to get event logs for.
describeFleetEvents
    :: Text -- ^ 'dfeFleetId'
    -> DescribeFleetEvents
describeFleetEvents pFleetId_ =
  DescribeFleetEvents'
    { _dfeStartTime = Nothing
    , _dfeNextToken = Nothing
    , _dfeEndTime = Nothing
    , _dfeLimit = Nothing
    , _dfeFleetId = pFleetId_
    }


-- | Earliest date to retrieve event logs for. If no start time is specified, this call returns entries starting from when the fleet was created to the specified end time. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
dfeStartTime :: Lens' DescribeFleetEvents (Maybe UTCTime)
dfeStartTime = lens _dfeStartTime (\ s a -> s{_dfeStartTime = a}) . mapping _Time

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
dfeNextToken :: Lens' DescribeFleetEvents (Maybe Text)
dfeNextToken = lens _dfeNextToken (\ s a -> s{_dfeNextToken = a})

-- | Most recent date to retrieve event logs for. If no end time is specified, this call returns entries from the specified start time up to the present. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
dfeEndTime :: Lens' DescribeFleetEvents (Maybe UTCTime)
dfeEndTime = lens _dfeEndTime (\ s a -> s{_dfeEndTime = a}) . mapping _Time

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
dfeLimit :: Lens' DescribeFleetEvents (Maybe Natural)
dfeLimit = lens _dfeLimit (\ s a -> s{_dfeLimit = a}) . mapping _Nat

-- | Unique identifier for a fleet to get event logs for.
dfeFleetId :: Lens' DescribeFleetEvents Text
dfeFleetId = lens _dfeFleetId (\ s a -> s{_dfeFleetId = a})

instance AWSRequest DescribeFleetEvents where
        type Rs DescribeFleetEvents =
             DescribeFleetEventsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFleetEventsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Events" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFleetEvents where

instance NFData DescribeFleetEvents where

instance ToHeaders DescribeFleetEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeFleetEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFleetEvents where
        toJSON DescribeFleetEvents'{..}
          = object
              (catMaybes
                 [("StartTime" .=) <$> _dfeStartTime,
                  ("NextToken" .=) <$> _dfeNextToken,
                  ("EndTime" .=) <$> _dfeEndTime,
                  ("Limit" .=) <$> _dfeLimit,
                  Just ("FleetId" .= _dfeFleetId)])

instance ToPath DescribeFleetEvents where
        toPath = const "/"

instance ToQuery DescribeFleetEvents where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeFleetEventsResponse' smart constructor.
data DescribeFleetEventsResponse = DescribeFleetEventsResponse'
  { _dfersNextToken      :: !(Maybe Text)
  , _dfersEvents         :: !(Maybe [Event])
  , _dfersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfersNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dfersEvents' - Collection of objects containing event log entries for the specified fleet.
--
-- * 'dfersResponseStatus' - -- | The response status code.
describeFleetEventsResponse
    :: Int -- ^ 'dfersResponseStatus'
    -> DescribeFleetEventsResponse
describeFleetEventsResponse pResponseStatus_ =
  DescribeFleetEventsResponse'
    { _dfersNextToken = Nothing
    , _dfersEvents = Nothing
    , _dfersResponseStatus = pResponseStatus_
    }


-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dfersNextToken :: Lens' DescribeFleetEventsResponse (Maybe Text)
dfersNextToken = lens _dfersNextToken (\ s a -> s{_dfersNextToken = a})

-- | Collection of objects containing event log entries for the specified fleet.
dfersEvents :: Lens' DescribeFleetEventsResponse [Event]
dfersEvents = lens _dfersEvents (\ s a -> s{_dfersEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
dfersResponseStatus :: Lens' DescribeFleetEventsResponse Int
dfersResponseStatus = lens _dfersResponseStatus (\ s a -> s{_dfersResponseStatus = a})

instance NFData DescribeFleetEventsResponse where
