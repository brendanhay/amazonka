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
-- Module      : Network.AWS.Redshift.DescribeScheduledActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes properties of scheduled actions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeScheduledActions
  ( -- * Creating a Request
    describeScheduledActions,
    DescribeScheduledActions,

    -- * Request Lenses
    dsasStartTime,
    dsasScheduledActionName,
    dsasFilters,
    dsasActive,
    dsasTargetActionType,
    dsasMarker,
    dsasMaxRecords,
    dsasEndTime,

    -- * Destructuring the Response
    describeScheduledActionsResponse,
    DescribeScheduledActionsResponse,

    -- * Response Lenses
    dsarsScheduledActions,
    dsarsMarker,
    dsarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { _dsasStartTime ::
      !(Maybe ISO8601),
    _dsasScheduledActionName :: !(Maybe Text),
    _dsasFilters ::
      !(Maybe [ScheduledActionFilter]),
    _dsasActive :: !(Maybe Bool),
    _dsasTargetActionType ::
      !(Maybe ScheduledActionTypeValues),
    _dsasMarker :: !(Maybe Text),
    _dsasMaxRecords :: !(Maybe Int),
    _dsasEndTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeScheduledActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasStartTime' - The start time in UTC of the scheduled actions to retrieve. Only active scheduled actions that have invocations after this time are retrieved.
--
-- * 'dsasScheduledActionName' - The name of the scheduled action to retrieve.
--
-- * 'dsasFilters' - List of scheduled action filters.
--
-- * 'dsasActive' - If true, retrieve only active scheduled actions. If false, retrieve only disabled scheduled actions.
--
-- * 'dsasTargetActionType' - The type of the scheduled actions to retrieve.
--
-- * 'dsasMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dsasMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
--
-- * 'dsasEndTime' - The end time in UTC of the scheduled action to retrieve. Only active scheduled actions that have invocations before this time are retrieved.
describeScheduledActions ::
  DescribeScheduledActions
describeScheduledActions =
  DescribeScheduledActions'
    { _dsasStartTime = Nothing,
      _dsasScheduledActionName = Nothing,
      _dsasFilters = Nothing,
      _dsasActive = Nothing,
      _dsasTargetActionType = Nothing,
      _dsasMarker = Nothing,
      _dsasMaxRecords = Nothing,
      _dsasEndTime = Nothing
    }

-- | The start time in UTC of the scheduled actions to retrieve. Only active scheduled actions that have invocations after this time are retrieved.
dsasStartTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsasStartTime = lens _dsasStartTime (\s a -> s {_dsasStartTime = a}) . mapping _Time

-- | The name of the scheduled action to retrieve.
dsasScheduledActionName :: Lens' DescribeScheduledActions (Maybe Text)
dsasScheduledActionName = lens _dsasScheduledActionName (\s a -> s {_dsasScheduledActionName = a})

-- | List of scheduled action filters.
dsasFilters :: Lens' DescribeScheduledActions [ScheduledActionFilter]
dsasFilters = lens _dsasFilters (\s a -> s {_dsasFilters = a}) . _Default . _Coerce

-- | If true, retrieve only active scheduled actions. If false, retrieve only disabled scheduled actions.
dsasActive :: Lens' DescribeScheduledActions (Maybe Bool)
dsasActive = lens _dsasActive (\s a -> s {_dsasActive = a})

-- | The type of the scheduled actions to retrieve.
dsasTargetActionType :: Lens' DescribeScheduledActions (Maybe ScheduledActionTypeValues)
dsasTargetActionType = lens _dsasTargetActionType (\s a -> s {_dsasTargetActionType = a})

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dsasMarker :: Lens' DescribeScheduledActions (Maybe Text)
dsasMarker = lens _dsasMarker (\s a -> s {_dsasMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dsasMaxRecords :: Lens' DescribeScheduledActions (Maybe Int)
dsasMaxRecords = lens _dsasMaxRecords (\s a -> s {_dsasMaxRecords = a})

-- | The end time in UTC of the scheduled action to retrieve. Only active scheduled actions that have invocations before this time are retrieved.
dsasEndTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsasEndTime = lens _dsasEndTime (\s a -> s {_dsasEndTime = a}) . mapping _Time

instance AWSPager DescribeScheduledActions where
  page rq rs
    | stop (rs ^. dsarsMarker) = Nothing
    | stop (rs ^. dsarsScheduledActions) = Nothing
    | otherwise = Just $ rq & dsasMarker .~ rs ^. dsarsMarker

instance AWSRequest DescribeScheduledActions where
  type Rs DescribeScheduledActions = DescribeScheduledActionsResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "DescribeScheduledActionsResult"
      ( \s h x ->
          DescribeScheduledActionsResponse'
            <$> ( x .@? "ScheduledActions" .!@ mempty
                    >>= may (parseXMLList "ScheduledAction")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeScheduledActions

instance NFData DescribeScheduledActions

instance ToHeaders DescribeScheduledActions where
  toHeaders = const mempty

instance ToPath DescribeScheduledActions where
  toPath = const "/"

instance ToQuery DescribeScheduledActions where
  toQuery DescribeScheduledActions' {..} =
    mconcat
      [ "Action" =: ("DescribeScheduledActions" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "StartTime" =: _dsasStartTime,
        "ScheduledActionName" =: _dsasScheduledActionName,
        "Filters"
          =: toQuery (toQueryList "ScheduledActionFilter" <$> _dsasFilters),
        "Active" =: _dsasActive,
        "TargetActionType" =: _dsasTargetActionType,
        "Marker" =: _dsasMarker,
        "MaxRecords" =: _dsasMaxRecords,
        "EndTime" =: _dsasEndTime
      ]

-- | /See:/ 'describeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { _dsarsScheduledActions ::
      !( Maybe
           [ScheduledAction]
       ),
    _dsarsMarker ::
      !(Maybe Text),
    _dsarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeScheduledActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsScheduledActions' - List of retrieved scheduled actions.
--
-- * 'dsarsMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dsarsResponseStatus' - -- | The response status code.
describeScheduledActionsResponse ::
  -- | 'dsarsResponseStatus'
  Int ->
  DescribeScheduledActionsResponse
describeScheduledActionsResponse pResponseStatus_ =
  DescribeScheduledActionsResponse'
    { _dsarsScheduledActions =
        Nothing,
      _dsarsMarker = Nothing,
      _dsarsResponseStatus = pResponseStatus_
    }

-- | List of retrieved scheduled actions.
dsarsScheduledActions :: Lens' DescribeScheduledActionsResponse [ScheduledAction]
dsarsScheduledActions = lens _dsarsScheduledActions (\s a -> s {_dsarsScheduledActions = a}) . _Default . _Coerce

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dsarsMarker :: Lens' DescribeScheduledActionsResponse (Maybe Text)
dsarsMarker = lens _dsarsMarker (\s a -> s {_dsarsMarker = a})

-- | -- | The response status code.
dsarsResponseStatus :: Lens' DescribeScheduledActionsResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\s a -> s {_dsarsResponseStatus = a})

instance NFData DescribeScheduledActionsResponse
