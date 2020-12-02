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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about upcoming executions of a maintenance window.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowSchedule
  ( -- * Creating a Request
    describeMaintenanceWindowSchedule,
    DescribeMaintenanceWindowSchedule,

    -- * Request Lenses
    dmwsResourceType,
    dmwsFilters,
    dmwsNextToken,
    dmwsTargets,
    dmwsMaxResults,
    dmwsWindowId,

    -- * Destructuring the Response
    describeMaintenanceWindowScheduleResponse,
    DescribeMaintenanceWindowScheduleResponse,

    -- * Response Lenses
    dmwsrsScheduledWindowExecutions,
    dmwsrsNextToken,
    dmwsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'describeMaintenanceWindowSchedule' smart constructor.
data DescribeMaintenanceWindowSchedule = DescribeMaintenanceWindowSchedule'
  { _dmwsResourceType ::
      !( Maybe
           MaintenanceWindowResourceType
       ),
    _dmwsFilters ::
      !( Maybe
           [PatchOrchestratorFilter]
       ),
    _dmwsNextToken ::
      !(Maybe Text),
    _dmwsTargets ::
      !(Maybe [Target]),
    _dmwsMaxResults ::
      !(Maybe Nat),
    _dmwsWindowId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMaintenanceWindowSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwsResourceType' - The type of resource you want to retrieve information about. For example, "INSTANCE".
--
-- * 'dmwsFilters' - Filters used to limit the range of results. For example, you can limit maintenance window executions to only those scheduled before or after a certain date and time.
--
-- * 'dmwsNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmwsTargets' - The instance ID or key/value pair to retrieve information about.
--
-- * 'dmwsMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dmwsWindowId' - The ID of the maintenance window to retrieve information about.
describeMaintenanceWindowSchedule ::
  DescribeMaintenanceWindowSchedule
describeMaintenanceWindowSchedule =
  DescribeMaintenanceWindowSchedule'
    { _dmwsResourceType = Nothing,
      _dmwsFilters = Nothing,
      _dmwsNextToken = Nothing,
      _dmwsTargets = Nothing,
      _dmwsMaxResults = Nothing,
      _dmwsWindowId = Nothing
    }

-- | The type of resource you want to retrieve information about. For example, "INSTANCE".
dmwsResourceType :: Lens' DescribeMaintenanceWindowSchedule (Maybe MaintenanceWindowResourceType)
dmwsResourceType = lens _dmwsResourceType (\s a -> s {_dmwsResourceType = a})

-- | Filters used to limit the range of results. For example, you can limit maintenance window executions to only those scheduled before or after a certain date and time.
dmwsFilters :: Lens' DescribeMaintenanceWindowSchedule [PatchOrchestratorFilter]
dmwsFilters = lens _dmwsFilters (\s a -> s {_dmwsFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmwsNextToken :: Lens' DescribeMaintenanceWindowSchedule (Maybe Text)
dmwsNextToken = lens _dmwsNextToken (\s a -> s {_dmwsNextToken = a})

-- | The instance ID or key/value pair to retrieve information about.
dmwsTargets :: Lens' DescribeMaintenanceWindowSchedule [Target]
dmwsTargets = lens _dmwsTargets (\s a -> s {_dmwsTargets = a}) . _Default . _Coerce

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dmwsMaxResults :: Lens' DescribeMaintenanceWindowSchedule (Maybe Natural)
dmwsMaxResults = lens _dmwsMaxResults (\s a -> s {_dmwsMaxResults = a}) . mapping _Nat

-- | The ID of the maintenance window to retrieve information about.
dmwsWindowId :: Lens' DescribeMaintenanceWindowSchedule (Maybe Text)
dmwsWindowId = lens _dmwsWindowId (\s a -> s {_dmwsWindowId = a})

instance AWSPager DescribeMaintenanceWindowSchedule where
  page rq rs
    | stop (rs ^. dmwsrsNextToken) = Nothing
    | stop (rs ^. dmwsrsScheduledWindowExecutions) = Nothing
    | otherwise = Just $ rq & dmwsNextToken .~ rs ^. dmwsrsNextToken

instance AWSRequest DescribeMaintenanceWindowSchedule where
  type
    Rs DescribeMaintenanceWindowSchedule =
      DescribeMaintenanceWindowScheduleResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowScheduleResponse'
            <$> (x .?> "ScheduledWindowExecutions" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeMaintenanceWindowSchedule

instance NFData DescribeMaintenanceWindowSchedule

instance ToHeaders DescribeMaintenanceWindowSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.DescribeMaintenanceWindowSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeMaintenanceWindowSchedule where
  toJSON DescribeMaintenanceWindowSchedule' {..} =
    object
      ( catMaybes
          [ ("ResourceType" .=) <$> _dmwsResourceType,
            ("Filters" .=) <$> _dmwsFilters,
            ("NextToken" .=) <$> _dmwsNextToken,
            ("Targets" .=) <$> _dmwsTargets,
            ("MaxResults" .=) <$> _dmwsMaxResults,
            ("WindowId" .=) <$> _dmwsWindowId
          ]
      )

instance ToPath DescribeMaintenanceWindowSchedule where
  toPath = const "/"

instance ToQuery DescribeMaintenanceWindowSchedule where
  toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowScheduleResponse' smart constructor.
data DescribeMaintenanceWindowScheduleResponse = DescribeMaintenanceWindowScheduleResponse'
  { _dmwsrsScheduledWindowExecutions ::
      !( Maybe
           [ScheduledWindowExecution]
       ),
    _dmwsrsNextToken ::
      !( Maybe
           Text
       ),
    _dmwsrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeMaintenanceWindowScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwsrsScheduledWindowExecutions' - Information about maintenance window executions scheduled for the specified time range.
--
-- * 'dmwsrsNextToken' - The token for the next set of items to return. (You use this token in the next call.)
--
-- * 'dmwsrsResponseStatus' - -- | The response status code.
describeMaintenanceWindowScheduleResponse ::
  -- | 'dmwsrsResponseStatus'
  Int ->
  DescribeMaintenanceWindowScheduleResponse
describeMaintenanceWindowScheduleResponse pResponseStatus_ =
  DescribeMaintenanceWindowScheduleResponse'
    { _dmwsrsScheduledWindowExecutions =
        Nothing,
      _dmwsrsNextToken = Nothing,
      _dmwsrsResponseStatus = pResponseStatus_
    }

-- | Information about maintenance window executions scheduled for the specified time range.
dmwsrsScheduledWindowExecutions :: Lens' DescribeMaintenanceWindowScheduleResponse [ScheduledWindowExecution]
dmwsrsScheduledWindowExecutions = lens _dmwsrsScheduledWindowExecutions (\s a -> s {_dmwsrsScheduledWindowExecutions = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You use this token in the next call.)
dmwsrsNextToken :: Lens' DescribeMaintenanceWindowScheduleResponse (Maybe Text)
dmwsrsNextToken = lens _dmwsrsNextToken (\s a -> s {_dmwsrsNextToken = a})

-- | -- | The response status code.
dmwsrsResponseStatus :: Lens' DescribeMaintenanceWindowScheduleResponse Int
dmwsrsResponseStatus = lens _dmwsrsResponseStatus (\s a -> s {_dmwsrsResponseStatus = a})

instance NFData DescribeMaintenanceWindowScheduleResponse
