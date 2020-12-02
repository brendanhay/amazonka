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
-- Module      : Network.AWS.MediaLive.DescribeSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a channel schedule
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.DescribeSchedule
  ( -- * Creating a Request
    describeSchedule,
    DescribeSchedule,

    -- * Request Lenses
    dNextToken,
    dMaxResults,
    dChannelId,

    -- * Destructuring the Response
    describeScheduleResponse,
    DescribeScheduleResponse,

    -- * Response Lenses
    dssrsNextToken,
    dssrsScheduleActions,
    dssrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeScheduleRequest
--
-- /See:/ 'describeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'
  { _dNextToken ::
      !(Maybe Text),
    _dMaxResults :: !(Maybe Nat),
    _dChannelId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNextToken' - Undocumented member.
--
-- * 'dMaxResults' - Undocumented member.
--
-- * 'dChannelId' - Id of the channel whose schedule is being updated.
describeSchedule ::
  -- | 'dChannelId'
  Text ->
  DescribeSchedule
describeSchedule pChannelId_ =
  DescribeSchedule'
    { _dNextToken = Nothing,
      _dMaxResults = Nothing,
      _dChannelId = pChannelId_
    }

-- | Undocumented member.
dNextToken :: Lens' DescribeSchedule (Maybe Text)
dNextToken = lens _dNextToken (\s a -> s {_dNextToken = a})

-- | Undocumented member.
dMaxResults :: Lens' DescribeSchedule (Maybe Natural)
dMaxResults = lens _dMaxResults (\s a -> s {_dMaxResults = a}) . mapping _Nat

-- | Id of the channel whose schedule is being updated.
dChannelId :: Lens' DescribeSchedule Text
dChannelId = lens _dChannelId (\s a -> s {_dChannelId = a})

instance AWSPager DescribeSchedule where
  page rq rs
    | stop (rs ^. dssrsNextToken) = Nothing
    | stop (rs ^. dssrsScheduleActions) = Nothing
    | otherwise = Just $ rq & dNextToken .~ rs ^. dssrsNextToken

instance AWSRequest DescribeSchedule where
  type Rs DescribeSchedule = DescribeScheduleResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DescribeScheduleResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "scheduleActions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeSchedule

instance NFData DescribeSchedule

instance ToHeaders DescribeSchedule where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeSchedule where
  toPath DescribeSchedule' {..} =
    mconcat ["/prod/channels/", toBS _dChannelId, "/schedule"]

instance ToQuery DescribeSchedule where
  toQuery DescribeSchedule' {..} =
    mconcat
      ["nextToken" =: _dNextToken, "maxResults" =: _dMaxResults]

-- | Placeholder documentation for DescribeScheduleResponse
--
-- /See:/ 'describeScheduleResponse' smart constructor.
data DescribeScheduleResponse = DescribeScheduleResponse'
  { _dssrsNextToken ::
      !(Maybe Text),
    _dssrsScheduleActions ::
      !(Maybe [ScheduleAction]),
    _dssrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsNextToken' - The next token; for use in pagination.
--
-- * 'dssrsScheduleActions' - The list of actions in the schedule.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeScheduleResponse ::
  -- | 'dssrsResponseStatus'
  Int ->
  DescribeScheduleResponse
describeScheduleResponse pResponseStatus_ =
  DescribeScheduleResponse'
    { _dssrsNextToken = Nothing,
      _dssrsScheduleActions = Nothing,
      _dssrsResponseStatus = pResponseStatus_
    }

-- | The next token; for use in pagination.
dssrsNextToken :: Lens' DescribeScheduleResponse (Maybe Text)
dssrsNextToken = lens _dssrsNextToken (\s a -> s {_dssrsNextToken = a})

-- | The list of actions in the schedule.
dssrsScheduleActions :: Lens' DescribeScheduleResponse [ScheduleAction]
dssrsScheduleActions = lens _dssrsScheduleActions (\s a -> s {_dssrsScheduleActions = a}) . _Default . _Coerce

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeScheduleResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\s a -> s {_dssrsResponseStatus = a})

instance NFData DescribeScheduleResponse
