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
-- Module      : Network.AWS.SSM.GetCalendarState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the state of the AWS Systems Manager Change Calendar at an optional, specified time. If you specify a time, @GetCalendarState@ returns the state of the calendar at a specific time, and returns the next time that the Change Calendar state will transition. If you do not specify a time, @GetCalendarState@ assumes the current time. Change Calendar entries have two possible states: @OPEN@ or @CLOSED@ .
--
--
-- If you specify more than one calendar in a request, the command returns the status of @OPEN@ only if all calendars in the request are open. If one or more calendars in the request are closed, the status returned is @CLOSED@ .
--
-- For more information about Systems Manager Change Calendar, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar.html AWS Systems Manager Change Calendar> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.GetCalendarState
  ( -- * Creating a Request
    getCalendarState,
    GetCalendarState,

    -- * Request Lenses
    gcsAtTime,
    gcsCalendarNames,

    -- * Destructuring the Response
    getCalendarStateResponse,
    GetCalendarStateResponse,

    -- * Response Lenses
    getrsState,
    getrsNextTransitionTime,
    getrsAtTime,
    getrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'getCalendarState' smart constructor.
data GetCalendarState = GetCalendarState'
  { _gcsAtTime ::
      !(Maybe Text),
    _gcsCalendarNames :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCalendarState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsAtTime' - (Optional) The specific time for which you want to get calendar state information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> format. If you do not add @AtTime@ , the current time is assumed.
--
-- * 'gcsCalendarNames' - The names or Amazon Resource Names (ARNs) of the Systems Manager documents that represent the calendar entries for which you want to get the state.
getCalendarState ::
  GetCalendarState
getCalendarState =
  GetCalendarState'
    { _gcsAtTime = Nothing,
      _gcsCalendarNames = mempty
    }

-- | (Optional) The specific time for which you want to get calendar state information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> format. If you do not add @AtTime@ , the current time is assumed.
gcsAtTime :: Lens' GetCalendarState (Maybe Text)
gcsAtTime = lens _gcsAtTime (\s a -> s {_gcsAtTime = a})

-- | The names or Amazon Resource Names (ARNs) of the Systems Manager documents that represent the calendar entries for which you want to get the state.
gcsCalendarNames :: Lens' GetCalendarState [Text]
gcsCalendarNames = lens _gcsCalendarNames (\s a -> s {_gcsCalendarNames = a}) . _Coerce

instance AWSRequest GetCalendarState where
  type Rs GetCalendarState = GetCalendarStateResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          GetCalendarStateResponse'
            <$> (x .?> "State")
            <*> (x .?> "NextTransitionTime")
            <*> (x .?> "AtTime")
            <*> (pure (fromEnum s))
      )

instance Hashable GetCalendarState

instance NFData GetCalendarState

instance ToHeaders GetCalendarState where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.GetCalendarState" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetCalendarState where
  toJSON GetCalendarState' {..} =
    object
      ( catMaybes
          [ ("AtTime" .=) <$> _gcsAtTime,
            Just ("CalendarNames" .= _gcsCalendarNames)
          ]
      )

instance ToPath GetCalendarState where
  toPath = const "/"

instance ToQuery GetCalendarState where
  toQuery = const mempty

-- | /See:/ 'getCalendarStateResponse' smart constructor.
data GetCalendarStateResponse = GetCalendarStateResponse'
  { _getrsState ::
      !(Maybe CalendarState),
    _getrsNextTransitionTime :: !(Maybe Text),
    _getrsAtTime :: !(Maybe Text),
    _getrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCalendarStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsState' - The state of the calendar. An @OPEN@ calendar indicates that actions are allowed to proceed, and a @CLOSED@ calendar indicates that actions are not allowed to proceed.
--
-- * 'getrsNextTransitionTime' - The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that the calendar state will change. If the current calendar state is @OPEN@ , @NextTransitionTime@ indicates when the calendar state changes to @CLOSED@ , and vice-versa.
--
-- * 'getrsAtTime' - The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that you specified in your command. If you did not specify a time, @GetCalendarState@ uses the current time.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getCalendarStateResponse ::
  -- | 'getrsResponseStatus'
  Int ->
  GetCalendarStateResponse
getCalendarStateResponse pResponseStatus_ =
  GetCalendarStateResponse'
    { _getrsState = Nothing,
      _getrsNextTransitionTime = Nothing,
      _getrsAtTime = Nothing,
      _getrsResponseStatus = pResponseStatus_
    }

-- | The state of the calendar. An @OPEN@ calendar indicates that actions are allowed to proceed, and a @CLOSED@ calendar indicates that actions are not allowed to proceed.
getrsState :: Lens' GetCalendarStateResponse (Maybe CalendarState)
getrsState = lens _getrsState (\s a -> s {_getrsState = a})

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that the calendar state will change. If the current calendar state is @OPEN@ , @NextTransitionTime@ indicates when the calendar state changes to @CLOSED@ , and vice-versa.
getrsNextTransitionTime :: Lens' GetCalendarStateResponse (Maybe Text)
getrsNextTransitionTime = lens _getrsNextTransitionTime (\s a -> s {_getrsNextTransitionTime = a})

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that you specified in your command. If you did not specify a time, @GetCalendarState@ uses the current time.
getrsAtTime :: Lens' GetCalendarStateResponse (Maybe Text)
getrsAtTime = lens _getrsAtTime (\s a -> s {_getrsAtTime = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetCalendarStateResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\s a -> s {_getrsResponseStatus = a})

instance NFData GetCalendarStateResponse
