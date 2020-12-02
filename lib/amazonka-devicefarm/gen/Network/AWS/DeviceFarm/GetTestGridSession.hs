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
-- Module      : Network.AWS.DeviceFarm.GetTestGridSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A session is an instance of a browser created through a @RemoteWebDriver@ with the URL from 'CreateTestGridUrlResult$url' . You can use the following to look up sessions:
--
--
--     * The session ARN ('GetTestGridSessionRequest$sessionArn' ).
--
--     * The project ARN and a session ID ('GetTestGridSessionRequest$projectArn' and 'GetTestGridSessionRequest$sessionId' ).
module Network.AWS.DeviceFarm.GetTestGridSession
  ( -- * Creating a Request
    getTestGridSession,
    GetTestGridSession,

    -- * Request Lenses
    gtgsSessionARN,
    gtgsProjectARN,
    gtgsSessionId,

    -- * Destructuring the Response
    getTestGridSessionResponse,
    GetTestGridSessionResponse,

    -- * Response Lenses
    gtgsrsTestGridSession,
    gtgsrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTestGridSession' smart constructor.
data GetTestGridSession = GetTestGridSession'
  { _gtgsSessionARN ::
      !(Maybe Text),
    _gtgsProjectARN :: !(Maybe Text),
    _gtgsSessionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTestGridSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgsSessionARN' - An ARN that uniquely identifies a 'TestGridSession' .
--
-- * 'gtgsProjectARN' - The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
--
-- * 'gtgsSessionId' - An ID associated with this session.
getTestGridSession ::
  GetTestGridSession
getTestGridSession =
  GetTestGridSession'
    { _gtgsSessionARN = Nothing,
      _gtgsProjectARN = Nothing,
      _gtgsSessionId = Nothing
    }

-- | An ARN that uniquely identifies a 'TestGridSession' .
gtgsSessionARN :: Lens' GetTestGridSession (Maybe Text)
gtgsSessionARN = lens _gtgsSessionARN (\s a -> s {_gtgsSessionARN = a})

-- | The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
gtgsProjectARN :: Lens' GetTestGridSession (Maybe Text)
gtgsProjectARN = lens _gtgsProjectARN (\s a -> s {_gtgsProjectARN = a})

-- | An ID associated with this session.
gtgsSessionId :: Lens' GetTestGridSession (Maybe Text)
gtgsSessionId = lens _gtgsSessionId (\s a -> s {_gtgsSessionId = a})

instance AWSRequest GetTestGridSession where
  type Rs GetTestGridSession = GetTestGridSessionResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          GetTestGridSessionResponse'
            <$> (x .?> "testGridSession") <*> (pure (fromEnum s))
      )

instance Hashable GetTestGridSession

instance NFData GetTestGridSession

instance ToHeaders GetTestGridSession where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.GetTestGridSession" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTestGridSession where
  toJSON GetTestGridSession' {..} =
    object
      ( catMaybes
          [ ("sessionArn" .=) <$> _gtgsSessionARN,
            ("projectArn" .=) <$> _gtgsProjectARN,
            ("sessionId" .=) <$> _gtgsSessionId
          ]
      )

instance ToPath GetTestGridSession where
  toPath = const "/"

instance ToQuery GetTestGridSession where
  toQuery = const mempty

-- | /See:/ 'getTestGridSessionResponse' smart constructor.
data GetTestGridSessionResponse = GetTestGridSessionResponse'
  { _gtgsrsTestGridSession ::
      !(Maybe TestGridSession),
    _gtgsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTestGridSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgsrsTestGridSession' - The 'TestGridSession' that was requested.
--
-- * 'gtgsrsResponseStatus' - -- | The response status code.
getTestGridSessionResponse ::
  -- | 'gtgsrsResponseStatus'
  Int ->
  GetTestGridSessionResponse
getTestGridSessionResponse pResponseStatus_ =
  GetTestGridSessionResponse'
    { _gtgsrsTestGridSession = Nothing,
      _gtgsrsResponseStatus = pResponseStatus_
    }

-- | The 'TestGridSession' that was requested.
gtgsrsTestGridSession :: Lens' GetTestGridSessionResponse (Maybe TestGridSession)
gtgsrsTestGridSession = lens _gtgsrsTestGridSession (\s a -> s {_gtgsrsTestGridSession = a})

-- | -- | The response status code.
gtgsrsResponseStatus :: Lens' GetTestGridSessionResponse Int
gtgsrsResponseStatus = lens _gtgsrsResponseStatus (\s a -> s {_gtgsrsResponseStatus = a})

instance NFData GetTestGridSessionResponse
