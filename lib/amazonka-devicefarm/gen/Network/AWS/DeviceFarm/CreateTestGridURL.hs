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
-- Module      : Network.AWS.DeviceFarm.CreateTestGridURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signed, short-term URL that can be passed to a Selenium @RemoteWebDriver@ constructor.
module Network.AWS.DeviceFarm.CreateTestGridURL
  ( -- * Creating a Request
    createTestGridURL,
    CreateTestGridURL,

    -- * Request Lenses
    ctguProjectARN,
    ctguExpiresInSeconds,

    -- * Destructuring the Response
    createTestGridURLResponse,
    CreateTestGridURLResponse,

    -- * Response Lenses
    ctgursExpires,
    ctgursUrl,
    ctgursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTestGridURL' smart constructor.
data CreateTestGridURL = CreateTestGridURL'
  { _ctguProjectARN ::
      !Text,
    _ctguExpiresInSeconds :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTestGridURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctguProjectARN' - ARN (from 'CreateTestGridProject' or 'ListTestGridProjects' ) to associate with the short-term URL.
--
-- * 'ctguExpiresInSeconds' - Lifetime, in seconds, of the URL.
createTestGridURL ::
  -- | 'ctguProjectARN'
  Text ->
  -- | 'ctguExpiresInSeconds'
  Natural ->
  CreateTestGridURL
createTestGridURL pProjectARN_ pExpiresInSeconds_ =
  CreateTestGridURL'
    { _ctguProjectARN = pProjectARN_,
      _ctguExpiresInSeconds = _Nat # pExpiresInSeconds_
    }

-- | ARN (from 'CreateTestGridProject' or 'ListTestGridProjects' ) to associate with the short-term URL.
ctguProjectARN :: Lens' CreateTestGridURL Text
ctguProjectARN = lens _ctguProjectARN (\s a -> s {_ctguProjectARN = a})

-- | Lifetime, in seconds, of the URL.
ctguExpiresInSeconds :: Lens' CreateTestGridURL Natural
ctguExpiresInSeconds = lens _ctguExpiresInSeconds (\s a -> s {_ctguExpiresInSeconds = a}) . _Nat

instance AWSRequest CreateTestGridURL where
  type Rs CreateTestGridURL = CreateTestGridURLResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          CreateTestGridURLResponse'
            <$> (x .?> "expires") <*> (x .?> "url") <*> (pure (fromEnum s))
      )

instance Hashable CreateTestGridURL

instance NFData CreateTestGridURL

instance ToHeaders CreateTestGridURL where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.CreateTestGridUrl" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTestGridURL where
  toJSON CreateTestGridURL' {..} =
    object
      ( catMaybes
          [ Just ("projectArn" .= _ctguProjectARN),
            Just ("expiresInSeconds" .= _ctguExpiresInSeconds)
          ]
      )

instance ToPath CreateTestGridURL where
  toPath = const "/"

instance ToQuery CreateTestGridURL where
  toQuery = const mempty

-- | /See:/ 'createTestGridURLResponse' smart constructor.
data CreateTestGridURLResponse = CreateTestGridURLResponse'
  { _ctgursExpires ::
      !(Maybe POSIX),
    _ctgursUrl :: !(Maybe Text),
    _ctgursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTestGridURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgursExpires' - The number of seconds the URL from 'CreateTestGridUrlResult$url' stays active.
--
-- * 'ctgursUrl' - A signed URL, expiring in 'CreateTestGridUrlRequest$expiresInSeconds' seconds, to be passed to a @RemoteWebDriver@ .
--
-- * 'ctgursResponseStatus' - -- | The response status code.
createTestGridURLResponse ::
  -- | 'ctgursResponseStatus'
  Int ->
  CreateTestGridURLResponse
createTestGridURLResponse pResponseStatus_ =
  CreateTestGridURLResponse'
    { _ctgursExpires = Nothing,
      _ctgursUrl = Nothing,
      _ctgursResponseStatus = pResponseStatus_
    }

-- | The number of seconds the URL from 'CreateTestGridUrlResult$url' stays active.
ctgursExpires :: Lens' CreateTestGridURLResponse (Maybe UTCTime)
ctgursExpires = lens _ctgursExpires (\s a -> s {_ctgursExpires = a}) . mapping _Time

-- | A signed URL, expiring in 'CreateTestGridUrlRequest$expiresInSeconds' seconds, to be passed to a @RemoteWebDriver@ .
ctgursUrl :: Lens' CreateTestGridURLResponse (Maybe Text)
ctgursUrl = lens _ctgursUrl (\s a -> s {_ctgursUrl = a})

-- | -- | The response status code.
ctgursResponseStatus :: Lens' CreateTestGridURLResponse Int
ctgursResponseStatus = lens _ctgursResponseStatus (\s a -> s {_ctgursResponseStatus = a})

instance NFData CreateTestGridURLResponse
