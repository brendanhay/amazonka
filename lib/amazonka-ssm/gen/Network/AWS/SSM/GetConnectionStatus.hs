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
-- Module      : Network.AWS.SSM.GetConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Session Manager connection status for an instance to determine whether it is running and ready to receive Session Manager connections.
module Network.AWS.SSM.GetConnectionStatus
  ( -- * Creating a Request
    getConnectionStatus,
    GetConnectionStatus,

    -- * Request Lenses
    gcsTarget,

    -- * Destructuring the Response
    getConnectionStatusResponse,
    GetConnectionStatusResponse,

    -- * Response Lenses
    gcsrsStatus,
    gcsrsTarget,
    gcsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'getConnectionStatus' smart constructor.
newtype GetConnectionStatus = GetConnectionStatus'
  { _gcsTarget ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsTarget' - The ID of the instance.
getConnectionStatus ::
  -- | 'gcsTarget'
  Text ->
  GetConnectionStatus
getConnectionStatus pTarget_ =
  GetConnectionStatus' {_gcsTarget = pTarget_}

-- | The ID of the instance.
gcsTarget :: Lens' GetConnectionStatus Text
gcsTarget = lens _gcsTarget (\s a -> s {_gcsTarget = a})

instance AWSRequest GetConnectionStatus where
  type Rs GetConnectionStatus = GetConnectionStatusResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          GetConnectionStatusResponse'
            <$> (x .?> "Status") <*> (x .?> "Target") <*> (pure (fromEnum s))
      )

instance Hashable GetConnectionStatus

instance NFData GetConnectionStatus

instance ToHeaders GetConnectionStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.GetConnectionStatus" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetConnectionStatus where
  toJSON GetConnectionStatus' {..} =
    object (catMaybes [Just ("Target" .= _gcsTarget)])

instance ToPath GetConnectionStatus where
  toPath = const "/"

instance ToQuery GetConnectionStatus where
  toQuery = const mempty

-- | /See:/ 'getConnectionStatusResponse' smart constructor.
data GetConnectionStatusResponse = GetConnectionStatusResponse'
  { _gcsrsStatus ::
      !(Maybe ConnectionStatus),
    _gcsrsTarget :: !(Maybe Text),
    _gcsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectionStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsrsStatus' - The status of the connection to the instance. For example, 'Connected' or 'Not Connected'.
--
-- * 'gcsrsTarget' - The ID of the instance to check connection status.
--
-- * 'gcsrsResponseStatus' - -- | The response status code.
getConnectionStatusResponse ::
  -- | 'gcsrsResponseStatus'
  Int ->
  GetConnectionStatusResponse
getConnectionStatusResponse pResponseStatus_ =
  GetConnectionStatusResponse'
    { _gcsrsStatus = Nothing,
      _gcsrsTarget = Nothing,
      _gcsrsResponseStatus = pResponseStatus_
    }

-- | The status of the connection to the instance. For example, 'Connected' or 'Not Connected'.
gcsrsStatus :: Lens' GetConnectionStatusResponse (Maybe ConnectionStatus)
gcsrsStatus = lens _gcsrsStatus (\s a -> s {_gcsrsStatus = a})

-- | The ID of the instance to check connection status.
gcsrsTarget :: Lens' GetConnectionStatusResponse (Maybe Text)
gcsrsTarget = lens _gcsrsTarget (\s a -> s {_gcsrsTarget = a})

-- | -- | The response status code.
gcsrsResponseStatus :: Lens' GetConnectionStatusResponse Int
gcsrsResponseStatus = lens _gcsrsResponseStatus (\s a -> s {_gcsrsResponseStatus = a})

instance NFData GetConnectionStatusResponse
