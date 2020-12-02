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
-- Module      : Network.AWS.StorageGateway.StartAvailabilityMonitorTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start a test that verifies that the specified gateway is configured for High Availability monitoring in your host environment. This request only initiates the test and that a successful response only indicates that the test was started. It doesn't indicate that the test passed. For the status of the test, invoke the @DescribeAvailabilityMonitorTest@ API.
module Network.AWS.StorageGateway.StartAvailabilityMonitorTest
  ( -- * Creating a Request
    startAvailabilityMonitorTest,
    StartAvailabilityMonitorTest,

    -- * Request Lenses
    samtGatewayARN,

    -- * Destructuring the Response
    startAvailabilityMonitorTestResponse,
    StartAvailabilityMonitorTestResponse,

    -- * Response Lenses
    samtrsGatewayARN,
    samtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'startAvailabilityMonitorTest' smart constructor.
newtype StartAvailabilityMonitorTest = StartAvailabilityMonitorTest'
  { _samtGatewayARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartAvailabilityMonitorTest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samtGatewayARN' - Undocumented member.
startAvailabilityMonitorTest ::
  -- | 'samtGatewayARN'
  Text ->
  StartAvailabilityMonitorTest
startAvailabilityMonitorTest pGatewayARN_ =
  StartAvailabilityMonitorTest' {_samtGatewayARN = pGatewayARN_}

-- | Undocumented member.
samtGatewayARN :: Lens' StartAvailabilityMonitorTest Text
samtGatewayARN = lens _samtGatewayARN (\s a -> s {_samtGatewayARN = a})

instance AWSRequest StartAvailabilityMonitorTest where
  type
    Rs StartAvailabilityMonitorTest =
      StartAvailabilityMonitorTestResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          StartAvailabilityMonitorTestResponse'
            <$> (x .?> "GatewayARN") <*> (pure (fromEnum s))
      )

instance Hashable StartAvailabilityMonitorTest

instance NFData StartAvailabilityMonitorTest

instance ToHeaders StartAvailabilityMonitorTest where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.StartAvailabilityMonitorTest" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartAvailabilityMonitorTest where
  toJSON StartAvailabilityMonitorTest' {..} =
    object (catMaybes [Just ("GatewayARN" .= _samtGatewayARN)])

instance ToPath StartAvailabilityMonitorTest where
  toPath = const "/"

instance ToQuery StartAvailabilityMonitorTest where
  toQuery = const mempty

-- | /See:/ 'startAvailabilityMonitorTestResponse' smart constructor.
data StartAvailabilityMonitorTestResponse = StartAvailabilityMonitorTestResponse'
  { _samtrsGatewayARN ::
      !(Maybe Text),
    _samtrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartAvailabilityMonitorTestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samtrsGatewayARN' - Undocumented member.
--
-- * 'samtrsResponseStatus' - -- | The response status code.
startAvailabilityMonitorTestResponse ::
  -- | 'samtrsResponseStatus'
  Int ->
  StartAvailabilityMonitorTestResponse
startAvailabilityMonitorTestResponse pResponseStatus_ =
  StartAvailabilityMonitorTestResponse'
    { _samtrsGatewayARN =
        Nothing,
      _samtrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
samtrsGatewayARN :: Lens' StartAvailabilityMonitorTestResponse (Maybe Text)
samtrsGatewayARN = lens _samtrsGatewayARN (\s a -> s {_samtrsGatewayARN = a})

-- | -- | The response status code.
samtrsResponseStatus :: Lens' StartAvailabilityMonitorTestResponse Int
samtrsResponseStatus = lens _samtrsResponseStatus (\s a -> s {_samtrsResponseStatus = a})

instance NFData StartAvailabilityMonitorTestResponse
