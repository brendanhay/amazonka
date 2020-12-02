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
-- Module      : Network.AWS.DirectConnect.StartBGPFailoverTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the virtual interface failover test that verifies your configuration meets your resiliency requirements by placing the BGP peering session in the DOWN state. You can then send traffic to verify that there are no outages.
--
--
-- You can run the test on public, private, transit, and hosted virtual interfaces.
--
-- You can use <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_ListVirtualInterfaceTestHistory.html ListVirtualInterfaceTestHistory> to view the virtual interface test history.
--
-- If you need to stop the test before the test interval completes, use <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_StopBgpFailoverTest.html StopBgpFailoverTest> .
module Network.AWS.DirectConnect.StartBGPFailoverTest
  ( -- * Creating a Request
    startBGPFailoverTest,
    StartBGPFailoverTest,

    -- * Request Lenses
    sbftBgpPeers,
    sbftTestDurationInMinutes,
    sbftVirtualInterfaceId,

    -- * Destructuring the Response
    startBGPFailoverTestResponse,
    StartBGPFailoverTestResponse,

    -- * Response Lenses
    sbftrsVirtualInterfaceTest,
    sbftrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startBGPFailoverTest' smart constructor.
data StartBGPFailoverTest = StartBGPFailoverTest'
  { _sbftBgpPeers ::
      !(Maybe [Text]),
    _sbftTestDurationInMinutes :: !(Maybe Int),
    _sbftVirtualInterfaceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartBGPFailoverTest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbftBgpPeers' - The BGP peers to place in the DOWN state.
--
-- * 'sbftTestDurationInMinutes' - The time in minutes that the virtual interface failover test will last. Maximum value: 180 minutes (3 hours). Default: 180 minutes (3 hours).
--
-- * 'sbftVirtualInterfaceId' - The ID of the virtual interface you want to test.
startBGPFailoverTest ::
  -- | 'sbftVirtualInterfaceId'
  Text ->
  StartBGPFailoverTest
startBGPFailoverTest pVirtualInterfaceId_ =
  StartBGPFailoverTest'
    { _sbftBgpPeers = Nothing,
      _sbftTestDurationInMinutes = Nothing,
      _sbftVirtualInterfaceId = pVirtualInterfaceId_
    }

-- | The BGP peers to place in the DOWN state.
sbftBgpPeers :: Lens' StartBGPFailoverTest [Text]
sbftBgpPeers = lens _sbftBgpPeers (\s a -> s {_sbftBgpPeers = a}) . _Default . _Coerce

-- | The time in minutes that the virtual interface failover test will last. Maximum value: 180 minutes (3 hours). Default: 180 minutes (3 hours).
sbftTestDurationInMinutes :: Lens' StartBGPFailoverTest (Maybe Int)
sbftTestDurationInMinutes = lens _sbftTestDurationInMinutes (\s a -> s {_sbftTestDurationInMinutes = a})

-- | The ID of the virtual interface you want to test.
sbftVirtualInterfaceId :: Lens' StartBGPFailoverTest Text
sbftVirtualInterfaceId = lens _sbftVirtualInterfaceId (\s a -> s {_sbftVirtualInterfaceId = a})

instance AWSRequest StartBGPFailoverTest where
  type Rs StartBGPFailoverTest = StartBGPFailoverTestResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          StartBGPFailoverTestResponse'
            <$> (x .?> "virtualInterfaceTest") <*> (pure (fromEnum s))
      )

instance Hashable StartBGPFailoverTest

instance NFData StartBGPFailoverTest

instance ToHeaders StartBGPFailoverTest where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.StartBgpFailoverTest" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartBGPFailoverTest where
  toJSON StartBGPFailoverTest' {..} =
    object
      ( catMaybes
          [ ("bgpPeers" .=) <$> _sbftBgpPeers,
            ("testDurationInMinutes" .=) <$> _sbftTestDurationInMinutes,
            Just ("virtualInterfaceId" .= _sbftVirtualInterfaceId)
          ]
      )

instance ToPath StartBGPFailoverTest where
  toPath = const "/"

instance ToQuery StartBGPFailoverTest where
  toQuery = const mempty

-- | /See:/ 'startBGPFailoverTestResponse' smart constructor.
data StartBGPFailoverTestResponse = StartBGPFailoverTestResponse'
  { _sbftrsVirtualInterfaceTest ::
      !( Maybe
           VirtualInterfaceTestHistory
       ),
    _sbftrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartBGPFailoverTestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbftrsVirtualInterfaceTest' - Information about the virtual interface failover test.
--
-- * 'sbftrsResponseStatus' - -- | The response status code.
startBGPFailoverTestResponse ::
  -- | 'sbftrsResponseStatus'
  Int ->
  StartBGPFailoverTestResponse
startBGPFailoverTestResponse pResponseStatus_ =
  StartBGPFailoverTestResponse'
    { _sbftrsVirtualInterfaceTest =
        Nothing,
      _sbftrsResponseStatus = pResponseStatus_
    }

-- | Information about the virtual interface failover test.
sbftrsVirtualInterfaceTest :: Lens' StartBGPFailoverTestResponse (Maybe VirtualInterfaceTestHistory)
sbftrsVirtualInterfaceTest = lens _sbftrsVirtualInterfaceTest (\s a -> s {_sbftrsVirtualInterfaceTest = a})

-- | -- | The response status code.
sbftrsResponseStatus :: Lens' StartBGPFailoverTestResponse Int
sbftrsResponseStatus = lens _sbftrsResponseStatus (\s a -> s {_sbftrsResponseStatus = a})

instance NFData StartBGPFailoverTestResponse
