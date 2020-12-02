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
-- Module      : Network.AWS.DirectConnect.StopBGPFailoverTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the virtual interface failover test.
module Network.AWS.DirectConnect.StopBGPFailoverTest
  ( -- * Creating a Request
    stopBGPFailoverTest,
    StopBGPFailoverTest,

    -- * Request Lenses
    sbgpftVirtualInterfaceId,

    -- * Destructuring the Response
    stopBGPFailoverTestResponse,
    StopBGPFailoverTestResponse,

    -- * Response Lenses
    sbgpftrsVirtualInterfaceTest,
    sbgpftrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopBGPFailoverTest' smart constructor.
newtype StopBGPFailoverTest = StopBGPFailoverTest'
  { _sbgpftVirtualInterfaceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopBGPFailoverTest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbgpftVirtualInterfaceId' - The ID of the virtual interface you no longer want to test.
stopBGPFailoverTest ::
  -- | 'sbgpftVirtualInterfaceId'
  Text ->
  StopBGPFailoverTest
stopBGPFailoverTest pVirtualInterfaceId_ =
  StopBGPFailoverTest'
    { _sbgpftVirtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface you no longer want to test.
sbgpftVirtualInterfaceId :: Lens' StopBGPFailoverTest Text
sbgpftVirtualInterfaceId = lens _sbgpftVirtualInterfaceId (\s a -> s {_sbgpftVirtualInterfaceId = a})

instance AWSRequest StopBGPFailoverTest where
  type Rs StopBGPFailoverTest = StopBGPFailoverTestResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          StopBGPFailoverTestResponse'
            <$> (x .?> "virtualInterfaceTest") <*> (pure (fromEnum s))
      )

instance Hashable StopBGPFailoverTest

instance NFData StopBGPFailoverTest

instance ToHeaders StopBGPFailoverTest where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.StopBgpFailoverTest" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopBGPFailoverTest where
  toJSON StopBGPFailoverTest' {..} =
    object
      ( catMaybes
          [Just ("virtualInterfaceId" .= _sbgpftVirtualInterfaceId)]
      )

instance ToPath StopBGPFailoverTest where
  toPath = const "/"

instance ToQuery StopBGPFailoverTest where
  toQuery = const mempty

-- | /See:/ 'stopBGPFailoverTestResponse' smart constructor.
data StopBGPFailoverTestResponse = StopBGPFailoverTestResponse'
  { _sbgpftrsVirtualInterfaceTest ::
      !( Maybe
           VirtualInterfaceTestHistory
       ),
    _sbgpftrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopBGPFailoverTestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbgpftrsVirtualInterfaceTest' - Information about the virtual interface failover test.
--
-- * 'sbgpftrsResponseStatus' - -- | The response status code.
stopBGPFailoverTestResponse ::
  -- | 'sbgpftrsResponseStatus'
  Int ->
  StopBGPFailoverTestResponse
stopBGPFailoverTestResponse pResponseStatus_ =
  StopBGPFailoverTestResponse'
    { _sbgpftrsVirtualInterfaceTest =
        Nothing,
      _sbgpftrsResponseStatus = pResponseStatus_
    }

-- | Information about the virtual interface failover test.
sbgpftrsVirtualInterfaceTest :: Lens' StopBGPFailoverTestResponse (Maybe VirtualInterfaceTestHistory)
sbgpftrsVirtualInterfaceTest = lens _sbgpftrsVirtualInterfaceTest (\s a -> s {_sbgpftrsVirtualInterfaceTest = a})

-- | -- | The response status code.
sbgpftrsResponseStatus :: Lens' StopBGPFailoverTestResponse Int
sbgpftrsResponseStatus = lens _sbgpftrsResponseStatus (\s a -> s {_sbgpftrsResponseStatus = a})

instance NFData StopBGPFailoverTestResponse
