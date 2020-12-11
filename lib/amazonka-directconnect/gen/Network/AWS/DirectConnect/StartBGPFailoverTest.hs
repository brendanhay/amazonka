{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- You can run the test on public, private, transit, and hosted virtual interfaces.
-- You can use <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_ListVirtualInterfaceTestHistory.html ListVirtualInterfaceTestHistory> to view the virtual interface test history.
-- If you need to stop the test before the test interval completes, use <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_StopBgpFailoverTest.html StopBgpFailoverTest> .
module Network.AWS.DirectConnect.StartBGPFailoverTest
  ( -- * Creating a request
    StartBGPFailoverTest (..),
    mkStartBGPFailoverTest,

    -- ** Request lenses
    sbftBgpPeers,
    sbftTestDurationInMinutes,
    sbftVirtualInterfaceId,

    -- * Destructuring the response
    StartBGPFailoverTestResponse (..),
    mkStartBGPFailoverTestResponse,

    -- ** Response lenses
    sbftrsVirtualInterfaceTest,
    sbftrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartBGPFailoverTest' smart constructor.
data StartBGPFailoverTest = StartBGPFailoverTest'
  { bgpPeers ::
      Lude.Maybe [Lude.Text],
    testDurationInMinutes :: Lude.Maybe Lude.Int,
    virtualInterfaceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartBGPFailoverTest' with the minimum fields required to make a request.
--
-- * 'bgpPeers' - The BGP peers to place in the DOWN state.
-- * 'testDurationInMinutes' - The time in minutes that the virtual interface failover test will last.
--
-- Maximum value: 180 minutes (3 hours).
-- Default: 180 minutes (3 hours).
-- * 'virtualInterfaceId' - The ID of the virtual interface you want to test.
mkStartBGPFailoverTest ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  StartBGPFailoverTest
mkStartBGPFailoverTest pVirtualInterfaceId_ =
  StartBGPFailoverTest'
    { bgpPeers = Lude.Nothing,
      testDurationInMinutes = Lude.Nothing,
      virtualInterfaceId = pVirtualInterfaceId_
    }

-- | The BGP peers to place in the DOWN state.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftBgpPeers :: Lens.Lens' StartBGPFailoverTest (Lude.Maybe [Lude.Text])
sbftBgpPeers = Lens.lens (bgpPeers :: StartBGPFailoverTest -> Lude.Maybe [Lude.Text]) (\s a -> s {bgpPeers = a} :: StartBGPFailoverTest)
{-# DEPRECATED sbftBgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead." #-}

-- | The time in minutes that the virtual interface failover test will last.
--
-- Maximum value: 180 minutes (3 hours).
-- Default: 180 minutes (3 hours).
--
-- /Note:/ Consider using 'testDurationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftTestDurationInMinutes :: Lens.Lens' StartBGPFailoverTest (Lude.Maybe Lude.Int)
sbftTestDurationInMinutes = Lens.lens (testDurationInMinutes :: StartBGPFailoverTest -> Lude.Maybe Lude.Int) (\s a -> s {testDurationInMinutes = a} :: StartBGPFailoverTest)
{-# DEPRECATED sbftTestDurationInMinutes "Use generic-lens or generic-optics with 'testDurationInMinutes' instead." #-}

-- | The ID of the virtual interface you want to test.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftVirtualInterfaceId :: Lens.Lens' StartBGPFailoverTest Lude.Text
sbftVirtualInterfaceId = Lens.lens (virtualInterfaceId :: StartBGPFailoverTest -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: StartBGPFailoverTest)
{-# DEPRECATED sbftVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest StartBGPFailoverTest where
  type Rs StartBGPFailoverTest = StartBGPFailoverTestResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartBGPFailoverTestResponse'
            Lude.<$> (x Lude..?> "virtualInterfaceTest")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartBGPFailoverTest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.StartBgpFailoverTest" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartBGPFailoverTest where
  toJSON StartBGPFailoverTest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bgpPeers" Lude..=) Lude.<$> bgpPeers,
            ("testDurationInMinutes" Lude..=) Lude.<$> testDurationInMinutes,
            Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId)
          ]
      )

instance Lude.ToPath StartBGPFailoverTest where
  toPath = Lude.const "/"

instance Lude.ToQuery StartBGPFailoverTest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartBGPFailoverTestResponse' smart constructor.
data StartBGPFailoverTestResponse = StartBGPFailoverTestResponse'
  { virtualInterfaceTest ::
      Lude.Maybe
        VirtualInterfaceTestHistory,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartBGPFailoverTestResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'virtualInterfaceTest' - Information about the virtual interface failover test.
mkStartBGPFailoverTestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartBGPFailoverTestResponse
mkStartBGPFailoverTestResponse pResponseStatus_ =
  StartBGPFailoverTestResponse'
    { virtualInterfaceTest =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the virtual interface failover test.
--
-- /Note:/ Consider using 'virtualInterfaceTest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftrsVirtualInterfaceTest :: Lens.Lens' StartBGPFailoverTestResponse (Lude.Maybe VirtualInterfaceTestHistory)
sbftrsVirtualInterfaceTest = Lens.lens (virtualInterfaceTest :: StartBGPFailoverTestResponse -> Lude.Maybe VirtualInterfaceTestHistory) (\s a -> s {virtualInterfaceTest = a} :: StartBGPFailoverTestResponse)
{-# DEPRECATED sbftrsVirtualInterfaceTest "Use generic-lens or generic-optics with 'virtualInterfaceTest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftrsResponseStatus :: Lens.Lens' StartBGPFailoverTestResponse Lude.Int
sbftrsResponseStatus = Lens.lens (responseStatus :: StartBGPFailoverTestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartBGPFailoverTestResponse)
{-# DEPRECATED sbftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
