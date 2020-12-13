{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    StopBGPFailoverTest (..),
    mkStopBGPFailoverTest,

    -- ** Request lenses
    sbftVirtualInterfaceId,

    -- * Destructuring the response
    StopBGPFailoverTestResponse (..),
    mkStopBGPFailoverTestResponse,

    -- ** Response lenses
    sbgpftrsVirtualInterfaceTest,
    sbgpftrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopBGPFailoverTest' smart constructor.
newtype StopBGPFailoverTest = StopBGPFailoverTest'
  { -- | The ID of the virtual interface you no longer want to test.
    virtualInterfaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopBGPFailoverTest' with the minimum fields required to make a request.
--
-- * 'virtualInterfaceId' - The ID of the virtual interface you no longer want to test.
mkStopBGPFailoverTest ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  StopBGPFailoverTest
mkStopBGPFailoverTest pVirtualInterfaceId_ =
  StopBGPFailoverTest' {virtualInterfaceId = pVirtualInterfaceId_}

-- | The ID of the virtual interface you no longer want to test.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftVirtualInterfaceId :: Lens.Lens' StopBGPFailoverTest Lude.Text
sbftVirtualInterfaceId = Lens.lens (virtualInterfaceId :: StopBGPFailoverTest -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: StopBGPFailoverTest)
{-# DEPRECATED sbftVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest StopBGPFailoverTest where
  type Rs StopBGPFailoverTest = StopBGPFailoverTestResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopBGPFailoverTestResponse'
            Lude.<$> (x Lude..?> "virtualInterfaceTest")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopBGPFailoverTest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.StopBgpFailoverTest" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopBGPFailoverTest where
  toJSON StopBGPFailoverTest' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId)]
      )

instance Lude.ToPath StopBGPFailoverTest where
  toPath = Lude.const "/"

instance Lude.ToQuery StopBGPFailoverTest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopBGPFailoverTestResponse' smart constructor.
data StopBGPFailoverTestResponse = StopBGPFailoverTestResponse'
  { -- | Information about the virtual interface failover test.
    virtualInterfaceTest :: Lude.Maybe VirtualInterfaceTestHistory,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopBGPFailoverTestResponse' with the minimum fields required to make a request.
--
-- * 'virtualInterfaceTest' - Information about the virtual interface failover test.
-- * 'responseStatus' - The response status code.
mkStopBGPFailoverTestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopBGPFailoverTestResponse
mkStopBGPFailoverTestResponse pResponseStatus_ =
  StopBGPFailoverTestResponse'
    { virtualInterfaceTest = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the virtual interface failover test.
--
-- /Note:/ Consider using 'virtualInterfaceTest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbgpftrsVirtualInterfaceTest :: Lens.Lens' StopBGPFailoverTestResponse (Lude.Maybe VirtualInterfaceTestHistory)
sbgpftrsVirtualInterfaceTest = Lens.lens (virtualInterfaceTest :: StopBGPFailoverTestResponse -> Lude.Maybe VirtualInterfaceTestHistory) (\s a -> s {virtualInterfaceTest = a} :: StopBGPFailoverTestResponse)
{-# DEPRECATED sbgpftrsVirtualInterfaceTest "Use generic-lens or generic-optics with 'virtualInterfaceTest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbgpftrsResponseStatus :: Lens.Lens' StopBGPFailoverTestResponse Lude.Int
sbgpftrsResponseStatus = Lens.lens (responseStatus :: StopBGPFailoverTestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopBGPFailoverTestResponse)
{-# DEPRECATED sbgpftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
