{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    StartAvailabilityMonitorTest (..),
    mkStartAvailabilityMonitorTest,

    -- ** Request lenses
    samtGatewayARN,

    -- * Destructuring the response
    StartAvailabilityMonitorTestResponse (..),
    mkStartAvailabilityMonitorTestResponse,

    -- ** Response lenses
    samtrsGatewayARN,
    samtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkStartAvailabilityMonitorTest' smart constructor.
newtype StartAvailabilityMonitorTest = StartAvailabilityMonitorTest'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAvailabilityMonitorTest' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkStartAvailabilityMonitorTest ::
  -- | 'gatewayARN'
  Lude.Text ->
  StartAvailabilityMonitorTest
mkStartAvailabilityMonitorTest pGatewayARN_ =
  StartAvailabilityMonitorTest' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samtGatewayARN :: Lens.Lens' StartAvailabilityMonitorTest Lude.Text
samtGatewayARN = Lens.lens (gatewayARN :: StartAvailabilityMonitorTest -> Lude.Text) (\s a -> s {gatewayARN = a} :: StartAvailabilityMonitorTest)
{-# DEPRECATED samtGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest StartAvailabilityMonitorTest where
  type
    Rs StartAvailabilityMonitorTest =
      StartAvailabilityMonitorTestResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartAvailabilityMonitorTestResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartAvailabilityMonitorTest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.StartAvailabilityMonitorTest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartAvailabilityMonitorTest where
  toJSON StartAvailabilityMonitorTest' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath StartAvailabilityMonitorTest where
  toPath = Lude.const "/"

instance Lude.ToQuery StartAvailabilityMonitorTest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartAvailabilityMonitorTestResponse' smart constructor.
data StartAvailabilityMonitorTestResponse = StartAvailabilityMonitorTestResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAvailabilityMonitorTestResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkStartAvailabilityMonitorTestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartAvailabilityMonitorTestResponse
mkStartAvailabilityMonitorTestResponse pResponseStatus_ =
  StartAvailabilityMonitorTestResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samtrsGatewayARN :: Lens.Lens' StartAvailabilityMonitorTestResponse (Lude.Maybe Lude.Text)
samtrsGatewayARN = Lens.lens (gatewayARN :: StartAvailabilityMonitorTestResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: StartAvailabilityMonitorTestResponse)
{-# DEPRECATED samtrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samtrsResponseStatus :: Lens.Lens' StartAvailabilityMonitorTestResponse Lude.Int
samtrsResponseStatus = Lens.lens (responseStatus :: StartAvailabilityMonitorTestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartAvailabilityMonitorTestResponse)
{-# DEPRECATED samtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
