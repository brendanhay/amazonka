{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the most recent High Availability monitoring test that was performed on the host in a cluster. If a test isn't performed, the status and start time in the response would be null.
module Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest
  ( -- * Creating a request
    DescribeAvailabilityMonitorTest (..),
    mkDescribeAvailabilityMonitorTest,

    -- ** Request lenses
    damtGatewayARN,

    -- * Destructuring the response
    DescribeAvailabilityMonitorTestResponse (..),
    mkDescribeAvailabilityMonitorTestResponse,

    -- ** Response lenses
    damtrsStatus,
    damtrsStartTime,
    damtrsGatewayARN,
    damtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDescribeAvailabilityMonitorTest' smart constructor.
newtype DescribeAvailabilityMonitorTest = DescribeAvailabilityMonitorTest'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailabilityMonitorTest' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkDescribeAvailabilityMonitorTest ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeAvailabilityMonitorTest
mkDescribeAvailabilityMonitorTest pGatewayARN_ =
  DescribeAvailabilityMonitorTest' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtGatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTest Lude.Text
damtGatewayARN = Lens.lens (gatewayARN :: DescribeAvailabilityMonitorTest -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeAvailabilityMonitorTest)
{-# DEPRECATED damtGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeAvailabilityMonitorTest where
  type
    Rs DescribeAvailabilityMonitorTest =
      DescribeAvailabilityMonitorTestResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAvailabilityMonitorTestResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "GatewayARN")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAvailabilityMonitorTest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeAvailabilityMonitorTest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAvailabilityMonitorTest where
  toJSON DescribeAvailabilityMonitorTest' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeAvailabilityMonitorTest where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAvailabilityMonitorTest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAvailabilityMonitorTestResponse' smart constructor.
data DescribeAvailabilityMonitorTestResponse = DescribeAvailabilityMonitorTestResponse'
  { -- | The status of the High Availability monitoring test. If a test hasn't been performed, the value of this field is null.
    status :: Lude.Maybe AvailabilityMonitorTestStatus,
    -- | The time the High Availability monitoring test was started. If a test hasn't been performed, the value of this field is null.
    startTime :: Lude.Maybe Lude.Timestamp,
    gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailabilityMonitorTestResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the High Availability monitoring test. If a test hasn't been performed, the value of this field is null.
-- * 'startTime' - The time the High Availability monitoring test was started. If a test hasn't been performed, the value of this field is null.
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkDescribeAvailabilityMonitorTestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAvailabilityMonitorTestResponse
mkDescribeAvailabilityMonitorTestResponse pResponseStatus_ =
  DescribeAvailabilityMonitorTestResponse'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the High Availability monitoring test. If a test hasn't been performed, the value of this field is null.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrsStatus :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Lude.Maybe AvailabilityMonitorTestStatus)
damtrsStatus = Lens.lens (status :: DescribeAvailabilityMonitorTestResponse -> Lude.Maybe AvailabilityMonitorTestStatus) (\s a -> s {status = a} :: DescribeAvailabilityMonitorTestResponse)
{-# DEPRECATED damtrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time the High Availability monitoring test was started. If a test hasn't been performed, the value of this field is null.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrsStartTime :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Lude.Maybe Lude.Timestamp)
damtrsStartTime = Lens.lens (startTime :: DescribeAvailabilityMonitorTestResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeAvailabilityMonitorTestResponse)
{-# DEPRECATED damtrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrsGatewayARN :: Lens.Lens' DescribeAvailabilityMonitorTestResponse (Lude.Maybe Lude.Text)
damtrsGatewayARN = Lens.lens (gatewayARN :: DescribeAvailabilityMonitorTestResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeAvailabilityMonitorTestResponse)
{-# DEPRECATED damtrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damtrsResponseStatus :: Lens.Lens' DescribeAvailabilityMonitorTestResponse Lude.Int
damtrsResponseStatus = Lens.lens (responseStatus :: DescribeAvailabilityMonitorTestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAvailabilityMonitorTestResponse)
{-# DEPRECATED damtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
