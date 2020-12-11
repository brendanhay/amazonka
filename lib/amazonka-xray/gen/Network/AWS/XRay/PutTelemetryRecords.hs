{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.PutTelemetryRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by the AWS X-Ray daemon to upload telemetry.
module Network.AWS.XRay.PutTelemetryRecords
  ( -- * Creating a request
    PutTelemetryRecords (..),
    mkPutTelemetryRecords,

    -- ** Request lenses
    ptrHostname,
    ptrEC2InstanceId,
    ptrResourceARN,
    ptrTelemetryRecords,

    -- * Destructuring the response
    PutTelemetryRecordsResponse (..),
    mkPutTelemetryRecordsResponse,

    -- ** Response lenses
    ptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkPutTelemetryRecords' smart constructor.
data PutTelemetryRecords = PutTelemetryRecords'
  { hostname ::
      Lude.Maybe Lude.Text,
    ec2InstanceId :: Lude.Maybe Lude.Text,
    resourceARN :: Lude.Maybe Lude.Text,
    telemetryRecords :: [TelemetryRecord]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutTelemetryRecords' with the minimum fields required to make a request.
--
-- * 'ec2InstanceId' -
-- * 'hostname' -
-- * 'resourceARN' -
-- * 'telemetryRecords' -
mkPutTelemetryRecords ::
  PutTelemetryRecords
mkPutTelemetryRecords =
  PutTelemetryRecords'
    { hostname = Lude.Nothing,
      ec2InstanceId = Lude.Nothing,
      resourceARN = Lude.Nothing,
      telemetryRecords = Lude.mempty
    }

-- |
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrHostname :: Lens.Lens' PutTelemetryRecords (Lude.Maybe Lude.Text)
ptrHostname = Lens.lens (hostname :: PutTelemetryRecords -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: PutTelemetryRecords)
{-# DEPRECATED ptrHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- |
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrEC2InstanceId :: Lens.Lens' PutTelemetryRecords (Lude.Maybe Lude.Text)
ptrEC2InstanceId = Lens.lens (ec2InstanceId :: PutTelemetryRecords -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceId = a} :: PutTelemetryRecords)
{-# DEPRECATED ptrEC2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- |
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrResourceARN :: Lens.Lens' PutTelemetryRecords (Lude.Maybe Lude.Text)
ptrResourceARN = Lens.lens (resourceARN :: PutTelemetryRecords -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: PutTelemetryRecords)
{-# DEPRECATED ptrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- |
--
-- /Note:/ Consider using 'telemetryRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrTelemetryRecords :: Lens.Lens' PutTelemetryRecords [TelemetryRecord]
ptrTelemetryRecords = Lens.lens (telemetryRecords :: PutTelemetryRecords -> [TelemetryRecord]) (\s a -> s {telemetryRecords = a} :: PutTelemetryRecords)
{-# DEPRECATED ptrTelemetryRecords "Use generic-lens or generic-optics with 'telemetryRecords' instead." #-}

instance Lude.AWSRequest PutTelemetryRecords where
  type Rs PutTelemetryRecords = PutTelemetryRecordsResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutTelemetryRecordsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutTelemetryRecords where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutTelemetryRecords where
  toJSON PutTelemetryRecords' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Hostname" Lude..=) Lude.<$> hostname,
            ("EC2InstanceId" Lude..=) Lude.<$> ec2InstanceId,
            ("ResourceARN" Lude..=) Lude.<$> resourceARN,
            Lude.Just ("TelemetryRecords" Lude..= telemetryRecords)
          ]
      )

instance Lude.ToPath PutTelemetryRecords where
  toPath = Lude.const "/TelemetryRecords"

instance Lude.ToQuery PutTelemetryRecords where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutTelemetryRecordsResponse' smart constructor.
newtype PutTelemetryRecordsResponse = PutTelemetryRecordsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutTelemetryRecordsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutTelemetryRecordsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutTelemetryRecordsResponse
mkPutTelemetryRecordsResponse pResponseStatus_ =
  PutTelemetryRecordsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsResponseStatus :: Lens.Lens' PutTelemetryRecordsResponse Lude.Int
ptrrsResponseStatus = Lens.lens (responseStatus :: PutTelemetryRecordsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutTelemetryRecordsResponse)
{-# DEPRECATED ptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
