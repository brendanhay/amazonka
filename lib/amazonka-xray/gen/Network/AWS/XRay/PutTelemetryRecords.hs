{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ptrTelemetryRecords,
    ptrEC2InstanceId,
    ptrHostname,
    ptrResourceARN,

    -- * Destructuring the response
    PutTelemetryRecordsResponse (..),
    mkPutTelemetryRecordsResponse,

    -- ** Response lenses
    ptrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkPutTelemetryRecords' smart constructor.
data PutTelemetryRecords = PutTelemetryRecords'
  { -- |
    telemetryRecords :: [Types.TelemetryRecord],
    -- |
    eC2InstanceId :: Core.Maybe Types.EC2InstanceId,
    -- |
    hostname :: Core.Maybe Types.Hostname,
    -- |
    resourceARN :: Core.Maybe Types.ResourceARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutTelemetryRecords' value with any optional fields omitted.
mkPutTelemetryRecords ::
  PutTelemetryRecords
mkPutTelemetryRecords =
  PutTelemetryRecords'
    { telemetryRecords = Core.mempty,
      eC2InstanceId = Core.Nothing,
      hostname = Core.Nothing,
      resourceARN = Core.Nothing
    }

-- |
--
-- /Note:/ Consider using 'telemetryRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrTelemetryRecords :: Lens.Lens' PutTelemetryRecords [Types.TelemetryRecord]
ptrTelemetryRecords = Lens.field @"telemetryRecords"
{-# DEPRECATED ptrTelemetryRecords "Use generic-lens or generic-optics with 'telemetryRecords' instead." #-}

-- |
--
-- /Note:/ Consider using 'eC2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrEC2InstanceId :: Lens.Lens' PutTelemetryRecords (Core.Maybe Types.EC2InstanceId)
ptrEC2InstanceId = Lens.field @"eC2InstanceId"
{-# DEPRECATED ptrEC2InstanceId "Use generic-lens or generic-optics with 'eC2InstanceId' instead." #-}

-- |
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrHostname :: Lens.Lens' PutTelemetryRecords (Core.Maybe Types.Hostname)
ptrHostname = Lens.field @"hostname"
{-# DEPRECATED ptrHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- |
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrResourceARN :: Lens.Lens' PutTelemetryRecords (Core.Maybe Types.ResourceARN)
ptrResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED ptrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Core.FromJSON PutTelemetryRecords where
  toJSON PutTelemetryRecords {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TelemetryRecords" Core..= telemetryRecords),
            ("EC2InstanceId" Core..=) Core.<$> eC2InstanceId,
            ("Hostname" Core..=) Core.<$> hostname,
            ("ResourceARN" Core..=) Core.<$> resourceARN
          ]
      )

instance Core.AWSRequest PutTelemetryRecords where
  type Rs PutTelemetryRecords = PutTelemetryRecordsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/TelemetryRecords",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutTelemetryRecordsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutTelemetryRecordsResponse' smart constructor.
newtype PutTelemetryRecordsResponse = PutTelemetryRecordsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutTelemetryRecordsResponse' value with any optional fields omitted.
mkPutTelemetryRecordsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutTelemetryRecordsResponse
mkPutTelemetryRecordsResponse responseStatus =
  PutTelemetryRecordsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrrsResponseStatus :: Lens.Lens' PutTelemetryRecordsResponse Core.Int
ptrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ptrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
