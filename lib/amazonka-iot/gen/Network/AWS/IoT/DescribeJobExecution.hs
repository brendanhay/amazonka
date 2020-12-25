{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job execution.
module Network.AWS.IoT.DescribeJobExecution
  ( -- * Creating a request
    DescribeJobExecution (..),
    mkDescribeJobExecution,

    -- ** Request lenses
    dJobId,
    dThingName,
    dExecutionNumber,

    -- * Destructuring the response
    DescribeJobExecutionResponse (..),
    mkDescribeJobExecutionResponse,

    -- ** Response lenses
    djerrsExecution,
    djerrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Types.JobId,
    -- | The name of the thing on which the job execution is running.
    thingName :: Types.ThingName,
    -- | A string (consisting of the digits "0" through "9" which is used to specify a particular job execution on a particular device.
    executionNumber :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobExecution' value with any optional fields omitted.
mkDescribeJobExecution ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'thingName'
  Types.ThingName ->
  DescribeJobExecution
mkDescribeJobExecution jobId thingName =
  DescribeJobExecution'
    { jobId,
      thingName,
      executionNumber = Core.Nothing
    }

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dJobId :: Lens.Lens' DescribeJobExecution Types.JobId
dJobId = Lens.field @"jobId"
{-# DEPRECATED dJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name of the thing on which the job execution is running.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingName :: Lens.Lens' DescribeJobExecution Types.ThingName
dThingName = Lens.field @"thingName"
{-# DEPRECATED dThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | A string (consisting of the digits "0" through "9" which is used to specify a particular job execution on a particular device.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExecutionNumber :: Lens.Lens' DescribeJobExecution (Core.Maybe Core.Integer)
dExecutionNumber = Lens.field @"executionNumber"
{-# DEPRECATED dExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

instance Core.AWSRequest DescribeJobExecution where
  type Rs DescribeJobExecution = DescribeJobExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/things/" Core.<> (Core.toText thingName) Core.<> ("/jobs/")
                Core.<> (Core.toText jobId)
            ),
        Core._rqQuery =
          Core.toQueryValue "executionNumber" Core.<$> executionNumber,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobExecutionResponse'
            Core.<$> (x Core..:? "execution") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { -- | Information about the job execution.
    execution :: Core.Maybe Types.JobExecution,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeJobExecutionResponse' value with any optional fields omitted.
mkDescribeJobExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeJobExecutionResponse
mkDescribeJobExecutionResponse responseStatus =
  DescribeJobExecutionResponse'
    { execution = Core.Nothing,
      responseStatus
    }

-- | Information about the job execution.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djerrsExecution :: Lens.Lens' DescribeJobExecutionResponse (Core.Maybe Types.JobExecution)
djerrsExecution = Lens.field @"execution"
{-# DEPRECATED djerrsExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djerrsResponseStatus :: Lens.Lens' DescribeJobExecutionResponse Core.Int
djerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED djerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
