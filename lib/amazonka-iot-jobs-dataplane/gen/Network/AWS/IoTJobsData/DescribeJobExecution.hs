{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.DescribeJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of a job execution.
module Network.AWS.IoTJobsData.DescribeJobExecution
    (
    -- * Creating a request
      DescribeJobExecution (..)
    , mkDescribeJobExecution
    -- ** Request lenses
    , djeJobId
    , djeThingName
    , djeExecutionNumber
    , djeIncludeJobDocument

    -- * Destructuring the response
    , DescribeJobExecutionResponse (..)
    , mkDescribeJobExecutionResponse
    -- ** Response lenses
    , djerrsExecution
    , djerrsResponseStatus
    ) where

import qualified Network.AWS.IoTJobsData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { jobId :: Types.DescribeJobExecutionJobId
    -- ^ The unique identifier assigned to this job when it was created.
  , thingName :: Types.ThingName
    -- ^ The thing name associated with the device the job execution is running on.
  , executionNumber :: Core.Maybe Core.Integer
    -- ^ Optional. A number that identifies a particular job execution on a particular device. If not specified, the latest job execution is returned.
  , includeJobDocument :: Core.Maybe Core.Bool
    -- ^ Optional. When set to true, the response contains the job document. The default is false.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobExecution' value with any optional fields omitted.
mkDescribeJobExecution
    :: Types.DescribeJobExecutionJobId -- ^ 'jobId'
    -> Types.ThingName -- ^ 'thingName'
    -> DescribeJobExecution
mkDescribeJobExecution jobId thingName
  = DescribeJobExecution'{jobId, thingName,
                          executionNumber = Core.Nothing, includeJobDocument = Core.Nothing}

-- | The unique identifier assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeJobId :: Lens.Lens' DescribeJobExecution Types.DescribeJobExecutionJobId
djeJobId = Lens.field @"jobId"
{-# INLINEABLE djeJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The thing name associated with the device the job execution is running on.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeThingName :: Lens.Lens' DescribeJobExecution Types.ThingName
djeThingName = Lens.field @"thingName"
{-# INLINEABLE djeThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | Optional. A number that identifies a particular job execution on a particular device. If not specified, the latest job execution is returned.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeExecutionNumber :: Lens.Lens' DescribeJobExecution (Core.Maybe Core.Integer)
djeExecutionNumber = Lens.field @"executionNumber"
{-# INLINEABLE djeExecutionNumber #-}
{-# DEPRECATED executionNumber "Use generic-lens or generic-optics with 'executionNumber' instead"  #-}

-- | Optional. When set to true, the response contains the job document. The default is false.
--
-- /Note:/ Consider using 'includeJobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeIncludeJobDocument :: Lens.Lens' DescribeJobExecution (Core.Maybe Core.Bool)
djeIncludeJobDocument = Lens.field @"includeJobDocument"
{-# INLINEABLE djeIncludeJobDocument #-}
{-# DEPRECATED includeJobDocument "Use generic-lens or generic-optics with 'includeJobDocument' instead"  #-}

instance Core.ToQuery DescribeJobExecution where
        toQuery DescribeJobExecution{..}
          = Core.maybe Core.mempty (Core.toQueryPair "executionNumber")
              executionNumber
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "includeJobDocument")
                includeJobDocument

instance Core.ToHeaders DescribeJobExecution where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeJobExecution where
        type Rs DescribeJobExecution = DescribeJobExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/jobs/" Core.<>
                             Core.toText jobId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeJobExecutionResponse' Core.<$>
                   (x Core..:? "execution") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { execution :: Core.Maybe Types.JobExecution
    -- ^ Contains data about a job execution.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobExecutionResponse' value with any optional fields omitted.
mkDescribeJobExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeJobExecutionResponse
mkDescribeJobExecutionResponse responseStatus
  = DescribeJobExecutionResponse'{execution = Core.Nothing,
                                  responseStatus}

-- | Contains data about a job execution.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djerrsExecution :: Lens.Lens' DescribeJobExecutionResponse (Core.Maybe Types.JobExecution)
djerrsExecution = Lens.field @"execution"
{-# INLINEABLE djerrsExecution #-}
{-# DEPRECATED execution "Use generic-lens or generic-optics with 'execution' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djerrsResponseStatus :: Lens.Lens' DescribeJobExecutionResponse Core.Int
djerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE djerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
