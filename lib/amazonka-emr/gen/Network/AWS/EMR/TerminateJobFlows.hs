{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.TerminateJobFlows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- TerminateJobFlows shuts a list of clusters (job flows) down. When a job flow is shut down, any step not yet completed is canceled and the EC2 instances on which the cluster is running are stopped. Any log files not already saved are uploaded to Amazon S3 if a LogUri was specified when the cluster was created.
--
-- The maximum number of clusters allowed is 10. The call to @TerminateJobFlows@ is asynchronous. Depending on the configuration of the cluster, it may take up to 1-5 minutes for the cluster to completely terminate and release allocated resources, such as Amazon EC2 instances.
module Network.AWS.EMR.TerminateJobFlows
  ( -- * Creating a request
    TerminateJobFlows (..),
    mkTerminateJobFlows,

    -- ** Request lenses
    tjfJobFlowIds,

    -- * Destructuring the response
    TerminateJobFlowsResponse (..),
    mkTerminateJobFlowsResponse,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the 'TerminateJobFlows' operation.
--
-- /See:/ 'mkTerminateJobFlows' smart constructor.
newtype TerminateJobFlows = TerminateJobFlows'
  { -- | A list of job flows to be shut down.
    jobFlowIds :: [Types.XmlString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateJobFlows' value with any optional fields omitted.
mkTerminateJobFlows ::
  TerminateJobFlows
mkTerminateJobFlows = TerminateJobFlows' {jobFlowIds = Core.mempty}

-- | A list of job flows to be shut down.
--
-- /Note:/ Consider using 'jobFlowIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfJobFlowIds :: Lens.Lens' TerminateJobFlows [Types.XmlString]
tjfJobFlowIds = Lens.field @"jobFlowIds"
{-# DEPRECATED tjfJobFlowIds "Use generic-lens or generic-optics with 'jobFlowIds' instead." #-}

instance Core.FromJSON TerminateJobFlows where
  toJSON TerminateJobFlows {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobFlowIds" Core..= jobFlowIds)])

instance Core.AWSRequest TerminateJobFlows where
  type Rs TerminateJobFlows = TerminateJobFlowsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.TerminateJobFlows")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull TerminateJobFlowsResponse'

-- | /See:/ 'mkTerminateJobFlowsResponse' smart constructor.
data TerminateJobFlowsResponse = TerminateJobFlowsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateJobFlowsResponse' value with any optional fields omitted.
mkTerminateJobFlowsResponse ::
  TerminateJobFlowsResponse
mkTerminateJobFlowsResponse = TerminateJobFlowsResponse'
