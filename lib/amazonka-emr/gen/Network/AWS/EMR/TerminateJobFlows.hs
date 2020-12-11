{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the 'TerminateJobFlows' operation.
--
-- /See:/ 'mkTerminateJobFlows' smart constructor.
newtype TerminateJobFlows = TerminateJobFlows'
  { jobFlowIds ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateJobFlows' with the minimum fields required to make a request.
--
-- * 'jobFlowIds' - A list of job flows to be shut down.
mkTerminateJobFlows ::
  TerminateJobFlows
mkTerminateJobFlows = TerminateJobFlows' {jobFlowIds = Lude.mempty}

-- | A list of job flows to be shut down.
--
-- /Note:/ Consider using 'jobFlowIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfJobFlowIds :: Lens.Lens' TerminateJobFlows [Lude.Text]
tjfJobFlowIds = Lens.lens (jobFlowIds :: TerminateJobFlows -> [Lude.Text]) (\s a -> s {jobFlowIds = a} :: TerminateJobFlows)
{-# DEPRECATED tjfJobFlowIds "Use generic-lens or generic-optics with 'jobFlowIds' instead." #-}

instance Lude.AWSRequest TerminateJobFlows where
  type Rs TerminateJobFlows = TerminateJobFlowsResponse
  request = Req.postJSON emrService
  response = Res.receiveNull TerminateJobFlowsResponse'

instance Lude.ToHeaders TerminateJobFlows where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.TerminateJobFlows" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TerminateJobFlows where
  toJSON TerminateJobFlows' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("JobFlowIds" Lude..= jobFlowIds)])

instance Lude.ToPath TerminateJobFlows where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateJobFlows where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTerminateJobFlowsResponse' smart constructor.
data TerminateJobFlowsResponse = TerminateJobFlowsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateJobFlowsResponse' with the minimum fields required to make a request.
mkTerminateJobFlowsResponse ::
  TerminateJobFlowsResponse
mkTerminateJobFlowsResponse = TerminateJobFlowsResponse'
