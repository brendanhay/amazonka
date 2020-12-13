{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.SetTerminationProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- SetTerminationProtection locks a cluster (job flow) so the EC2 instances in the cluster cannot be terminated by user intervention, an API call, or in the event of a job-flow error. The cluster still terminates upon successful completion of the job flow. Calling @SetTerminationProtection@ on a cluster is similar to calling the Amazon EC2 @DisableAPITermination@ API on all EC2 instances in a cluster.
--
-- @SetTerminationProtection@ is used to prevent accidental termination of a cluster and to ensure that in the event of an error, the instances persist so that you can recover any data stored in their ephemeral instance storage.
-- To terminate a cluster that has been locked by setting @SetTerminationProtection@ to @true@ , you must first unlock the job flow by a subsequent call to @SetTerminationProtection@ in which you set the value to @false@ .
-- For more information, see<https://docs.aws.amazon.com/emr/latest/ManagementGuide/UsingEMR_TerminationProtection.html Managing Cluster Termination> in the /Amazon EMR Management Guide/ .
module Network.AWS.EMR.SetTerminationProtection
  ( -- * Creating a request
    SetTerminationProtection (..),
    mkSetTerminationProtection,

    -- ** Request lenses
    stpJobFlowIds,
    stpTerminationProtected,

    -- * Destructuring the response
    SetTerminationProtectionResponse (..),
    mkSetTerminationProtectionResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input argument to the 'TerminationProtection' operation.
--
-- /See:/ 'mkSetTerminationProtection' smart constructor.
data SetTerminationProtection = SetTerminationProtection'
  { -- | A list of strings that uniquely identify the clusters to protect. This identifier is returned by 'RunJobFlow' and can also be obtained from 'DescribeJobFlows' .
    jobFlowIds :: [Lude.Text],
    -- | A Boolean that indicates whether to protect the cluster and prevent the Amazon EC2 instances in the cluster from shutting down due to API calls, user intervention, or job-flow error.
    terminationProtected :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTerminationProtection' with the minimum fields required to make a request.
--
-- * 'jobFlowIds' - A list of strings that uniquely identify the clusters to protect. This identifier is returned by 'RunJobFlow' and can also be obtained from 'DescribeJobFlows' .
-- * 'terminationProtected' - A Boolean that indicates whether to protect the cluster and prevent the Amazon EC2 instances in the cluster from shutting down due to API calls, user intervention, or job-flow error.
mkSetTerminationProtection ::
  -- | 'terminationProtected'
  Lude.Bool ->
  SetTerminationProtection
mkSetTerminationProtection pTerminationProtected_ =
  SetTerminationProtection'
    { jobFlowIds = Lude.mempty,
      terminationProtected = pTerminationProtected_
    }

-- | A list of strings that uniquely identify the clusters to protect. This identifier is returned by 'RunJobFlow' and can also be obtained from 'DescribeJobFlows' .
--
-- /Note:/ Consider using 'jobFlowIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stpJobFlowIds :: Lens.Lens' SetTerminationProtection [Lude.Text]
stpJobFlowIds = Lens.lens (jobFlowIds :: SetTerminationProtection -> [Lude.Text]) (\s a -> s {jobFlowIds = a} :: SetTerminationProtection)
{-# DEPRECATED stpJobFlowIds "Use generic-lens or generic-optics with 'jobFlowIds' instead." #-}

-- | A Boolean that indicates whether to protect the cluster and prevent the Amazon EC2 instances in the cluster from shutting down due to API calls, user intervention, or job-flow error.
--
-- /Note:/ Consider using 'terminationProtected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stpTerminationProtected :: Lens.Lens' SetTerminationProtection Lude.Bool
stpTerminationProtected = Lens.lens (terminationProtected :: SetTerminationProtection -> Lude.Bool) (\s a -> s {terminationProtected = a} :: SetTerminationProtection)
{-# DEPRECATED stpTerminationProtected "Use generic-lens or generic-optics with 'terminationProtected' instead." #-}

instance Lude.AWSRequest SetTerminationProtection where
  type Rs SetTerminationProtection = SetTerminationProtectionResponse
  request = Req.postJSON emrService
  response = Res.receiveNull SetTerminationProtectionResponse'

instance Lude.ToHeaders SetTerminationProtection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.SetTerminationProtection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetTerminationProtection where
  toJSON SetTerminationProtection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobFlowIds" Lude..= jobFlowIds),
            Lude.Just ("TerminationProtected" Lude..= terminationProtected)
          ]
      )

instance Lude.ToPath SetTerminationProtection where
  toPath = Lude.const "/"

instance Lude.ToQuery SetTerminationProtection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetTerminationProtectionResponse' smart constructor.
data SetTerminationProtectionResponse = SetTerminationProtectionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTerminationProtectionResponse' with the minimum fields required to make a request.
mkSetTerminationProtectionResponse ::
  SetTerminationProtectionResponse
mkSetTerminationProtectionResponse =
  SetTerminationProtectionResponse'
