{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SetTerminationProtection (..)
    , mkSetTerminationProtection
    -- ** Request lenses
    , stpJobFlowIds
    , stpTerminationProtected

    -- * Destructuring the response
    , SetTerminationProtectionResponse (..)
    , mkSetTerminationProtectionResponse
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input argument to the 'TerminationProtection' operation. 
--
-- /See:/ 'mkSetTerminationProtection' smart constructor.
data SetTerminationProtection = SetTerminationProtection'
  { jobFlowIds :: [Types.XmlString]
    -- ^ A list of strings that uniquely identify the clusters to protect. This identifier is returned by 'RunJobFlow' and can also be obtained from 'DescribeJobFlows' . 
  , terminationProtected :: Core.Bool
    -- ^ A Boolean that indicates whether to protect the cluster and prevent the Amazon EC2 instances in the cluster from shutting down due to API calls, user intervention, or job-flow error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTerminationProtection' value with any optional fields omitted.
mkSetTerminationProtection
    :: Core.Bool -- ^ 'terminationProtected'
    -> SetTerminationProtection
mkSetTerminationProtection terminationProtected
  = SetTerminationProtection'{jobFlowIds = Core.mempty,
                              terminationProtected}

-- | A list of strings that uniquely identify the clusters to protect. This identifier is returned by 'RunJobFlow' and can also be obtained from 'DescribeJobFlows' . 
--
-- /Note:/ Consider using 'jobFlowIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stpJobFlowIds :: Lens.Lens' SetTerminationProtection [Types.XmlString]
stpJobFlowIds = Lens.field @"jobFlowIds"
{-# INLINEABLE stpJobFlowIds #-}
{-# DEPRECATED jobFlowIds "Use generic-lens or generic-optics with 'jobFlowIds' instead"  #-}

-- | A Boolean that indicates whether to protect the cluster and prevent the Amazon EC2 instances in the cluster from shutting down due to API calls, user intervention, or job-flow error.
--
-- /Note:/ Consider using 'terminationProtected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stpTerminationProtected :: Lens.Lens' SetTerminationProtection Core.Bool
stpTerminationProtected = Lens.field @"terminationProtected"
{-# INLINEABLE stpTerminationProtected #-}
{-# DEPRECATED terminationProtected "Use generic-lens or generic-optics with 'terminationProtected' instead"  #-}

instance Core.ToQuery SetTerminationProtection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetTerminationProtection where
        toHeaders SetTerminationProtection{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.SetTerminationProtection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetTerminationProtection where
        toJSON SetTerminationProtection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobFlowIds" Core..= jobFlowIds),
                  Core.Just ("TerminationProtected" Core..= terminationProtected)])

instance Core.AWSRequest SetTerminationProtection where
        type Rs SetTerminationProtection = SetTerminationProtectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull SetTerminationProtectionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetTerminationProtectionResponse' smart constructor.
data SetTerminationProtectionResponse = SetTerminationProtectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTerminationProtectionResponse' value with any optional fields omitted.
mkSetTerminationProtectionResponse
    :: SetTerminationProtectionResponse
mkSetTerminationProtectionResponse
  = SetTerminationProtectionResponse'
