{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling policy.
--
-- Deleting either a step scaling policy or a simple scaling policy deletes the underlying alarm action, but does not delete the alarm, even if it no longer has an associated action.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/deleting-scaling-policy.html Deleting a scaling policy> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DeletePolicy
    (
    -- * Creating a request
      DeletePolicy (..)
    , mkDeletePolicy
    -- ** Request lenses
    , dpPolicyName
    , dpAutoScalingGroupName

    -- * Destructuring the response
    , DeletePolicyResponse (..)
    , mkDeletePolicyResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { policyName :: Types.ResourceName
    -- ^ The name or Amazon Resource Name (ARN) of the policy.
  , autoScalingGroupName :: Core.Maybe Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicy' value with any optional fields omitted.
mkDeletePolicy
    :: Types.ResourceName -- ^ 'policyName'
    -> DeletePolicy
mkDeletePolicy policyName
  = DeletePolicy'{policyName, autoScalingGroupName = Core.Nothing}

-- | The name or Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyName :: Lens.Lens' DeletePolicy Types.ResourceName
dpPolicyName = Lens.field @"policyName"
{-# INLINEABLE dpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpAutoScalingGroupName :: Lens.Lens' DeletePolicy (Core.Maybe Types.ResourceName)
dpAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dpAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

instance Core.ToQuery DeletePolicy where
        toQuery DeletePolicy{..}
          = Core.toQueryPair "Action" ("DeletePolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoScalingGroupName")
                autoScalingGroupName

instance Core.ToHeaders DeletePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePolicy where
        type Rs DeletePolicy = DeletePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeletePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicyResponse' value with any optional fields omitted.
mkDeletePolicyResponse
    :: DeletePolicyResponse
mkDeletePolicyResponse = DeletePolicyResponse'
