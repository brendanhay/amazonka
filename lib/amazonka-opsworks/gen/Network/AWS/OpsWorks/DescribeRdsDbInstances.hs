{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeRdsDbInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon RDS instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
-- This call accepts only one resource-identifying parameter.
module Network.AWS.OpsWorks.DescribeRdsDbInstances
    (
    -- * Creating a request
      DescribeRdsDbInstances (..)
    , mkDescribeRdsDbInstances
    -- ** Request lenses
    , drdiStackId
    , drdiRdsDbInstanceArns

    -- * Destructuring the response
    , DescribeRdsDbInstancesResponse (..)
    , mkDescribeRdsDbInstancesResponse
    -- ** Response lenses
    , drdirrsRdsDbInstances
    , drdirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRdsDbInstances' smart constructor.
data DescribeRdsDbInstances = DescribeRdsDbInstances'
  { stackId :: Core.Text
    -- ^ The ID of the stack with which the instances are registered. The operation returns descriptions of all registered Amazon RDS instances.
  , rdsDbInstanceArns :: Core.Maybe [Core.Text]
    -- ^ An array containing the ARNs of the instances to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRdsDbInstances' value with any optional fields omitted.
mkDescribeRdsDbInstances
    :: Core.Text -- ^ 'stackId'
    -> DescribeRdsDbInstances
mkDescribeRdsDbInstances stackId
  = DescribeRdsDbInstances'{stackId,
                            rdsDbInstanceArns = Core.Nothing}

-- | The ID of the stack with which the instances are registered. The operation returns descriptions of all registered Amazon RDS instances.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiStackId :: Lens.Lens' DescribeRdsDbInstances Core.Text
drdiStackId = Lens.field @"stackId"
{-# INLINEABLE drdiStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | An array containing the ARNs of the instances to be described.
--
-- /Note:/ Consider using 'rdsDbInstanceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiRdsDbInstanceArns :: Lens.Lens' DescribeRdsDbInstances (Core.Maybe [Core.Text])
drdiRdsDbInstanceArns = Lens.field @"rdsDbInstanceArns"
{-# INLINEABLE drdiRdsDbInstanceArns #-}
{-# DEPRECATED rdsDbInstanceArns "Use generic-lens or generic-optics with 'rdsDbInstanceArns' instead"  #-}

instance Core.ToQuery DescribeRdsDbInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRdsDbInstances where
        toHeaders DescribeRdsDbInstances{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DescribeRdsDbInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRdsDbInstances where
        toJSON DescribeRdsDbInstances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackId" Core..= stackId),
                  ("RdsDbInstanceArns" Core..=) Core.<$> rdsDbInstanceArns])

instance Core.AWSRequest DescribeRdsDbInstances where
        type Rs DescribeRdsDbInstances = DescribeRdsDbInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRdsDbInstancesResponse' Core.<$>
                   (x Core..:? "RdsDbInstances") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeRdsDbInstances@ request.
--
-- /See:/ 'mkDescribeRdsDbInstancesResponse' smart constructor.
data DescribeRdsDbInstancesResponse = DescribeRdsDbInstancesResponse'
  { rdsDbInstances :: Core.Maybe [Types.RdsDbInstance]
    -- ^ An a array of @RdsDbInstance@ objects that describe the instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRdsDbInstancesResponse' value with any optional fields omitted.
mkDescribeRdsDbInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRdsDbInstancesResponse
mkDescribeRdsDbInstancesResponse responseStatus
  = DescribeRdsDbInstancesResponse'{rdsDbInstances = Core.Nothing,
                                    responseStatus}

-- | An a array of @RdsDbInstance@ objects that describe the instances.
--
-- /Note:/ Consider using 'rdsDbInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdirrsRdsDbInstances :: Lens.Lens' DescribeRdsDbInstancesResponse (Core.Maybe [Types.RdsDbInstance])
drdirrsRdsDbInstances = Lens.field @"rdsDbInstances"
{-# INLINEABLE drdirrsRdsDbInstances #-}
{-# DEPRECATED rdsDbInstances "Use generic-lens or generic-optics with 'rdsDbInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdirrsResponseStatus :: Lens.Lens' DescribeRdsDbInstancesResponse Core.Int
drdirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drdirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
