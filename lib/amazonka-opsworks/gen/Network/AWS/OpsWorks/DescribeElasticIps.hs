{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeElasticIps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP addresses> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeElasticIps
    (
    -- * Creating a request
      DescribeElasticIps (..)
    , mkDescribeElasticIps
    -- ** Request lenses
    , deiInstanceId
    , deiIps
    , deiStackId

    -- * Destructuring the response
    , DescribeElasticIpsResponse (..)
    , mkDescribeElasticIpsResponse
    -- ** Response lenses
    , deirrsElasticIps
    , deirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeElasticIps' smart constructor.
data DescribeElasticIps = DescribeElasticIps'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses associated with the specified instance.
  , ips :: Core.Maybe [Core.Text]
    -- ^ An array of Elastic IP addresses to be described. If you include this parameter, @DescribeElasticIps@ returns a description of the specified Elastic IP addresses. Otherwise, it returns a description of every Elastic IP address.
  , stackId :: Core.Maybe Core.Text
    -- ^ A stack ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses that are registered with the specified stack.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticIps' value with any optional fields omitted.
mkDescribeElasticIps
    :: DescribeElasticIps
mkDescribeElasticIps
  = DescribeElasticIps'{instanceId = Core.Nothing,
                        ips = Core.Nothing, stackId = Core.Nothing}

-- | The instance ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiInstanceId :: Lens.Lens' DescribeElasticIps (Core.Maybe Core.Text)
deiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE deiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | An array of Elastic IP addresses to be described. If you include this parameter, @DescribeElasticIps@ returns a description of the specified Elastic IP addresses. Otherwise, it returns a description of every Elastic IP address.
--
-- /Note:/ Consider using 'ips' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiIps :: Lens.Lens' DescribeElasticIps (Core.Maybe [Core.Text])
deiIps = Lens.field @"ips"
{-# INLINEABLE deiIps #-}
{-# DEPRECATED ips "Use generic-lens or generic-optics with 'ips' instead"  #-}

-- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses that are registered with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiStackId :: Lens.Lens' DescribeElasticIps (Core.Maybe Core.Text)
deiStackId = Lens.field @"stackId"
{-# INLINEABLE deiStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery DescribeElasticIps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeElasticIps where
        toHeaders DescribeElasticIps{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DescribeElasticIps")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeElasticIps where
        toJSON DescribeElasticIps{..}
          = Core.object
              (Core.catMaybes
                 [("InstanceId" Core..=) Core.<$> instanceId,
                  ("Ips" Core..=) Core.<$> ips,
                  ("StackId" Core..=) Core.<$> stackId])

instance Core.AWSRequest DescribeElasticIps where
        type Rs DescribeElasticIps = DescribeElasticIpsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeElasticIpsResponse' Core.<$>
                   (x Core..:? "ElasticIps") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeElasticIps@ request.
--
-- /See:/ 'mkDescribeElasticIpsResponse' smart constructor.
data DescribeElasticIpsResponse = DescribeElasticIpsResponse'
  { elasticIps :: Core.Maybe [Types.ElasticIp]
    -- ^ An @ElasticIps@ object that describes the specified Elastic IP addresses.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticIpsResponse' value with any optional fields omitted.
mkDescribeElasticIpsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeElasticIpsResponse
mkDescribeElasticIpsResponse responseStatus
  = DescribeElasticIpsResponse'{elasticIps = Core.Nothing,
                                responseStatus}

-- | An @ElasticIps@ object that describes the specified Elastic IP addresses.
--
-- /Note:/ Consider using 'elasticIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deirrsElasticIps :: Lens.Lens' DescribeElasticIpsResponse (Core.Maybe [Types.ElasticIp])
deirrsElasticIps = Lens.field @"elasticIps"
{-# INLINEABLE deirrsElasticIps #-}
{-# DEPRECATED elasticIps "Use generic-lens or generic-optics with 'elasticIps' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deirrsResponseStatus :: Lens.Lens' DescribeElasticIpsResponse Core.Int
deirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE deirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
