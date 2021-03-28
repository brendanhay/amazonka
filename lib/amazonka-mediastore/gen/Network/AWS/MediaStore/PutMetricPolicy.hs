{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.PutMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The metric policy that you want to add to the container. A metric policy allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. It takes up to 20 minutes for the new policy to take effect.
module Network.AWS.MediaStore.PutMetricPolicy
    (
    -- * Creating a request
      PutMetricPolicy (..)
    , mkPutMetricPolicy
    -- ** Request lenses
    , pmpContainerName
    , pmpMetricPolicy

    -- * Destructuring the response
    , PutMetricPolicyResponse (..)
    , mkPutMetricPolicyResponse
    -- ** Response lenses
    , pmprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutMetricPolicy' smart constructor.
data PutMetricPolicy = PutMetricPolicy'
  { containerName :: Types.ContainerName
    -- ^ The name of the container that you want to add the metric policy to.
  , metricPolicy :: Types.MetricPolicy
    -- ^ The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:
--
--
--     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.
--
--
--     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricPolicy' value with any optional fields omitted.
mkPutMetricPolicy
    :: Types.ContainerName -- ^ 'containerName'
    -> Types.MetricPolicy -- ^ 'metricPolicy'
    -> PutMetricPolicy
mkPutMetricPolicy containerName metricPolicy
  = PutMetricPolicy'{containerName, metricPolicy}

-- | The name of the container that you want to add the metric policy to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpContainerName :: Lens.Lens' PutMetricPolicy Types.ContainerName
pmpContainerName = Lens.field @"containerName"
{-# INLINEABLE pmpContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

-- | The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:
--
--
--     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.
--
--
--     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
--
--
--
-- /Note:/ Consider using 'metricPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpMetricPolicy :: Lens.Lens' PutMetricPolicy Types.MetricPolicy
pmpMetricPolicy = Lens.field @"metricPolicy"
{-# INLINEABLE pmpMetricPolicy #-}
{-# DEPRECATED metricPolicy "Use generic-lens or generic-optics with 'metricPolicy' instead"  #-}

instance Core.ToQuery PutMetricPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutMetricPolicy where
        toHeaders PutMetricPolicy{..}
          = Core.pure ("X-Amz-Target", "MediaStore_20170901.PutMetricPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutMetricPolicy where
        toJSON PutMetricPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName),
                  Core.Just ("MetricPolicy" Core..= metricPolicy)])

instance Core.AWSRequest PutMetricPolicy where
        type Rs PutMetricPolicy = PutMetricPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutMetricPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutMetricPolicyResponse' smart constructor.
newtype PutMetricPolicyResponse = PutMetricPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricPolicyResponse' value with any optional fields omitted.
mkPutMetricPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutMetricPolicyResponse
mkPutMetricPolicyResponse responseStatus
  = PutMetricPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmprrsResponseStatus :: Lens.Lens' PutMetricPolicyResponse Core.Int
pmprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pmprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
