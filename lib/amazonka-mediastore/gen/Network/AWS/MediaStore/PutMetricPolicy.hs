{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutMetricPolicy (..),
    mkPutMetricPolicy,

    -- ** Request lenses
    pmpContainerName,
    pmpMetricPolicy,

    -- * Destructuring the response
    PutMetricPolicyResponse (..),
    mkPutMetricPolicyResponse,

    -- ** Response lenses
    pmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutMetricPolicy' smart constructor.
data PutMetricPolicy = PutMetricPolicy'
  { -- | The name of the container that you want to add the metric policy to.
    containerName :: Types.ContainerName,
    -- | The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:
    --
    --
    --     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.
    --
    --
    --     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
    metricPolicy :: Types.MetricPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricPolicy' value with any optional fields omitted.
mkPutMetricPolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  -- | 'metricPolicy'
  Types.MetricPolicy ->
  PutMetricPolicy
mkPutMetricPolicy containerName metricPolicy =
  PutMetricPolicy' {containerName, metricPolicy}

-- | The name of the container that you want to add the metric policy to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpContainerName :: Lens.Lens' PutMetricPolicy Types.ContainerName
pmpContainerName = Lens.field @"containerName"
{-# DEPRECATED pmpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

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
{-# DEPRECATED pmpMetricPolicy "Use generic-lens or generic-optics with 'metricPolicy' instead." #-}

instance Core.FromJSON PutMetricPolicy where
  toJSON PutMetricPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContainerName" Core..= containerName),
            Core.Just ("MetricPolicy" Core..= metricPolicy)
          ]
      )

instance Core.AWSRequest PutMetricPolicy where
  type Rs PutMetricPolicy = PutMetricPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "MediaStore_20170901.PutMetricPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutMetricPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutMetricPolicyResponse' smart constructor.
newtype PutMetricPolicyResponse = PutMetricPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricPolicyResponse' value with any optional fields omitted.
mkPutMetricPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutMetricPolicyResponse
mkPutMetricPolicyResponse responseStatus =
  PutMetricPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmprrsResponseStatus :: Lens.Lens' PutMetricPolicyResponse Core.Int
pmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
