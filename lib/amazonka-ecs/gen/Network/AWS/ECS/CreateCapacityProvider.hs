{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.CreateCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new capacity provider. Capacity providers are associated with an Amazon ECS cluster and are used in capacity provider strategies to facilitate cluster auto scaling.
--
-- Only capacity providers using an Auto Scaling group can be created. Amazon ECS tasks on AWS Fargate use the @FARGATE@ and @FARGATE_SPOT@ capacity providers which are already created and available to all accounts in Regions supported by AWS Fargate.
module Network.AWS.ECS.CreateCapacityProvider
    (
    -- * Creating a request
      CreateCapacityProvider (..)
    , mkCreateCapacityProvider
    -- ** Request lenses
    , ccpName
    , ccpAutoScalingGroupProvider
    , ccpTags

    -- * Destructuring the response
    , CreateCapacityProviderResponse (..)
    , mkCreateCapacityProviderResponse
    -- ** Response lenses
    , ccprrsCapacityProvider
    , ccprrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCapacityProvider' smart constructor.
data CreateCapacityProvider = CreateCapacityProvider'
  { name :: Core.Text
    -- ^ The name of the capacity provider. Up to 255 characters are allowed, including letters (upper and lowercase), numbers, underscores, and hyphens. The name cannot be prefixed with "@aws@ ", "@ecs@ ", or "@fargate@ ".
  , autoScalingGroupProvider :: Types.AutoScalingGroupProvider
    -- ^ The details of the Auto Scaling group for the capacity provider.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The metadata that you apply to the capacity provider to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCapacityProvider' value with any optional fields omitted.
mkCreateCapacityProvider
    :: Core.Text -- ^ 'name'
    -> Types.AutoScalingGroupProvider -- ^ 'autoScalingGroupProvider'
    -> CreateCapacityProvider
mkCreateCapacityProvider name autoScalingGroupProvider
  = CreateCapacityProvider'{name, autoScalingGroupProvider,
                            tags = Core.Nothing}

-- | The name of the capacity provider. Up to 255 characters are allowed, including letters (upper and lowercase), numbers, underscores, and hyphens. The name cannot be prefixed with "@aws@ ", "@ecs@ ", or "@fargate@ ".
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpName :: Lens.Lens' CreateCapacityProvider Core.Text
ccpName = Lens.field @"name"
{-# INLINEABLE ccpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The details of the Auto Scaling group for the capacity provider.
--
-- /Note:/ Consider using 'autoScalingGroupProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpAutoScalingGroupProvider :: Lens.Lens' CreateCapacityProvider Types.AutoScalingGroupProvider
ccpAutoScalingGroupProvider = Lens.field @"autoScalingGroupProvider"
{-# INLINEABLE ccpAutoScalingGroupProvider #-}
{-# DEPRECATED autoScalingGroupProvider "Use generic-lens or generic-optics with 'autoScalingGroupProvider' instead"  #-}

-- | The metadata that you apply to the capacity provider to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpTags :: Lens.Lens' CreateCapacityProvider (Core.Maybe [Types.Tag])
ccpTags = Lens.field @"tags"
{-# INLINEABLE ccpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCapacityProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCapacityProvider where
        toHeaders CreateCapacityProvider{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.CreateCapacityProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCapacityProvider where
        toJSON CreateCapacityProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just
                    ("autoScalingGroupProvider" Core..= autoScalingGroupProvider),
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCapacityProvider where
        type Rs CreateCapacityProvider = CreateCapacityProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCapacityProviderResponse' Core.<$>
                   (x Core..:? "capacityProvider") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCapacityProviderResponse' smart constructor.
data CreateCapacityProviderResponse = CreateCapacityProviderResponse'
  { capacityProvider :: Core.Maybe Types.CapacityProvider
    -- ^ The full description of the new capacity provider.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCapacityProviderResponse' value with any optional fields omitted.
mkCreateCapacityProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCapacityProviderResponse
mkCreateCapacityProviderResponse responseStatus
  = CreateCapacityProviderResponse'{capacityProvider = Core.Nothing,
                                    responseStatus}

-- | The full description of the new capacity provider.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsCapacityProvider :: Lens.Lens' CreateCapacityProviderResponse (Core.Maybe Types.CapacityProvider)
ccprrsCapacityProvider = Lens.field @"capacityProvider"
{-# INLINEABLE ccprrsCapacityProvider #-}
{-# DEPRECATED capacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsResponseStatus :: Lens.Lens' CreateCapacityProviderResponse Core.Int
ccprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
