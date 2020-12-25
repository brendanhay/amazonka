{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys the new @EndpointConfig@ specified in the request, switches to using newly created endpoint, and then deletes resources provisioned for the endpoint using the previous @EndpointConfig@ (there is no availability loss).
--
-- When Amazon SageMaker receives the request, it sets the endpoint status to @Updating@ . After updating the endpoint, it sets the status to @InService@ . To check the status of an endpoint, use the 'DescribeEndpoint' API.
module Network.AWS.SageMaker.UpdateEndpoint
  ( -- * Creating a request
    UpdateEndpoint (..),
    mkUpdateEndpoint,

    -- ** Request lenses
    ueEndpointName,
    ueEndpointConfigName,
    ueExcludeRetainedVariantProperties,
    ueRetainAllVariantProperties,

    -- * Destructuring the response
    UpdateEndpointResponse (..),
    mkUpdateEndpointResponse,

    -- ** Response lenses
    uerfrsEndpointArn,
    uerfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | The name of the endpoint whose configuration you want to update.
    endpointName :: Types.EndpointName,
    -- | The name of the new endpoint configuration.
    endpointConfigName :: Types.EndpointConfigName,
    -- | When you are updating endpoint resources with 'UpdateEndpointInput$RetainAllVariantProperties' , whose value is set to @true@ , @ExcludeRetainedVariantProperties@ specifies the list of type 'VariantProperty' to override with the values provided by @EndpointConfig@ . If you don't specify a value for @ExcludeAllVariantProperties@ , no variant properties are overridden.
    excludeRetainedVariantProperties :: Core.Maybe [Types.VariantProperty],
    -- | When updating endpoint resources, enables or disables the retention of variant properties, such as the instance count or the variant weight. To retain the variant properties of an endpoint when updating it, set @RetainAllVariantProperties@ to @true@ . To use the variant properties specified in a new @EndpointConfig@ call when updating an endpoint, set @RetainAllVariantProperties@ to @false@ .
    retainAllVariantProperties :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpoint' value with any optional fields omitted.
mkUpdateEndpoint ::
  -- | 'endpointName'
  Types.EndpointName ->
  -- | 'endpointConfigName'
  Types.EndpointConfigName ->
  UpdateEndpoint
mkUpdateEndpoint endpointName endpointConfigName =
  UpdateEndpoint'
    { endpointName,
      endpointConfigName,
      excludeRetainedVariantProperties = Core.Nothing,
      retainAllVariantProperties = Core.Nothing
    }

-- | The name of the endpoint whose configuration you want to update.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointName :: Lens.Lens' UpdateEndpoint Types.EndpointName
ueEndpointName = Lens.field @"endpointName"
{-# DEPRECATED ueEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The name of the new endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointConfigName :: Lens.Lens' UpdateEndpoint Types.EndpointConfigName
ueEndpointConfigName = Lens.field @"endpointConfigName"
{-# DEPRECATED ueEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | When you are updating endpoint resources with 'UpdateEndpointInput$RetainAllVariantProperties' , whose value is set to @true@ , @ExcludeRetainedVariantProperties@ specifies the list of type 'VariantProperty' to override with the values provided by @EndpointConfig@ . If you don't specify a value for @ExcludeAllVariantProperties@ , no variant properties are overridden.
--
-- /Note:/ Consider using 'excludeRetainedVariantProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueExcludeRetainedVariantProperties :: Lens.Lens' UpdateEndpoint (Core.Maybe [Types.VariantProperty])
ueExcludeRetainedVariantProperties = Lens.field @"excludeRetainedVariantProperties"
{-# DEPRECATED ueExcludeRetainedVariantProperties "Use generic-lens or generic-optics with 'excludeRetainedVariantProperties' instead." #-}

-- | When updating endpoint resources, enables or disables the retention of variant properties, such as the instance count or the variant weight. To retain the variant properties of an endpoint when updating it, set @RetainAllVariantProperties@ to @true@ . To use the variant properties specified in a new @EndpointConfig@ call when updating an endpoint, set @RetainAllVariantProperties@ to @false@ .
--
-- /Note:/ Consider using 'retainAllVariantProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueRetainAllVariantProperties :: Lens.Lens' UpdateEndpoint (Core.Maybe Core.Bool)
ueRetainAllVariantProperties = Lens.field @"retainAllVariantProperties"
{-# DEPRECATED ueRetainAllVariantProperties "Use generic-lens or generic-optics with 'retainAllVariantProperties' instead." #-}

instance Core.FromJSON UpdateEndpoint where
  toJSON UpdateEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointName" Core..= endpointName),
            Core.Just ("EndpointConfigName" Core..= endpointConfigName),
            ("ExcludeRetainedVariantProperties" Core..=)
              Core.<$> excludeRetainedVariantProperties,
            ("RetainAllVariantProperties" Core..=)
              Core.<$> retainAllVariantProperties
          ]
      )

instance Core.AWSRequest UpdateEndpoint where
  type Rs UpdateEndpoint = UpdateEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointResponse'
            Core.<$> (x Core..: "EndpointArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Types.EndpointArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointResponse' value with any optional fields omitted.
mkUpdateEndpointResponse ::
  -- | 'endpointArn'
  Types.EndpointArn ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateEndpointResponse
mkUpdateEndpointResponse endpointArn responseStatus =
  UpdateEndpointResponse' {endpointArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerfrsEndpointArn :: Lens.Lens' UpdateEndpointResponse Types.EndpointArn
uerfrsEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED uerfrsEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerfrsResponseStatus :: Lens.Lens' UpdateEndpointResponse Core.Int
uerfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uerfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
