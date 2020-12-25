{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.CreateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model-specific endpoint for synchronous inference for a previously trained custom model
module Network.AWS.Comprehend.CreateEndpoint
  ( -- * Creating a request
    CreateEndpoint (..),
    mkCreateEndpoint,

    -- ** Request lenses
    ceEndpointName,
    ceModelArn,
    ceDesiredInferenceUnits,
    ceClientRequestToken,
    ceTags,

    -- * Destructuring the response
    CreateEndpointResponse (..),
    mkCreateEndpointResponse,

    -- ** Response lenses
    cerrsEndpointArn,
    cerrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource.
    endpointName :: Types.ComprehendEndpointName,
    -- | The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
    modelArn :: Types.ComprehendModelArn,
    -- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
    desiredInferenceUnits :: Core.Natural,
    -- | An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ .
    clientRequestToken :: Core.Maybe Types.ClientRequestTokenString,
    -- | Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpoint' value with any optional fields omitted.
mkCreateEndpoint ::
  -- | 'endpointName'
  Types.ComprehendEndpointName ->
  -- | 'modelArn'
  Types.ComprehendModelArn ->
  -- | 'desiredInferenceUnits'
  Core.Natural ->
  CreateEndpoint
mkCreateEndpoint endpointName modelArn desiredInferenceUnits =
  CreateEndpoint'
    { endpointName,
      modelArn,
      desiredInferenceUnits,
      clientRequestToken = Core.Nothing,
      tags = Core.Nothing
    }

-- | This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointName :: Lens.Lens' CreateEndpoint Types.ComprehendEndpointName
ceEndpointName = Lens.field @"endpointName"
{-# DEPRECATED ceEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
--
-- /Note:/ Consider using 'modelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceModelArn :: Lens.Lens' CreateEndpoint Types.ComprehendModelArn
ceModelArn = Lens.field @"modelArn"
{-# DEPRECATED ceModelArn "Use generic-lens or generic-optics with 'modelArn' instead." #-}

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDesiredInferenceUnits :: Lens.Lens' CreateEndpoint Core.Natural
ceDesiredInferenceUnits = Lens.field @"desiredInferenceUnits"
{-# DEPRECATED ceDesiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead." #-}

-- | An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceClientRequestToken :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ClientRequestTokenString)
ceClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED ceClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Core.Maybe [Types.Tag])
ceTags = Lens.field @"tags"
{-# DEPRECATED ceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateEndpoint where
  toJSON CreateEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointName" Core..= endpointName),
            Core.Just ("ModelArn" Core..= modelArn),
            Core.Just ("DesiredInferenceUnits" Core..= desiredInferenceUnits),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Comprehend_20171127.CreateEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Core.<$> (x Core..:? "EndpointArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The Amazon Resource Number (ARN) of the endpoint being created.
    endpointArn :: Core.Maybe Types.EndpointArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpointResponse' value with any optional fields omitted.
mkCreateEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateEndpointResponse
mkCreateEndpointResponse responseStatus =
  CreateEndpointResponse'
    { endpointArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Number (ARN) of the endpoint being created.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsEndpointArn :: Lens.Lens' CreateEndpointResponse (Core.Maybe Types.EndpointArn)
cerrsEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED cerrsEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsResponseStatus :: Lens.Lens' CreateEndpointResponse Core.Int
cerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
