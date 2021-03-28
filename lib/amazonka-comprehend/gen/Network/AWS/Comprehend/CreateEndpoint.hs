{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateEndpoint (..)
    , mkCreateEndpoint
    -- ** Request lenses
    , ceEndpointName
    , ceModelArn
    , ceDesiredInferenceUnits
    , ceClientRequestToken
    , ceTags

    -- * Destructuring the response
    , CreateEndpointResponse (..)
    , mkCreateEndpointResponse
    -- ** Response lenses
    , cerrsEndpointArn
    , cerrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { endpointName :: Types.ComprehendEndpointName
    -- ^ This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource. 
  , modelArn :: Types.ComprehendModelArn
    -- ^ The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
  , desiredInferenceUnits :: Core.Natural
    -- ^ The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
  , clientRequestToken :: Core.Maybe Types.ClientRequestTokenString
    -- ^ An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ . 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpoint' value with any optional fields omitted.
mkCreateEndpoint
    :: Types.ComprehendEndpointName -- ^ 'endpointName'
    -> Types.ComprehendModelArn -- ^ 'modelArn'
    -> Core.Natural -- ^ 'desiredInferenceUnits'
    -> CreateEndpoint
mkCreateEndpoint endpointName modelArn desiredInferenceUnits
  = CreateEndpoint'{endpointName, modelArn, desiredInferenceUnits,
                    clientRequestToken = Core.Nothing, tags = Core.Nothing}

-- | This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource. 
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointName :: Lens.Lens' CreateEndpoint Types.ComprehendEndpointName
ceEndpointName = Lens.field @"endpointName"
{-# INLINEABLE ceEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
--
-- /Note:/ Consider using 'modelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceModelArn :: Lens.Lens' CreateEndpoint Types.ComprehendModelArn
ceModelArn = Lens.field @"modelArn"
{-# INLINEABLE ceModelArn #-}
{-# DEPRECATED modelArn "Use generic-lens or generic-optics with 'modelArn' instead"  #-}

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDesiredInferenceUnits :: Lens.Lens' CreateEndpoint Core.Natural
ceDesiredInferenceUnits = Lens.field @"desiredInferenceUnits"
{-# INLINEABLE ceDesiredInferenceUnits #-}
{-# DEPRECATED desiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead"  #-}

-- | An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ . 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceClientRequestToken :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ClientRequestTokenString)
ceClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE ceClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department. 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Core.Maybe [Types.Tag])
ceTags = Lens.field @"tags"
{-# INLINEABLE ceTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateEndpoint where
        toHeaders CreateEndpoint{..}
          = Core.pure ("X-Amz-Target", "Comprehend_20171127.CreateEndpoint")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateEndpoint where
        toJSON CreateEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointName" Core..= endpointName),
                  Core.Just ("ModelArn" Core..= modelArn),
                  Core.Just ("DesiredInferenceUnits" Core..= desiredInferenceUnits),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateEndpoint where
        type Rs CreateEndpoint = CreateEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateEndpointResponse' Core.<$>
                   (x Core..:? "EndpointArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { endpointArn :: Core.Maybe Types.EndpointArn
    -- ^ The Amazon Resource Number (ARN) of the endpoint being created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpointResponse' value with any optional fields omitted.
mkCreateEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateEndpointResponse
mkCreateEndpointResponse responseStatus
  = CreateEndpointResponse'{endpointArn = Core.Nothing,
                            responseStatus}

-- | The Amazon Resource Number (ARN) of the endpoint being created.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsEndpointArn :: Lens.Lens' CreateEndpointResponse (Core.Maybe Types.EndpointArn)
cerrsEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE cerrsEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsResponseStatus :: Lens.Lens' CreateEndpointResponse Core.Int
cerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
