{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerRuntime.InvokeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- After you deploy a model into production using Amazon SageMaker hosting services, your client applications use this API to get inferences from the model hosted at the specified endpoint. 
--
-- For an overview of Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> . 
-- Amazon SageMaker strips all POST headers except those supported by the API. Amazon SageMaker might add additional headers. You should not rely on the behavior of headers outside those enumerated in the request syntax. 
-- Calls to @InvokeEndpoint@ are authenticated by using AWS Signature Version 4. For information, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> in the /Amazon S3 API Reference/ .
-- A customer's model containers must respond to requests within 60 seconds. The model itself can have a maximum processing time of 60 seconds before responding to the /invocations. If your model is going to take 50-60 seconds of processing time, the SDK socket timeout should be set to be 70 seconds.
module Network.AWS.SageMakerRuntime.InvokeEndpoint
    (
    -- * Creating a request
      InvokeEndpoint (..)
    , mkInvokeEndpoint
    -- ** Request lenses
    , ieEndpointName
    , ieBody
    , ieAccept
    , ieContentType
    , ieCustomAttributes
    , ieTargetModel
    , ieTargetVariant

    -- * Destructuring the response
    , InvokeEndpointResponse (..)
    , mkInvokeEndpointResponse
    -- ** Response lenses
    , ierrsBody
    , ierrsContentType
    , ierrsCustomAttributes
    , ierrsInvokedProductionVariant
    , ierrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMakerRuntime.Types as Types

-- | /See:/ 'mkInvokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { endpointName :: Types.EndpointName
    -- ^ The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API. 
  , body :: Types.ByteString
    -- ^ Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model. 
--
-- For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
  , accept :: Core.Maybe Types.Header
    -- ^ The desired MIME type of the inference in the response.
  , contentType :: Core.Maybe Types.Header
    -- ^ The MIME type of the input data in the request body.
  , customAttributes :: Core.Maybe Types.CustomAttributesHeader
    -- ^ Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
  , targetModel :: Core.Maybe Types.TargetModelHeader
    -- ^ The model to request for inference when invoking a multi-model endpoint. 
  , targetVariant :: Core.Maybe Types.TargetVariant
    -- ^ Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InvokeEndpoint' value with any optional fields omitted.
mkInvokeEndpoint
    :: Types.EndpointName -- ^ 'endpointName'
    -> Types.ByteString -- ^ 'body'
    -> InvokeEndpoint
mkInvokeEndpoint endpointName body
  = InvokeEndpoint'{endpointName, body, accept = Core.Nothing,
                    contentType = Core.Nothing, customAttributes = Core.Nothing,
                    targetModel = Core.Nothing, targetVariant = Core.Nothing}

-- | The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API. 
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieEndpointName :: Lens.Lens' InvokeEndpoint Types.EndpointName
ieEndpointName = Lens.field @"endpointName"
{-# INLINEABLE ieEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model. 
--
-- For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieBody :: Lens.Lens' InvokeEndpoint Types.ByteString
ieBody = Lens.field @"body"
{-# INLINEABLE ieBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The desired MIME type of the inference in the response.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieAccept :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.Header)
ieAccept = Lens.field @"accept"
{-# INLINEABLE ieAccept #-}
{-# DEPRECATED accept "Use generic-lens or generic-optics with 'accept' instead"  #-}

-- | The MIME type of the input data in the request body.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieContentType :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.Header)
ieContentType = Lens.field @"contentType"
{-# INLINEABLE ieContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieCustomAttributes :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.CustomAttributesHeader)
ieCustomAttributes = Lens.field @"customAttributes"
{-# INLINEABLE ieCustomAttributes #-}
{-# DEPRECATED customAttributes "Use generic-lens or generic-optics with 'customAttributes' instead"  #-}

-- | The model to request for inference when invoking a multi-model endpoint. 
--
-- /Note:/ Consider using 'targetModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTargetModel :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.TargetModelHeader)
ieTargetModel = Lens.field @"targetModel"
{-# INLINEABLE ieTargetModel #-}
{-# DEPRECATED targetModel "Use generic-lens or generic-optics with 'targetModel' instead"  #-}

-- | Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
--
-- /Note:/ Consider using 'targetVariant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTargetVariant :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.TargetVariant)
ieTargetVariant = Lens.field @"targetVariant"
{-# INLINEABLE ieTargetVariant #-}
{-# DEPRECATED targetVariant "Use generic-lens or generic-optics with 'targetVariant' instead"  #-}

instance Core.ToQuery InvokeEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InvokeEndpoint where
        toHeaders InvokeEndpoint{..}
          = Core.toHeaders "Accept" accept Core.<>
              Core.toHeaders "Content-Type" contentType
              Core.<>
              Core.toHeaders "X-Amzn-SageMaker-Custom-Attributes"
                customAttributes
              Core.<> Core.toHeaders "X-Amzn-SageMaker-Target-Model" targetModel
              Core.<>
              Core.toHeaders "X-Amzn-SageMaker-Target-Variant" targetVariant

instance Core.AWSRequest InvokeEndpoint where
        type Rs InvokeEndpoint = InvokeEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/endpoints/" Core.<> Core.toText endpointName Core.<>
                             "/invocations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBytes
              (\ s h x ->
                 InvokeEndpointResponse' Core.<$>
                   (Core.pure x) Core.<*> Core.parseHeaderMaybe "Content-Type" h
                     Core.<*>
                     Core.parseHeaderMaybe "X-Amzn-SageMaker-Custom-Attributes" h
                     Core.<*>
                     Core.parseHeaderMaybe "x-Amzn-Invoked-Production-Variant" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkInvokeEndpointResponse' smart constructor.
data InvokeEndpointResponse = InvokeEndpointResponse'
  { body :: Types.ByteString
    -- ^ Includes the inference provided by the model.
--
-- For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
  , contentType :: Core.Maybe Types.ContentType
    -- ^ The MIME type of the inference returned in the response body.
  , customAttributes :: Core.Maybe Types.CustomAttributes
    -- ^ Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back. 
--
-- This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
  , invokedProductionVariant :: Core.Maybe Types.InvokedProductionVariant
    -- ^ Identifies the production variant that was invoked.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InvokeEndpointResponse' value with any optional fields omitted.
mkInvokeEndpointResponse
    :: Types.ByteString -- ^ 'body'
    -> Core.Int -- ^ 'responseStatus'
    -> InvokeEndpointResponse
mkInvokeEndpointResponse body responseStatus
  = InvokeEndpointResponse'{body, contentType = Core.Nothing,
                            customAttributes = Core.Nothing,
                            invokedProductionVariant = Core.Nothing, responseStatus}

-- | Includes the inference provided by the model.
--
-- For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsBody :: Lens.Lens' InvokeEndpointResponse Types.ByteString
ierrsBody = Lens.field @"body"
{-# INLINEABLE ierrsBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The MIME type of the inference returned in the response body.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsContentType :: Lens.Lens' InvokeEndpointResponse (Core.Maybe Types.ContentType)
ierrsContentType = Lens.field @"contentType"
{-# INLINEABLE ierrsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back. 
--
-- This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsCustomAttributes :: Lens.Lens' InvokeEndpointResponse (Core.Maybe Types.CustomAttributes)
ierrsCustomAttributes = Lens.field @"customAttributes"
{-# INLINEABLE ierrsCustomAttributes #-}
{-# DEPRECATED customAttributes "Use generic-lens or generic-optics with 'customAttributes' instead"  #-}

-- | Identifies the production variant that was invoked.
--
-- /Note:/ Consider using 'invokedProductionVariant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsInvokedProductionVariant :: Lens.Lens' InvokeEndpointResponse (Core.Maybe Types.InvokedProductionVariant)
ierrsInvokedProductionVariant = Lens.field @"invokedProductionVariant"
{-# INLINEABLE ierrsInvokedProductionVariant #-}
{-# DEPRECATED invokedProductionVariant "Use generic-lens or generic-optics with 'invokedProductionVariant' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsResponseStatus :: Lens.Lens' InvokeEndpointResponse Core.Int
ierrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ierrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
