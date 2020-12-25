{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    InvokeEndpoint (..),
    mkInvokeEndpoint,

    -- ** Request lenses
    ieEndpointName,
    ieBody,
    ieAccept,
    ieContentType,
    ieCustomAttributes,
    ieTargetModel,
    ieTargetVariant,

    -- * Destructuring the response
    InvokeEndpointResponse (..),
    mkInvokeEndpointResponse,

    -- ** Response lenses
    ierrsBody,
    ierrsContentType,
    ierrsCustomAttributes,
    ierrsInvokedProductionVariant,
    ierrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMakerRuntime.Types as Types

-- | /See:/ 'mkInvokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { -- | The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
    endpointName :: Types.EndpointName,
    -- | Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.
    --
    -- For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
    body :: Types.ByteString,
    -- | The desired MIME type of the inference in the response.
    accept :: Core.Maybe Types.Header,
    -- | The MIME type of the input data in the request body.
    contentType :: Core.Maybe Types.Header,
    -- | Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
    customAttributes :: Core.Maybe Types.CustomAttributesHeader,
    -- | The model to request for inference when invoking a multi-model endpoint.
    targetModel :: Core.Maybe Types.TargetModelHeader,
    -- | Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
    targetVariant :: Core.Maybe Types.TargetVariant
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InvokeEndpoint' value with any optional fields omitted.
mkInvokeEndpoint ::
  -- | 'endpointName'
  Types.EndpointName ->
  -- | 'body'
  Types.ByteString ->
  InvokeEndpoint
mkInvokeEndpoint endpointName body =
  InvokeEndpoint'
    { endpointName,
      body,
      accept = Core.Nothing,
      contentType = Core.Nothing,
      customAttributes = Core.Nothing,
      targetModel = Core.Nothing,
      targetVariant = Core.Nothing
    }

-- | The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieEndpointName :: Lens.Lens' InvokeEndpoint Types.EndpointName
ieEndpointName = Lens.field @"endpointName"
{-# DEPRECATED ieEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.
--
-- For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieBody :: Lens.Lens' InvokeEndpoint Types.ByteString
ieBody = Lens.field @"body"
{-# DEPRECATED ieBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The desired MIME type of the inference in the response.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieAccept :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.Header)
ieAccept = Lens.field @"accept"
{-# DEPRECATED ieAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

-- | The MIME type of the input data in the request body.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieContentType :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.Header)
ieContentType = Lens.field @"contentType"
{-# DEPRECATED ieContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieCustomAttributes :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.CustomAttributesHeader)
ieCustomAttributes = Lens.field @"customAttributes"
{-# DEPRECATED ieCustomAttributes "Use generic-lens or generic-optics with 'customAttributes' instead." #-}

-- | The model to request for inference when invoking a multi-model endpoint.
--
-- /Note:/ Consider using 'targetModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTargetModel :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.TargetModelHeader)
ieTargetModel = Lens.field @"targetModel"
{-# DEPRECATED ieTargetModel "Use generic-lens or generic-optics with 'targetModel' instead." #-}

-- | Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
--
-- /Note:/ Consider using 'targetVariant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTargetVariant :: Lens.Lens' InvokeEndpoint (Core.Maybe Types.TargetVariant)
ieTargetVariant = Lens.field @"targetVariant"
{-# DEPRECATED ieTargetVariant "Use generic-lens or generic-optics with 'targetVariant' instead." #-}

instance Core.AWSRequest InvokeEndpoint where
  type Rs InvokeEndpoint = InvokeEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/endpoints/" Core.<> (Core.toText endpointName)
                Core.<> ("/invocations")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Accept" accept
            Core.<> (Core.toHeaders "Content-Type" contentType)
            Core.<> ( Core.toHeaders
                        "X-Amzn-SageMaker-Custom-Attributes"
                        customAttributes
                    )
            Core.<> (Core.toHeaders "X-Amzn-SageMaker-Target-Model" targetModel)
            Core.<> (Core.toHeaders "X-Amzn-SageMaker-Target-Variant" targetVariant),
        Core._rqBody = Core.toBody body
      }
  response =
    Response.receiveBytes
      ( \s h x ->
          InvokeEndpointResponse'
            Core.<$> (Core.pure x)
            Core.<*> (Core.parseHeaderMaybe "Content-Type" h)
            Core.<*> (Core.parseHeaderMaybe "X-Amzn-SageMaker-Custom-Attributes" h)
            Core.<*> (Core.parseHeaderMaybe "x-Amzn-Invoked-Production-Variant" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkInvokeEndpointResponse' smart constructor.
data InvokeEndpointResponse = InvokeEndpointResponse'
  { -- | Includes the inference provided by the model.
    --
    -- For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
    body :: Types.ByteString,
    -- | The MIME type of the inference returned in the response body.
    contentType :: Core.Maybe Types.ContentType,
    -- | Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back.
    --
    -- This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
    customAttributes :: Core.Maybe Types.CustomAttributes,
    -- | Identifies the production variant that was invoked.
    invokedProductionVariant :: Core.Maybe Types.InvokedProductionVariant,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InvokeEndpointResponse' value with any optional fields omitted.
mkInvokeEndpointResponse ::
  -- | 'body'
  Types.ByteString ->
  -- | 'responseStatus'
  Core.Int ->
  InvokeEndpointResponse
mkInvokeEndpointResponse body responseStatus =
  InvokeEndpointResponse'
    { body,
      contentType = Core.Nothing,
      customAttributes = Core.Nothing,
      invokedProductionVariant = Core.Nothing,
      responseStatus
    }

-- | Includes the inference provided by the model.
--
-- For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsBody :: Lens.Lens' InvokeEndpointResponse Types.ByteString
ierrsBody = Lens.field @"body"
{-# DEPRECATED ierrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The MIME type of the inference returned in the response body.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsContentType :: Lens.Lens' InvokeEndpointResponse (Core.Maybe Types.ContentType)
ierrsContentType = Lens.field @"contentType"
{-# DEPRECATED ierrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back.
--
-- This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsCustomAttributes :: Lens.Lens' InvokeEndpointResponse (Core.Maybe Types.CustomAttributes)
ierrsCustomAttributes = Lens.field @"customAttributes"
{-# DEPRECATED ierrsCustomAttributes "Use generic-lens or generic-optics with 'customAttributes' instead." #-}

-- | Identifies the production variant that was invoked.
--
-- /Note:/ Consider using 'invokedProductionVariant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsInvokedProductionVariant :: Lens.Lens' InvokeEndpointResponse (Core.Maybe Types.InvokedProductionVariant)
ierrsInvokedProductionVariant = Lens.field @"invokedProductionVariant"
{-# DEPRECATED ierrsInvokedProductionVariant "Use generic-lens or generic-optics with 'invokedProductionVariant' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ierrsResponseStatus :: Lens.Lens' InvokeEndpointResponse Core.Int
ierrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ierrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
