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
    ieTargetModel,
    ieCustomAttributes,
    ieTargetVariant,
    ieContentType,

    -- * Destructuring the response
    InvokeEndpointResponse (..),
    mkInvokeEndpointResponse,

    -- ** Response lenses
    iersInvokedProductionVariant,
    iersBody,
    iersCustomAttributes,
    iersContentType,
    iersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMakerRuntime.Types

-- | /See:/ 'mkInvokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { -- | The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
    endpointName :: Lude.Text,
    -- | Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.
    --
    -- For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
    body :: ByteString,
    -- | The desired MIME type of the inference in the response.
    accept :: Lude.Maybe Lude.Text,
    -- | The model to request for inference when invoking a multi-model endpoint.
    targetModel :: Lude.Maybe Lude.Text,
    -- | Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
    customAttributes :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
    targetVariant :: Lude.Maybe Lude.Text,
    -- | The MIME type of the input data in the request body.
    contentType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InvokeEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
-- * 'body' - Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.
--
-- For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
-- * 'accept' - The desired MIME type of the inference in the response.
-- * 'targetModel' - The model to request for inference when invoking a multi-model endpoint.
-- * 'customAttributes' - Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
-- * 'targetVariant' - Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
-- * 'contentType' - The MIME type of the input data in the request body.
mkInvokeEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'body'
  ByteString ->
  InvokeEndpoint
mkInvokeEndpoint pEndpointName_ pBody_ =
  InvokeEndpoint'
    { endpointName = pEndpointName_,
      body = pBody_,
      accept = Lude.Nothing,
      targetModel = Lude.Nothing,
      customAttributes = Lude.Nothing,
      targetVariant = Lude.Nothing,
      contentType = Lude.Nothing
    }

-- | The name of the endpoint that you specified when you created the endpoint using the <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint> API.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieEndpointName :: Lens.Lens' InvokeEndpoint Lude.Text
ieEndpointName = Lens.lens (endpointName :: InvokeEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: InvokeEndpoint)
{-# DEPRECATED ieEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | Provides input data, in the format specified in the @ContentType@ request header. Amazon SageMaker passes all of the data in the body to the model.
--
-- For information about the format of the request body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieBody :: Lens.Lens' InvokeEndpoint ByteString
ieBody = Lens.lens (body :: InvokeEndpoint -> ByteString) (\s a -> s {body = a} :: InvokeEndpoint)
{-# DEPRECATED ieBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The desired MIME type of the inference in the response.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieAccept :: Lens.Lens' InvokeEndpoint (Lude.Maybe Lude.Text)
ieAccept = Lens.lens (accept :: InvokeEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {accept = a} :: InvokeEndpoint)
{-# DEPRECATED ieAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

-- | The model to request for inference when invoking a multi-model endpoint.
--
-- /Note:/ Consider using 'targetModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTargetModel :: Lens.Lens' InvokeEndpoint (Lude.Maybe Lude.Text)
ieTargetModel = Lens.lens (targetModel :: InvokeEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {targetModel = a} :: InvokeEndpoint)
{-# DEPRECATED ieTargetModel "Use generic-lens or generic-optics with 'targetModel' instead." #-}

-- | Provides additional information about a request for an inference submitted to a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to provide an ID that you can use to track a request or to provide other metadata that a service endpoint was programmed to process. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieCustomAttributes :: Lens.Lens' InvokeEndpoint (Lude.Maybe (Lude.Sensitive Lude.Text))
ieCustomAttributes = Lens.lens (customAttributes :: InvokeEndpoint -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {customAttributes = a} :: InvokeEndpoint)
{-# DEPRECATED ieCustomAttributes "Use generic-lens or generic-optics with 'customAttributes' instead." #-}

-- | Specify the production variant to send the inference request to when invoking an endpoint that is running two or more variants. Note that this parameter overrides the default behavior for the endpoint, which is to distribute the invocation traffic based on the variant weights.
--
-- /Note:/ Consider using 'targetVariant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieTargetVariant :: Lens.Lens' InvokeEndpoint (Lude.Maybe Lude.Text)
ieTargetVariant = Lens.lens (targetVariant :: InvokeEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {targetVariant = a} :: InvokeEndpoint)
{-# DEPRECATED ieTargetVariant "Use generic-lens or generic-optics with 'targetVariant' instead." #-}

-- | The MIME type of the input data in the request body.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieContentType :: Lens.Lens' InvokeEndpoint (Lude.Maybe Lude.Text)
ieContentType = Lens.lens (contentType :: InvokeEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: InvokeEndpoint)
{-# DEPRECATED ieContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.AWSRequest InvokeEndpoint where
  type Rs InvokeEndpoint = InvokeEndpointResponse
  request = Req.postBody sageMakerRuntimeService
  response =
    Res.receiveBytes
      ( \s h x ->
          InvokeEndpointResponse'
            Lude.<$> (h Lude..#? "x-Amzn-Invoked-Production-Variant")
            Lude.<*> (Lude.pure x)
            Lude.<*> (h Lude..#? "X-Amzn-SageMaker-Custom-Attributes")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody InvokeEndpoint where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders InvokeEndpoint where
  toHeaders InvokeEndpoint' {..} =
    Lude.mconcat
      [ "Accept" Lude.=# accept,
        "X-Amzn-SageMaker-Target-Model" Lude.=# targetModel,
        "X-Amzn-SageMaker-Custom-Attributes" Lude.=# customAttributes,
        "X-Amzn-SageMaker-Target-Variant" Lude.=# targetVariant,
        "Content-Type" Lude.=# contentType
      ]

instance Lude.ToPath InvokeEndpoint where
  toPath InvokeEndpoint' {..} =
    Lude.mconcat
      ["/endpoints/", Lude.toBS endpointName, "/invocations"]

instance Lude.ToQuery InvokeEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkInvokeEndpointResponse' smart constructor.
data InvokeEndpointResponse = InvokeEndpointResponse'
  { -- | Identifies the production variant that was invoked.
    invokedProductionVariant :: Lude.Maybe Lude.Text,
    -- | Includes the inference provided by the model.
    --
    -- For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
    body :: ByteString,
    -- | Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back.
    --
    -- This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
    customAttributes :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The MIME type of the inference returned in the response body.
    contentType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InvokeEndpointResponse' with the minimum fields required to make a request.
--
-- * 'invokedProductionVariant' - Identifies the production variant that was invoked.
-- * 'body' - Includes the inference provided by the model.
--
-- For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
-- * 'customAttributes' - Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back.
--
-- This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
-- * 'contentType' - The MIME type of the inference returned in the response body.
-- * 'responseStatus' - The response status code.
mkInvokeEndpointResponse ::
  -- | 'body'
  ByteString ->
  -- | 'responseStatus'
  Lude.Int ->
  InvokeEndpointResponse
mkInvokeEndpointResponse pBody_ pResponseStatus_ =
  InvokeEndpointResponse'
    { invokedProductionVariant = Lude.Nothing,
      body = pBody_,
      customAttributes = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the production variant that was invoked.
--
-- /Note:/ Consider using 'invokedProductionVariant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iersInvokedProductionVariant :: Lens.Lens' InvokeEndpointResponse (Lude.Maybe Lude.Text)
iersInvokedProductionVariant = Lens.lens (invokedProductionVariant :: InvokeEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {invokedProductionVariant = a} :: InvokeEndpointResponse)
{-# DEPRECATED iersInvokedProductionVariant "Use generic-lens or generic-optics with 'invokedProductionVariant' instead." #-}

-- | Includes the inference provided by the model.
--
-- For information about the format of the response body, see <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iersBody :: Lens.Lens' InvokeEndpointResponse ByteString
iersBody = Lens.lens (body :: InvokeEndpointResponse -> ByteString) (\s a -> s {body = a} :: InvokeEndpointResponse)
{-# DEPRECATED iersBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | Provides additional information in the response about the inference returned by a model hosted at an Amazon SageMaker endpoint. The information is an opaque value that is forwarded verbatim. You could use this value, for example, to return an ID received in the @CustomAttributes@ header of a request or other metadata that a service endpoint was programmed to produce. The value must consist of no more than 1024 visible US-ASCII characters as specified in <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components> of the Hypertext Transfer Protocol (HTTP/1.1). If the customer wants the custom attribute returned, the model must set the custom attribute to be included on the way back.
--
-- This feature is currently supported in the AWS SDKs but not in the Amazon SageMaker Python SDK.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iersCustomAttributes :: Lens.Lens' InvokeEndpointResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
iersCustomAttributes = Lens.lens (customAttributes :: InvokeEndpointResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {customAttributes = a} :: InvokeEndpointResponse)
{-# DEPRECATED iersCustomAttributes "Use generic-lens or generic-optics with 'customAttributes' instead." #-}

-- | The MIME type of the inference returned in the response body.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iersContentType :: Lens.Lens' InvokeEndpointResponse (Lude.Maybe Lude.Text)
iersContentType = Lens.lens (contentType :: InvokeEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: InvokeEndpointResponse)
{-# DEPRECATED iersContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iersResponseStatus :: Lens.Lens' InvokeEndpointResponse Lude.Int
iersResponseStatus = Lens.lens (responseStatus :: InvokeEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InvokeEndpointResponse)
{-# DEPRECATED iersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
