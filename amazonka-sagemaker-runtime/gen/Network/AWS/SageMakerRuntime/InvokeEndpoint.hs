{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerRuntime.InvokeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- After you deploy a model into production using Amazon SageMaker hosting
-- services, your client applications use this API to get inferences from
-- the model hosted at the specified endpoint.
--
-- For an overview of Amazon SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works>.
--
-- Amazon SageMaker strips all POST headers except those supported by the
-- API. Amazon SageMaker might add additional headers. You should not rely
-- on the behavior of headers outside those enumerated in the request
-- syntax.
--
-- Calls to @InvokeEndpoint@ are authenticated by using AWS Signature
-- Version 4. For information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)>
-- in the /Amazon S3 API Reference/.
--
-- A customer\'s model containers must respond to requests within 60
-- seconds. The model itself can have a maximum processing time of 60
-- seconds before responding to invocations. If your model is going to take
-- 50-60 seconds of processing time, the SDK socket timeout should be set
-- to be 70 seconds.
--
-- Endpoints are scoped to an individual account, and are not public. The
-- URL does not contain the account ID, but Amazon SageMaker determines the
-- account ID from the authentication token that is supplied by the caller.
module Network.AWS.SageMakerRuntime.InvokeEndpoint
  ( -- * Creating a Request
    InvokeEndpoint (..),
    newInvokeEndpoint,

    -- * Request Lenses
    invokeEndpoint_targetContainerHostname,
    invokeEndpoint_contentType,
    invokeEndpoint_targetModel,
    invokeEndpoint_accept,
    invokeEndpoint_customAttributes,
    invokeEndpoint_inferenceId,
    invokeEndpoint_targetVariant,
    invokeEndpoint_endpointName,
    invokeEndpoint_body,

    -- * Destructuring the Response
    InvokeEndpointResponse (..),
    newInvokeEndpointResponse,

    -- * Response Lenses
    invokeEndpointResponse_contentType,
    invokeEndpointResponse_invokedProductionVariant,
    invokeEndpointResponse_customAttributes,
    invokeEndpointResponse_httpStatus,
    invokeEndpointResponse_body,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMakerRuntime.Types

-- | /See:/ 'newInvokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { -- | If the endpoint hosts multiple containers and is configured to use
    -- direct invocation, this parameter specifies the host name of the
    -- container to invoke.
    targetContainerHostname :: Prelude.Maybe Prelude.Text,
    -- | The MIME type of the input data in the request body.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The model to request for inference when invoking a multi-model endpoint.
    targetModel :: Prelude.Maybe Prelude.Text,
    -- | The desired MIME type of the inference in the response.
    accept :: Prelude.Maybe Prelude.Text,
    -- | Provides additional information about a request for an inference
    -- submitted to a model hosted at an Amazon SageMaker endpoint. The
    -- information is an opaque value that is forwarded verbatim. You could use
    -- this value, for example, to provide an ID that you can use to track a
    -- request or to provide other metadata that a service endpoint was
    -- programmed to process. The value must consist of no more than 1024
    -- visible US-ASCII characters as specified in
    -- <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
    -- of the Hypertext Transfer Protocol (HTTP\/1.1).
    --
    -- The code in your model is responsible for setting or updating any custom
    -- attributes in the response. If your code does not set this value in the
    -- response, an empty value is returned. For example, if a custom attribute
    -- represents the trace ID, your model can prepend the custom attribute
    -- with @Trace ID:@ in your post-processing function.
    --
    -- This feature is currently supported in the AWS SDKs but not in the
    -- Amazon SageMaker Python SDK.
    customAttributes :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | If you provide a value, it is added to the captured data when you enable
    -- data capture on the endpoint. For information about data capture, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture Data>.
    inferenceId :: Prelude.Maybe Prelude.Text,
    -- | Specify the production variant to send the inference request to when
    -- invoking an endpoint that is running two or more variants. Note that
    -- this parameter overrides the default behavior for the endpoint, which is
    -- to distribute the invocation traffic based on the variant weights.
    --
    -- For information about how to use variant targeting to perform a\/b
    -- testing, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-ab-testing.html Test models in production>
    targetVariant :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint that you specified when you created the
    -- endpoint using the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint>
    -- API.
    endpointName :: Prelude.Text,
    -- | Provides input data, in the format specified in the @ContentType@
    -- request header. Amazon SageMaker passes all of the data in the body to
    -- the model.
    --
    -- For information about the format of the request body, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
    body :: Prelude.Text
  }
  deriving (Prelude.Generic)

-- |
-- Create a value of 'InvokeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetContainerHostname', 'invokeEndpoint_targetContainerHostname' - If the endpoint hosts multiple containers and is configured to use
-- direct invocation, this parameter specifies the host name of the
-- container to invoke.
--
-- 'contentType', 'invokeEndpoint_contentType' - The MIME type of the input data in the request body.
--
-- 'targetModel', 'invokeEndpoint_targetModel' - The model to request for inference when invoking a multi-model endpoint.
--
-- 'accept', 'invokeEndpoint_accept' - The desired MIME type of the inference in the response.
--
-- 'customAttributes', 'invokeEndpoint_customAttributes' - Provides additional information about a request for an inference
-- submitted to a model hosted at an Amazon SageMaker endpoint. The
-- information is an opaque value that is forwarded verbatim. You could use
-- this value, for example, to provide an ID that you can use to track a
-- request or to provide other metadata that a service endpoint was
-- programmed to process. The value must consist of no more than 1024
-- visible US-ASCII characters as specified in
-- <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
-- of the Hypertext Transfer Protocol (HTTP\/1.1).
--
-- The code in your model is responsible for setting or updating any custom
-- attributes in the response. If your code does not set this value in the
-- response, an empty value is returned. For example, if a custom attribute
-- represents the trace ID, your model can prepend the custom attribute
-- with @Trace ID:@ in your post-processing function.
--
-- This feature is currently supported in the AWS SDKs but not in the
-- Amazon SageMaker Python SDK.
--
-- 'inferenceId', 'invokeEndpoint_inferenceId' - If you provide a value, it is added to the captured data when you enable
-- data capture on the endpoint. For information about data capture, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture Data>.
--
-- 'targetVariant', 'invokeEndpoint_targetVariant' - Specify the production variant to send the inference request to when
-- invoking an endpoint that is running two or more variants. Note that
-- this parameter overrides the default behavior for the endpoint, which is
-- to distribute the invocation traffic based on the variant weights.
--
-- For information about how to use variant targeting to perform a\/b
-- testing, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-ab-testing.html Test models in production>
--
-- 'endpointName', 'invokeEndpoint_endpointName' - The name of the endpoint that you specified when you created the
-- endpoint using the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint>
-- API.
--
-- 'body', 'invokeEndpoint_body' - Provides input data, in the format specified in the @ContentType@
-- request header. Amazon SageMaker passes all of the data in the body to
-- the model.
--
-- For information about the format of the request body, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
newInvokeEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'body'
  Prelude.Text ->
  InvokeEndpoint
newInvokeEndpoint pEndpointName_ pBody_ =
  InvokeEndpoint'
    { targetContainerHostname =
        Prelude.Nothing,
      contentType = Prelude.Nothing,
      targetModel = Prelude.Nothing,
      accept = Prelude.Nothing,
      customAttributes = Prelude.Nothing,
      inferenceId = Prelude.Nothing,
      targetVariant = Prelude.Nothing,
      endpointName = pEndpointName_,
      body = pBody_
    }

-- | If the endpoint hosts multiple containers and is configured to use
-- direct invocation, this parameter specifies the host name of the
-- container to invoke.
invokeEndpoint_targetContainerHostname :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_targetContainerHostname = Lens.lens (\InvokeEndpoint' {targetContainerHostname} -> targetContainerHostname) (\s@InvokeEndpoint' {} a -> s {targetContainerHostname = a} :: InvokeEndpoint)

-- | The MIME type of the input data in the request body.
invokeEndpoint_contentType :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_contentType = Lens.lens (\InvokeEndpoint' {contentType} -> contentType) (\s@InvokeEndpoint' {} a -> s {contentType = a} :: InvokeEndpoint)

-- | The model to request for inference when invoking a multi-model endpoint.
invokeEndpoint_targetModel :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_targetModel = Lens.lens (\InvokeEndpoint' {targetModel} -> targetModel) (\s@InvokeEndpoint' {} a -> s {targetModel = a} :: InvokeEndpoint)

-- | The desired MIME type of the inference in the response.
invokeEndpoint_accept :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_accept = Lens.lens (\InvokeEndpoint' {accept} -> accept) (\s@InvokeEndpoint' {} a -> s {accept = a} :: InvokeEndpoint)

-- | Provides additional information about a request for an inference
-- submitted to a model hosted at an Amazon SageMaker endpoint. The
-- information is an opaque value that is forwarded verbatim. You could use
-- this value, for example, to provide an ID that you can use to track a
-- request or to provide other metadata that a service endpoint was
-- programmed to process. The value must consist of no more than 1024
-- visible US-ASCII characters as specified in
-- <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
-- of the Hypertext Transfer Protocol (HTTP\/1.1).
--
-- The code in your model is responsible for setting or updating any custom
-- attributes in the response. If your code does not set this value in the
-- response, an empty value is returned. For example, if a custom attribute
-- represents the trace ID, your model can prepend the custom attribute
-- with @Trace ID:@ in your post-processing function.
--
-- This feature is currently supported in the AWS SDKs but not in the
-- Amazon SageMaker Python SDK.
invokeEndpoint_customAttributes :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_customAttributes = Lens.lens (\InvokeEndpoint' {customAttributes} -> customAttributes) (\s@InvokeEndpoint' {} a -> s {customAttributes = a} :: InvokeEndpoint) Prelude.. Lens.mapping Core._Sensitive

-- | If you provide a value, it is added to the captured data when you enable
-- data capture on the endpoint. For information about data capture, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture Data>.
invokeEndpoint_inferenceId :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_inferenceId = Lens.lens (\InvokeEndpoint' {inferenceId} -> inferenceId) (\s@InvokeEndpoint' {} a -> s {inferenceId = a} :: InvokeEndpoint)

-- | Specify the production variant to send the inference request to when
-- invoking an endpoint that is running two or more variants. Note that
-- this parameter overrides the default behavior for the endpoint, which is
-- to distribute the invocation traffic based on the variant weights.
--
-- For information about how to use variant targeting to perform a\/b
-- testing, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-ab-testing.html Test models in production>
invokeEndpoint_targetVariant :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_targetVariant = Lens.lens (\InvokeEndpoint' {targetVariant} -> targetVariant) (\s@InvokeEndpoint' {} a -> s {targetVariant = a} :: InvokeEndpoint)

-- | The name of the endpoint that you specified when you created the
-- endpoint using the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html CreateEndpoint>
-- API.
invokeEndpoint_endpointName :: Lens.Lens' InvokeEndpoint Prelude.Text
invokeEndpoint_endpointName = Lens.lens (\InvokeEndpoint' {endpointName} -> endpointName) (\s@InvokeEndpoint' {} a -> s {endpointName = a} :: InvokeEndpoint)

-- | Provides input data, in the format specified in the @ContentType@
-- request header. Amazon SageMaker passes all of the data in the body to
-- the model.
--
-- For information about the format of the request body, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
invokeEndpoint_body :: Lens.Lens' InvokeEndpoint Prelude.Text
invokeEndpoint_body = Lens.lens (\InvokeEndpoint' {body} -> body) (\s@InvokeEndpoint' {} a -> s {body = a} :: InvokeEndpoint)

instance Core.AWSRequest InvokeEndpoint where
  type
    AWSResponse InvokeEndpoint =
      InvokeEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          InvokeEndpointResponse'
            Prelude.<$> (h Core..#? "Content-Type")
            Prelude.<*> (h Core..#? "x-Amzn-Invoked-Production-Variant")
            Prelude.<*> (h Core..#? "X-Amzn-SageMaker-Custom-Attributes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable InvokeEndpoint

instance Prelude.NFData InvokeEndpoint

instance Core.ToHeaders InvokeEndpoint where
  toHeaders InvokeEndpoint' {..} =
    Prelude.mconcat
      [ "X-Amzn-SageMaker-Target-Container-Hostname"
          Core.=# targetContainerHostname,
        "Content-Type" Core.=# contentType,
        "X-Amzn-SageMaker-Target-Model" Core.=# targetModel,
        "Accept" Core.=# accept,
        "X-Amzn-SageMaker-Custom-Attributes"
          Core.=# customAttributes,
        "X-Amzn-SageMaker-Inference-Id" Core.=# inferenceId,
        "X-Amzn-SageMaker-Target-Variant"
          Core.=# targetVariant
      ]

instance Core.ToJSON InvokeEndpoint where
  toJSON InvokeEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Body" Core..= body)]
      )

instance Core.ToPath InvokeEndpoint where
  toPath InvokeEndpoint' {..} =
    Prelude.mconcat
      [ "/endpoints/",
        Core.toBS endpointName,
        "/invocations"
      ]

instance Core.ToQuery InvokeEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInvokeEndpointResponse' smart constructor.
data InvokeEndpointResponse = InvokeEndpointResponse'
  { -- | The MIME type of the inference returned in the response body.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | Identifies the production variant that was invoked.
    invokedProductionVariant :: Prelude.Maybe Prelude.Text,
    -- | Provides additional information in the response about the inference
    -- returned by a model hosted at an Amazon SageMaker endpoint. The
    -- information is an opaque value that is forwarded verbatim. You could use
    -- this value, for example, to return an ID received in the
    -- @CustomAttributes@ header of a request or other metadata that a service
    -- endpoint was programmed to produce. The value must consist of no more
    -- than 1024 visible US-ASCII characters as specified in
    -- <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
    -- of the Hypertext Transfer Protocol (HTTP\/1.1). If the customer wants
    -- the custom attribute returned, the model must set the custom attribute
    -- to be included on the way back.
    --
    -- The code in your model is responsible for setting or updating any custom
    -- attributes in the response. If your code does not set this value in the
    -- response, an empty value is returned. For example, if a custom attribute
    -- represents the trace ID, your model can prepend the custom attribute
    -- with @Trace ID:@ in your post-processing function.
    --
    -- This feature is currently supported in the AWS SDKs but not in the
    -- Amazon SageMaker Python SDK.
    customAttributes :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Includes the inference provided by the model.
    --
    -- For information about the format of the response body, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
    body :: Prelude.Text
  }
  deriving (Prelude.Generic)

-- |
-- Create a value of 'InvokeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'invokeEndpointResponse_contentType' - The MIME type of the inference returned in the response body.
--
-- 'invokedProductionVariant', 'invokeEndpointResponse_invokedProductionVariant' - Identifies the production variant that was invoked.
--
-- 'customAttributes', 'invokeEndpointResponse_customAttributes' - Provides additional information in the response about the inference
-- returned by a model hosted at an Amazon SageMaker endpoint. The
-- information is an opaque value that is forwarded verbatim. You could use
-- this value, for example, to return an ID received in the
-- @CustomAttributes@ header of a request or other metadata that a service
-- endpoint was programmed to produce. The value must consist of no more
-- than 1024 visible US-ASCII characters as specified in
-- <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
-- of the Hypertext Transfer Protocol (HTTP\/1.1). If the customer wants
-- the custom attribute returned, the model must set the custom attribute
-- to be included on the way back.
--
-- The code in your model is responsible for setting or updating any custom
-- attributes in the response. If your code does not set this value in the
-- response, an empty value is returned. For example, if a custom attribute
-- represents the trace ID, your model can prepend the custom attribute
-- with @Trace ID:@ in your post-processing function.
--
-- This feature is currently supported in the AWS SDKs but not in the
-- Amazon SageMaker Python SDK.
--
-- 'httpStatus', 'invokeEndpointResponse_httpStatus' - The response's http status code.
--
-- 'body', 'invokeEndpointResponse_body' - Includes the inference provided by the model.
--
-- For information about the format of the response body, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
newInvokeEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'body'
  Prelude.Text ->
  InvokeEndpointResponse
newInvokeEndpointResponse pHttpStatus_ pBody_ =
  InvokeEndpointResponse'
    { contentType =
        Prelude.Nothing,
      invokedProductionVariant = Prelude.Nothing,
      customAttributes = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      body = pBody_
    }

-- | The MIME type of the inference returned in the response body.
invokeEndpointResponse_contentType :: Lens.Lens' InvokeEndpointResponse (Prelude.Maybe Prelude.Text)
invokeEndpointResponse_contentType = Lens.lens (\InvokeEndpointResponse' {contentType} -> contentType) (\s@InvokeEndpointResponse' {} a -> s {contentType = a} :: InvokeEndpointResponse)

-- | Identifies the production variant that was invoked.
invokeEndpointResponse_invokedProductionVariant :: Lens.Lens' InvokeEndpointResponse (Prelude.Maybe Prelude.Text)
invokeEndpointResponse_invokedProductionVariant = Lens.lens (\InvokeEndpointResponse' {invokedProductionVariant} -> invokedProductionVariant) (\s@InvokeEndpointResponse' {} a -> s {invokedProductionVariant = a} :: InvokeEndpointResponse)

-- | Provides additional information in the response about the inference
-- returned by a model hosted at an Amazon SageMaker endpoint. The
-- information is an opaque value that is forwarded verbatim. You could use
-- this value, for example, to return an ID received in the
-- @CustomAttributes@ header of a request or other metadata that a service
-- endpoint was programmed to produce. The value must consist of no more
-- than 1024 visible US-ASCII characters as specified in
-- <https://tools.ietf.org/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
-- of the Hypertext Transfer Protocol (HTTP\/1.1). If the customer wants
-- the custom attribute returned, the model must set the custom attribute
-- to be included on the way back.
--
-- The code in your model is responsible for setting or updating any custom
-- attributes in the response. If your code does not set this value in the
-- response, an empty value is returned. For example, if a custom attribute
-- represents the trace ID, your model can prepend the custom attribute
-- with @Trace ID:@ in your post-processing function.
--
-- This feature is currently supported in the AWS SDKs but not in the
-- Amazon SageMaker Python SDK.
invokeEndpointResponse_customAttributes :: Lens.Lens' InvokeEndpointResponse (Prelude.Maybe Prelude.Text)
invokeEndpointResponse_customAttributes = Lens.lens (\InvokeEndpointResponse' {customAttributes} -> customAttributes) (\s@InvokeEndpointResponse' {} a -> s {customAttributes = a} :: InvokeEndpointResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
invokeEndpointResponse_httpStatus :: Lens.Lens' InvokeEndpointResponse Prelude.Int
invokeEndpointResponse_httpStatus = Lens.lens (\InvokeEndpointResponse' {httpStatus} -> httpStatus) (\s@InvokeEndpointResponse' {} a -> s {httpStatus = a} :: InvokeEndpointResponse)

-- | Includes the inference provided by the model.
--
-- For information about the format of the response body, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
invokeEndpointResponse_body :: Lens.Lens' InvokeEndpointResponse Prelude.Text
invokeEndpointResponse_body = Lens.lens (\InvokeEndpointResponse' {body} -> body) (\s@InvokeEndpointResponse' {} a -> s {body = a} :: InvokeEndpointResponse)

instance Prelude.NFData InvokeEndpointResponse
