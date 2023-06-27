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
-- Module      : Amazonka.SageMakerRuntime.InvokeEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- Calls to @InvokeEndpoint@ are authenticated by using Amazon Web Services
-- Signature Version 4. For information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (Amazon Web Services Signature Version 4)>
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
module Amazonka.SageMakerRuntime.InvokeEndpoint
  ( -- * Creating a Request
    InvokeEndpoint (..),
    newInvokeEndpoint,

    -- * Request Lenses
    invokeEndpoint_accept,
    invokeEndpoint_contentType,
    invokeEndpoint_customAttributes,
    invokeEndpoint_enableExplanations,
    invokeEndpoint_inferenceId,
    invokeEndpoint_targetContainerHostname,
    invokeEndpoint_targetModel,
    invokeEndpoint_targetVariant,
    invokeEndpoint_endpointName,
    invokeEndpoint_body,

    -- * Destructuring the Response
    InvokeEndpointResponse (..),
    newInvokeEndpointResponse,

    -- * Response Lenses
    invokeEndpointResponse_contentType,
    invokeEndpointResponse_customAttributes,
    invokeEndpointResponse_invokedProductionVariant,
    invokeEndpointResponse_httpStatus,
    invokeEndpointResponse_body,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerRuntime.Types

-- | /See:/ 'newInvokeEndpoint' smart constructor.
data InvokeEndpoint = InvokeEndpoint'
  { -- | The desired MIME type of the inference in the response.
    accept :: Prelude.Maybe Prelude.Text,
    -- | The MIME type of the input data in the request body.
    contentType :: Prelude.Maybe Prelude.Text,
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
    -- This feature is currently supported in the Amazon Web Services SDKs but
    -- not in the Amazon SageMaker Python SDK.
    customAttributes :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An optional JMESPath expression used to override the
    -- @EnableExplanations@ parameter of the @ClarifyExplainerConfig@ API. See
    -- the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-enable EnableExplanations>
    -- section in the developer guide for more information.
    enableExplanations :: Prelude.Maybe Prelude.Text,
    -- | If you provide a value, it is added to the captured data when you enable
    -- data capture on the endpoint. For information about data capture, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture Data>.
    inferenceId :: Prelude.Maybe Prelude.Text,
    -- | If the endpoint hosts multiple containers and is configured to use
    -- direct invocation, this parameter specifies the host name of the
    -- container to invoke.
    targetContainerHostname :: Prelude.Maybe Prelude.Text,
    -- | The model to request for inference when invoking a multi-model endpoint.
    targetModel :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'invokeEndpoint_accept' - The desired MIME type of the inference in the response.
--
-- 'contentType', 'invokeEndpoint_contentType' - The MIME type of the input data in the request body.
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
-- This feature is currently supported in the Amazon Web Services SDKs but
-- not in the Amazon SageMaker Python SDK.
--
-- 'enableExplanations', 'invokeEndpoint_enableExplanations' - An optional JMESPath expression used to override the
-- @EnableExplanations@ parameter of the @ClarifyExplainerConfig@ API. See
-- the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-enable EnableExplanations>
-- section in the developer guide for more information.
--
-- 'inferenceId', 'invokeEndpoint_inferenceId' - If you provide a value, it is added to the captured data when you enable
-- data capture on the endpoint. For information about data capture, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture Data>.
--
-- 'targetContainerHostname', 'invokeEndpoint_targetContainerHostname' - If the endpoint hosts multiple containers and is configured to use
-- direct invocation, this parameter specifies the host name of the
-- container to invoke.
--
-- 'targetModel', 'invokeEndpoint_targetModel' - The model to request for inference when invoking a multi-model endpoint.
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
    { accept = Prelude.Nothing,
      contentType = Prelude.Nothing,
      customAttributes = Prelude.Nothing,
      enableExplanations = Prelude.Nothing,
      inferenceId = Prelude.Nothing,
      targetContainerHostname = Prelude.Nothing,
      targetModel = Prelude.Nothing,
      targetVariant = Prelude.Nothing,
      endpointName = pEndpointName_,
      body = pBody_
    }

-- | The desired MIME type of the inference in the response.
invokeEndpoint_accept :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_accept = Lens.lens (\InvokeEndpoint' {accept} -> accept) (\s@InvokeEndpoint' {} a -> s {accept = a} :: InvokeEndpoint)

-- | The MIME type of the input data in the request body.
invokeEndpoint_contentType :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_contentType = Lens.lens (\InvokeEndpoint' {contentType} -> contentType) (\s@InvokeEndpoint' {} a -> s {contentType = a} :: InvokeEndpoint)

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
-- This feature is currently supported in the Amazon Web Services SDKs but
-- not in the Amazon SageMaker Python SDK.
invokeEndpoint_customAttributes :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_customAttributes = Lens.lens (\InvokeEndpoint' {customAttributes} -> customAttributes) (\s@InvokeEndpoint' {} a -> s {customAttributes = a} :: InvokeEndpoint) Prelude.. Lens.mapping Data._Sensitive

-- | An optional JMESPath expression used to override the
-- @EnableExplanations@ parameter of the @ClarifyExplainerConfig@ API. See
-- the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-enable EnableExplanations>
-- section in the developer guide for more information.
invokeEndpoint_enableExplanations :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_enableExplanations = Lens.lens (\InvokeEndpoint' {enableExplanations} -> enableExplanations) (\s@InvokeEndpoint' {} a -> s {enableExplanations = a} :: InvokeEndpoint)

-- | If you provide a value, it is added to the captured data when you enable
-- data capture on the endpoint. For information about data capture, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture Data>.
invokeEndpoint_inferenceId :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_inferenceId = Lens.lens (\InvokeEndpoint' {inferenceId} -> inferenceId) (\s@InvokeEndpoint' {} a -> s {inferenceId = a} :: InvokeEndpoint)

-- | If the endpoint hosts multiple containers and is configured to use
-- direct invocation, this parameter specifies the host name of the
-- container to invoke.
invokeEndpoint_targetContainerHostname :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_targetContainerHostname = Lens.lens (\InvokeEndpoint' {targetContainerHostname} -> targetContainerHostname) (\s@InvokeEndpoint' {} a -> s {targetContainerHostname = a} :: InvokeEndpoint)

-- | The model to request for inference when invoking a multi-model endpoint.
invokeEndpoint_targetModel :: Lens.Lens' InvokeEndpoint (Prelude.Maybe Prelude.Text)
invokeEndpoint_targetModel = Lens.lens (\InvokeEndpoint' {targetModel} -> targetModel) (\s@InvokeEndpoint' {} a -> s {targetModel = a} :: InvokeEndpoint)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InvokeEndpointResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "X-Amzn-SageMaker-Custom-Attributes")
            Prelude.<*> (h Data..#? "x-Amzn-Invoked-Production-Variant")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable InvokeEndpoint where
  hashWithSalt _salt InvokeEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` accept
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` customAttributes
      `Prelude.hashWithSalt` enableExplanations
      `Prelude.hashWithSalt` inferenceId
      `Prelude.hashWithSalt` targetContainerHostname
      `Prelude.hashWithSalt` targetModel
      `Prelude.hashWithSalt` targetVariant
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` body

instance Prelude.NFData InvokeEndpoint where
  rnf InvokeEndpoint' {..} =
    Prelude.rnf accept
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf customAttributes
      `Prelude.seq` Prelude.rnf enableExplanations
      `Prelude.seq` Prelude.rnf inferenceId
      `Prelude.seq` Prelude.rnf targetContainerHostname
      `Prelude.seq` Prelude.rnf targetModel
      `Prelude.seq` Prelude.rnf targetVariant
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf body

instance Data.ToHeaders InvokeEndpoint where
  toHeaders InvokeEndpoint' {..} =
    Prelude.mconcat
      [ "Accept" Data.=# accept,
        "Content-Type" Data.=# contentType,
        "X-Amzn-SageMaker-Custom-Attributes"
          Data.=# customAttributes,
        "X-Amzn-SageMaker-Enable-Explanations"
          Data.=# enableExplanations,
        "X-Amzn-SageMaker-Inference-Id" Data.=# inferenceId,
        "X-Amzn-SageMaker-Target-Container-Hostname"
          Data.=# targetContainerHostname,
        "X-Amzn-SageMaker-Target-Model" Data.=# targetModel,
        "X-Amzn-SageMaker-Target-Variant"
          Data.=# targetVariant
      ]

instance Data.ToJSON InvokeEndpoint where
  toJSON InvokeEndpoint' {..} = Data.toJSON body

instance Data.ToPath InvokeEndpoint where
  toPath InvokeEndpoint' {..} =
    Prelude.mconcat
      [ "/endpoints/",
        Data.toBS endpointName,
        "/invocations"
      ]

instance Data.ToQuery InvokeEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInvokeEndpointResponse' smart constructor.
data InvokeEndpointResponse = InvokeEndpointResponse'
  { -- | The MIME type of the inference returned in the response body.
    contentType :: Prelude.Maybe Prelude.Text,
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
    -- This feature is currently supported in the Amazon Web Services SDKs but
    -- not in the Amazon SageMaker Python SDK.
    customAttributes :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Identifies the production variant that was invoked.
    invokedProductionVariant :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Includes the inference provided by the model.
    --
    -- For information about the format of the response body, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
    --
    -- If the explainer is activated, the body includes the explanations
    -- provided by the model. For more information, see the __Response
    -- section__ under
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response Invoke the Endpoint>
    -- in the Developer Guide.
    body :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- This feature is currently supported in the Amazon Web Services SDKs but
-- not in the Amazon SageMaker Python SDK.
--
-- 'invokedProductionVariant', 'invokeEndpointResponse_invokedProductionVariant' - Identifies the production variant that was invoked.
--
-- 'httpStatus', 'invokeEndpointResponse_httpStatus' - The response's http status code.
--
-- 'body', 'invokeEndpointResponse_body' - Includes the inference provided by the model.
--
-- For information about the format of the response body, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
--
-- If the explainer is activated, the body includes the explanations
-- provided by the model. For more information, see the __Response
-- section__ under
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response Invoke the Endpoint>
-- in the Developer Guide.
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
      customAttributes = Prelude.Nothing,
      invokedProductionVariant = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      body = pBody_
    }

-- | The MIME type of the inference returned in the response body.
invokeEndpointResponse_contentType :: Lens.Lens' InvokeEndpointResponse (Prelude.Maybe Prelude.Text)
invokeEndpointResponse_contentType = Lens.lens (\InvokeEndpointResponse' {contentType} -> contentType) (\s@InvokeEndpointResponse' {} a -> s {contentType = a} :: InvokeEndpointResponse)

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
-- This feature is currently supported in the Amazon Web Services SDKs but
-- not in the Amazon SageMaker Python SDK.
invokeEndpointResponse_customAttributes :: Lens.Lens' InvokeEndpointResponse (Prelude.Maybe Prelude.Text)
invokeEndpointResponse_customAttributes = Lens.lens (\InvokeEndpointResponse' {customAttributes} -> customAttributes) (\s@InvokeEndpointResponse' {} a -> s {customAttributes = a} :: InvokeEndpointResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Identifies the production variant that was invoked.
invokeEndpointResponse_invokedProductionVariant :: Lens.Lens' InvokeEndpointResponse (Prelude.Maybe Prelude.Text)
invokeEndpointResponse_invokedProductionVariant = Lens.lens (\InvokeEndpointResponse' {invokedProductionVariant} -> invokedProductionVariant) (\s@InvokeEndpointResponse' {} a -> s {invokedProductionVariant = a} :: InvokeEndpointResponse)

-- | The response's http status code.
invokeEndpointResponse_httpStatus :: Lens.Lens' InvokeEndpointResponse Prelude.Int
invokeEndpointResponse_httpStatus = Lens.lens (\InvokeEndpointResponse' {httpStatus} -> httpStatus) (\s@InvokeEndpointResponse' {} a -> s {httpStatus = a} :: InvokeEndpointResponse)

-- | Includes the inference provided by the model.
--
-- For information about the format of the response body, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/cdf-inference.html Common Data Formats-Inference>.
--
-- If the explainer is activated, the body includes the explanations
-- provided by the model. For more information, see the __Response
-- section__ under
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response Invoke the Endpoint>
-- in the Developer Guide.
invokeEndpointResponse_body :: Lens.Lens' InvokeEndpointResponse Prelude.Text
invokeEndpointResponse_body = Lens.lens (\InvokeEndpointResponse' {body} -> body) (\s@InvokeEndpointResponse' {} a -> s {body = a} :: InvokeEndpointResponse)

instance Prelude.NFData InvokeEndpointResponse where
  rnf InvokeEndpointResponse' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf customAttributes
      `Prelude.seq` Prelude.rnf invokedProductionVariant
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf body
