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
-- Module      : Amazonka.SageMakerRuntime.InvokeEndpointAsync
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- After you deploy a model into production using Amazon SageMaker hosting
-- services, your client applications use this API to get inferences from
-- the model hosted at the specified endpoint in an asynchronous manner.
--
-- Inference requests sent to this API are enqueued for asynchronous
-- processing. The processing of the inference request may or may not
-- complete before the you receive a response from this API. The response
-- from this API will not contain the result of the inference request but
-- contain information about where you can locate it.
--
-- Amazon SageMaker strips all @POST@ headers except those supported by the
-- API. Amazon SageMaker might add additional headers. You should not rely
-- on the behavior of headers outside those enumerated in the request
-- syntax.
--
-- Calls to @InvokeEndpointAsync@ are authenticated by using Amazon Web
-- Services Signature Version 4. For information, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (Amazon Web Services Signature Version 4)>
-- in the /Amazon S3 API Reference/.
module Amazonka.SageMakerRuntime.InvokeEndpointAsync
  ( -- * Creating a Request
    InvokeEndpointAsync (..),
    newInvokeEndpointAsync,

    -- * Request Lenses
    invokeEndpointAsync_accept,
    invokeEndpointAsync_contentType,
    invokeEndpointAsync_customAttributes,
    invokeEndpointAsync_inferenceId,
    invokeEndpointAsync_requestTTLSeconds,
    invokeEndpointAsync_endpointName,
    invokeEndpointAsync_inputLocation,

    -- * Destructuring the Response
    InvokeEndpointAsyncResponse (..),
    newInvokeEndpointAsyncResponse,

    -- * Response Lenses
    invokeEndpointAsyncResponse_inferenceId,
    invokeEndpointAsyncResponse_outputLocation,
    invokeEndpointAsyncResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerRuntime.Types

-- | /See:/ 'newInvokeEndpointAsync' smart constructor.
data InvokeEndpointAsync = InvokeEndpointAsync'
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
    -- <https://datatracker.ietf.org/doc/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
    -- of the Hypertext Transfer Protocol (HTTP\/1.1).
    --
    -- The code in your model is responsible for setting or updating any custom
    -- attributes in the response. If your code does not set this value in the
    -- response, an empty value is returned. For example, if a custom attribute
    -- represents the trace ID, your model can prepend the custom attribute
    -- with @Trace ID@: in your post-processing function.
    --
    -- This feature is currently supported in the Amazon Web Services SDKs but
    -- not in the Amazon SageMaker Python SDK.
    customAttributes :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identifier for the inference request. Amazon SageMaker will generate
    -- an identifier for you if none is specified.
    inferenceId :: Prelude.Maybe Prelude.Text,
    -- | Maximum age in seconds a request can be in the queue before it is marked
    -- as expired.
    requestTTLSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the endpoint that you specified when you created the
    -- endpoint using the
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpoint.html CreateEndpoint>
    -- API.
    endpointName :: Prelude.Text,
    -- | The Amazon S3 URI where the inference request payload is stored.
    inputLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeEndpointAsync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'invokeEndpointAsync_accept' - The desired MIME type of the inference in the response.
--
-- 'contentType', 'invokeEndpointAsync_contentType' - The MIME type of the input data in the request body.
--
-- 'customAttributes', 'invokeEndpointAsync_customAttributes' - Provides additional information about a request for an inference
-- submitted to a model hosted at an Amazon SageMaker endpoint. The
-- information is an opaque value that is forwarded verbatim. You could use
-- this value, for example, to provide an ID that you can use to track a
-- request or to provide other metadata that a service endpoint was
-- programmed to process. The value must consist of no more than 1024
-- visible US-ASCII characters as specified in
-- <https://datatracker.ietf.org/doc/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
-- of the Hypertext Transfer Protocol (HTTP\/1.1).
--
-- The code in your model is responsible for setting or updating any custom
-- attributes in the response. If your code does not set this value in the
-- response, an empty value is returned. For example, if a custom attribute
-- represents the trace ID, your model can prepend the custom attribute
-- with @Trace ID@: in your post-processing function.
--
-- This feature is currently supported in the Amazon Web Services SDKs but
-- not in the Amazon SageMaker Python SDK.
--
-- 'inferenceId', 'invokeEndpointAsync_inferenceId' - The identifier for the inference request. Amazon SageMaker will generate
-- an identifier for you if none is specified.
--
-- 'requestTTLSeconds', 'invokeEndpointAsync_requestTTLSeconds' - Maximum age in seconds a request can be in the queue before it is marked
-- as expired.
--
-- 'endpointName', 'invokeEndpointAsync_endpointName' - The name of the endpoint that you specified when you created the
-- endpoint using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpoint.html CreateEndpoint>
-- API.
--
-- 'inputLocation', 'invokeEndpointAsync_inputLocation' - The Amazon S3 URI where the inference request payload is stored.
newInvokeEndpointAsync ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'inputLocation'
  Prelude.Text ->
  InvokeEndpointAsync
newInvokeEndpointAsync pEndpointName_ pInputLocation_ =
  InvokeEndpointAsync'
    { accept = Prelude.Nothing,
      contentType = Prelude.Nothing,
      customAttributes = Prelude.Nothing,
      inferenceId = Prelude.Nothing,
      requestTTLSeconds = Prelude.Nothing,
      endpointName = pEndpointName_,
      inputLocation = pInputLocation_
    }

-- | The desired MIME type of the inference in the response.
invokeEndpointAsync_accept :: Lens.Lens' InvokeEndpointAsync (Prelude.Maybe Prelude.Text)
invokeEndpointAsync_accept = Lens.lens (\InvokeEndpointAsync' {accept} -> accept) (\s@InvokeEndpointAsync' {} a -> s {accept = a} :: InvokeEndpointAsync)

-- | The MIME type of the input data in the request body.
invokeEndpointAsync_contentType :: Lens.Lens' InvokeEndpointAsync (Prelude.Maybe Prelude.Text)
invokeEndpointAsync_contentType = Lens.lens (\InvokeEndpointAsync' {contentType} -> contentType) (\s@InvokeEndpointAsync' {} a -> s {contentType = a} :: InvokeEndpointAsync)

-- | Provides additional information about a request for an inference
-- submitted to a model hosted at an Amazon SageMaker endpoint. The
-- information is an opaque value that is forwarded verbatim. You could use
-- this value, for example, to provide an ID that you can use to track a
-- request or to provide other metadata that a service endpoint was
-- programmed to process. The value must consist of no more than 1024
-- visible US-ASCII characters as specified in
-- <https://datatracker.ietf.org/doc/html/rfc7230#section-3.2.6 Section 3.3.6. Field Value Components>
-- of the Hypertext Transfer Protocol (HTTP\/1.1).
--
-- The code in your model is responsible for setting or updating any custom
-- attributes in the response. If your code does not set this value in the
-- response, an empty value is returned. For example, if a custom attribute
-- represents the trace ID, your model can prepend the custom attribute
-- with @Trace ID@: in your post-processing function.
--
-- This feature is currently supported in the Amazon Web Services SDKs but
-- not in the Amazon SageMaker Python SDK.
invokeEndpointAsync_customAttributes :: Lens.Lens' InvokeEndpointAsync (Prelude.Maybe Prelude.Text)
invokeEndpointAsync_customAttributes = Lens.lens (\InvokeEndpointAsync' {customAttributes} -> customAttributes) (\s@InvokeEndpointAsync' {} a -> s {customAttributes = a} :: InvokeEndpointAsync) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier for the inference request. Amazon SageMaker will generate
-- an identifier for you if none is specified.
invokeEndpointAsync_inferenceId :: Lens.Lens' InvokeEndpointAsync (Prelude.Maybe Prelude.Text)
invokeEndpointAsync_inferenceId = Lens.lens (\InvokeEndpointAsync' {inferenceId} -> inferenceId) (\s@InvokeEndpointAsync' {} a -> s {inferenceId = a} :: InvokeEndpointAsync)

-- | Maximum age in seconds a request can be in the queue before it is marked
-- as expired.
invokeEndpointAsync_requestTTLSeconds :: Lens.Lens' InvokeEndpointAsync (Prelude.Maybe Prelude.Natural)
invokeEndpointAsync_requestTTLSeconds = Lens.lens (\InvokeEndpointAsync' {requestTTLSeconds} -> requestTTLSeconds) (\s@InvokeEndpointAsync' {} a -> s {requestTTLSeconds = a} :: InvokeEndpointAsync)

-- | The name of the endpoint that you specified when you created the
-- endpoint using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpoint.html CreateEndpoint>
-- API.
invokeEndpointAsync_endpointName :: Lens.Lens' InvokeEndpointAsync Prelude.Text
invokeEndpointAsync_endpointName = Lens.lens (\InvokeEndpointAsync' {endpointName} -> endpointName) (\s@InvokeEndpointAsync' {} a -> s {endpointName = a} :: InvokeEndpointAsync)

-- | The Amazon S3 URI where the inference request payload is stored.
invokeEndpointAsync_inputLocation :: Lens.Lens' InvokeEndpointAsync Prelude.Text
invokeEndpointAsync_inputLocation = Lens.lens (\InvokeEndpointAsync' {inputLocation} -> inputLocation) (\s@InvokeEndpointAsync' {} a -> s {inputLocation = a} :: InvokeEndpointAsync)

instance Core.AWSRequest InvokeEndpointAsync where
  type
    AWSResponse InvokeEndpointAsync =
      InvokeEndpointAsyncResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InvokeEndpointAsyncResponse'
            Prelude.<$> (x Data..?> "InferenceId")
            Prelude.<*> (h Data..#? "X-Amzn-SageMaker-OutputLocation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InvokeEndpointAsync where
  hashWithSalt _salt InvokeEndpointAsync' {..} =
    _salt `Prelude.hashWithSalt` accept
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` customAttributes
      `Prelude.hashWithSalt` inferenceId
      `Prelude.hashWithSalt` requestTTLSeconds
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` inputLocation

instance Prelude.NFData InvokeEndpointAsync where
  rnf InvokeEndpointAsync' {..} =
    Prelude.rnf accept
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf customAttributes
      `Prelude.seq` Prelude.rnf inferenceId
      `Prelude.seq` Prelude.rnf requestTTLSeconds
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf inputLocation

instance Data.ToHeaders InvokeEndpointAsync where
  toHeaders InvokeEndpointAsync' {..} =
    Prelude.mconcat
      [ "X-Amzn-SageMaker-Accept" Data.=# accept,
        "X-Amzn-SageMaker-Content-Type" Data.=# contentType,
        "X-Amzn-SageMaker-Custom-Attributes"
          Data.=# customAttributes,
        "X-Amzn-SageMaker-Inference-Id" Data.=# inferenceId,
        "X-Amzn-SageMaker-RequestTTLSeconds"
          Data.=# requestTTLSeconds,
        "X-Amzn-SageMaker-InputLocation"
          Data.=# inputLocation,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON InvokeEndpointAsync where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath InvokeEndpointAsync where
  toPath InvokeEndpointAsync' {..} =
    Prelude.mconcat
      [ "/endpoints/",
        Data.toBS endpointName,
        "/async-invocations"
      ]

instance Data.ToQuery InvokeEndpointAsync where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInvokeEndpointAsyncResponse' smart constructor.
data InvokeEndpointAsyncResponse = InvokeEndpointAsyncResponse'
  { -- | Identifier for an inference request. This will be the same as the
    -- @InferenceId@ specified in the input. Amazon SageMaker will generate an
    -- identifier for you if you do not specify one.
    inferenceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI where the inference response payload is stored.
    outputLocation :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeEndpointAsyncResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceId', 'invokeEndpointAsyncResponse_inferenceId' - Identifier for an inference request. This will be the same as the
-- @InferenceId@ specified in the input. Amazon SageMaker will generate an
-- identifier for you if you do not specify one.
--
-- 'outputLocation', 'invokeEndpointAsyncResponse_outputLocation' - The Amazon S3 URI where the inference response payload is stored.
--
-- 'httpStatus', 'invokeEndpointAsyncResponse_httpStatus' - The response's http status code.
newInvokeEndpointAsyncResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InvokeEndpointAsyncResponse
newInvokeEndpointAsyncResponse pHttpStatus_ =
  InvokeEndpointAsyncResponse'
    { inferenceId =
        Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for an inference request. This will be the same as the
-- @InferenceId@ specified in the input. Amazon SageMaker will generate an
-- identifier for you if you do not specify one.
invokeEndpointAsyncResponse_inferenceId :: Lens.Lens' InvokeEndpointAsyncResponse (Prelude.Maybe Prelude.Text)
invokeEndpointAsyncResponse_inferenceId = Lens.lens (\InvokeEndpointAsyncResponse' {inferenceId} -> inferenceId) (\s@InvokeEndpointAsyncResponse' {} a -> s {inferenceId = a} :: InvokeEndpointAsyncResponse)

-- | The Amazon S3 URI where the inference response payload is stored.
invokeEndpointAsyncResponse_outputLocation :: Lens.Lens' InvokeEndpointAsyncResponse (Prelude.Maybe Prelude.Text)
invokeEndpointAsyncResponse_outputLocation = Lens.lens (\InvokeEndpointAsyncResponse' {outputLocation} -> outputLocation) (\s@InvokeEndpointAsyncResponse' {} a -> s {outputLocation = a} :: InvokeEndpointAsyncResponse)

-- | The response's http status code.
invokeEndpointAsyncResponse_httpStatus :: Lens.Lens' InvokeEndpointAsyncResponse Prelude.Int
invokeEndpointAsyncResponse_httpStatus = Lens.lens (\InvokeEndpointAsyncResponse' {httpStatus} -> httpStatus) (\s@InvokeEndpointAsyncResponse' {} a -> s {httpStatus = a} :: InvokeEndpointAsyncResponse)

instance Prelude.NFData InvokeEndpointAsyncResponse where
  rnf InvokeEndpointAsyncResponse' {..} =
    Prelude.rnf inferenceId
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf httpStatus
