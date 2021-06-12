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
-- Module      : Network.AWS.APIGateway.TestInvokeMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of a Method in your RestApi with headers,
-- parameters, and an incoming request body.
module Network.AWS.APIGateway.TestInvokeMethod
  ( -- * Creating a Request
    TestInvokeMethod (..),
    newTestInvokeMethod,

    -- * Request Lenses
    testInvokeMethod_headers,
    testInvokeMethod_stageVariables,
    testInvokeMethod_body,
    testInvokeMethod_clientCertificateId,
    testInvokeMethod_multiValueHeaders,
    testInvokeMethod_pathWithQueryString,
    testInvokeMethod_restApiId,
    testInvokeMethod_resourceId,
    testInvokeMethod_httpMethod,

    -- * Destructuring the Response
    TestInvokeMethodResponse (..),
    newTestInvokeMethodResponse,

    -- * Response Lenses
    testInvokeMethodResponse_status,
    testInvokeMethodResponse_headers,
    testInvokeMethodResponse_body,
    testInvokeMethodResponse_log,
    testInvokeMethodResponse_multiValueHeaders,
    testInvokeMethodResponse_latency,
    testInvokeMethodResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Make a request to simulate the execution of a Method.
--
-- /See:/ 'newTestInvokeMethod' smart constructor.
data TestInvokeMethod = TestInvokeMethod'
  { -- | A key-value map of headers to simulate an incoming invocation request.
    headers :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A key-value map of stage variables to simulate an invocation on a
    -- deployed Stage.
    stageVariables :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The simulated request body of an incoming invocation request.
    body :: Core.Maybe Core.Text,
    -- | A ClientCertificate identifier to use in the test invocation. API
    -- Gateway will use the certificate when making the HTTPS request to the
    -- defined back-end endpoint.
    clientCertificateId :: Core.Maybe Core.Text,
    -- | The headers as a map from string to list of values to simulate an
    -- incoming invocation request.
    multiValueHeaders :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The URI path, including query string, of the simulated invocation
    -- request. Use this to specify path parameters and query string
    -- parameters.
    pathWithQueryString :: Core.Maybe Core.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] Specifies a test invoke method request\'s resource ID.
    resourceId :: Core.Text,
    -- | [Required] Specifies a test invoke method request\'s HTTP method.
    httpMethod :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestInvokeMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'testInvokeMethod_headers' - A key-value map of headers to simulate an incoming invocation request.
--
-- 'stageVariables', 'testInvokeMethod_stageVariables' - A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
--
-- 'body', 'testInvokeMethod_body' - The simulated request body of an incoming invocation request.
--
-- 'clientCertificateId', 'testInvokeMethod_clientCertificateId' - A ClientCertificate identifier to use in the test invocation. API
-- Gateway will use the certificate when making the HTTPS request to the
-- defined back-end endpoint.
--
-- 'multiValueHeaders', 'testInvokeMethod_multiValueHeaders' - The headers as a map from string to list of values to simulate an
-- incoming invocation request.
--
-- 'pathWithQueryString', 'testInvokeMethod_pathWithQueryString' - The URI path, including query string, of the simulated invocation
-- request. Use this to specify path parameters and query string
-- parameters.
--
-- 'restApiId', 'testInvokeMethod_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'testInvokeMethod_resourceId' - [Required] Specifies a test invoke method request\'s resource ID.
--
-- 'httpMethod', 'testInvokeMethod_httpMethod' - [Required] Specifies a test invoke method request\'s HTTP method.
newTestInvokeMethod ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  TestInvokeMethod
newTestInvokeMethod
  pRestApiId_
  pResourceId_
  pHttpMethod_ =
    TestInvokeMethod'
      { headers = Core.Nothing,
        stageVariables = Core.Nothing,
        body = Core.Nothing,
        clientCertificateId = Core.Nothing,
        multiValueHeaders = Core.Nothing,
        pathWithQueryString = Core.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_
      }

-- | A key-value map of headers to simulate an incoming invocation request.
testInvokeMethod_headers :: Lens.Lens' TestInvokeMethod (Core.Maybe (Core.HashMap Core.Text Core.Text))
testInvokeMethod_headers = Lens.lens (\TestInvokeMethod' {headers} -> headers) (\s@TestInvokeMethod' {} a -> s {headers = a} :: TestInvokeMethod) Core.. Lens.mapping Lens._Coerce

-- | A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
testInvokeMethod_stageVariables :: Lens.Lens' TestInvokeMethod (Core.Maybe (Core.HashMap Core.Text Core.Text))
testInvokeMethod_stageVariables = Lens.lens (\TestInvokeMethod' {stageVariables} -> stageVariables) (\s@TestInvokeMethod' {} a -> s {stageVariables = a} :: TestInvokeMethod) Core.. Lens.mapping Lens._Coerce

-- | The simulated request body of an incoming invocation request.
testInvokeMethod_body :: Lens.Lens' TestInvokeMethod (Core.Maybe Core.Text)
testInvokeMethod_body = Lens.lens (\TestInvokeMethod' {body} -> body) (\s@TestInvokeMethod' {} a -> s {body = a} :: TestInvokeMethod)

-- | A ClientCertificate identifier to use in the test invocation. API
-- Gateway will use the certificate when making the HTTPS request to the
-- defined back-end endpoint.
testInvokeMethod_clientCertificateId :: Lens.Lens' TestInvokeMethod (Core.Maybe Core.Text)
testInvokeMethod_clientCertificateId = Lens.lens (\TestInvokeMethod' {clientCertificateId} -> clientCertificateId) (\s@TestInvokeMethod' {} a -> s {clientCertificateId = a} :: TestInvokeMethod)

-- | The headers as a map from string to list of values to simulate an
-- incoming invocation request.
testInvokeMethod_multiValueHeaders :: Lens.Lens' TestInvokeMethod (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
testInvokeMethod_multiValueHeaders = Lens.lens (\TestInvokeMethod' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeMethod' {} a -> s {multiValueHeaders = a} :: TestInvokeMethod) Core.. Lens.mapping Lens._Coerce

-- | The URI path, including query string, of the simulated invocation
-- request. Use this to specify path parameters and query string
-- parameters.
testInvokeMethod_pathWithQueryString :: Lens.Lens' TestInvokeMethod (Core.Maybe Core.Text)
testInvokeMethod_pathWithQueryString = Lens.lens (\TestInvokeMethod' {pathWithQueryString} -> pathWithQueryString) (\s@TestInvokeMethod' {} a -> s {pathWithQueryString = a} :: TestInvokeMethod)

-- | [Required] The string identifier of the associated RestApi.
testInvokeMethod_restApiId :: Lens.Lens' TestInvokeMethod Core.Text
testInvokeMethod_restApiId = Lens.lens (\TestInvokeMethod' {restApiId} -> restApiId) (\s@TestInvokeMethod' {} a -> s {restApiId = a} :: TestInvokeMethod)

-- | [Required] Specifies a test invoke method request\'s resource ID.
testInvokeMethod_resourceId :: Lens.Lens' TestInvokeMethod Core.Text
testInvokeMethod_resourceId = Lens.lens (\TestInvokeMethod' {resourceId} -> resourceId) (\s@TestInvokeMethod' {} a -> s {resourceId = a} :: TestInvokeMethod)

-- | [Required] Specifies a test invoke method request\'s HTTP method.
testInvokeMethod_httpMethod :: Lens.Lens' TestInvokeMethod Core.Text
testInvokeMethod_httpMethod = Lens.lens (\TestInvokeMethod' {httpMethod} -> httpMethod) (\s@TestInvokeMethod' {} a -> s {httpMethod = a} :: TestInvokeMethod)

instance Core.AWSRequest TestInvokeMethod where
  type
    AWSResponse TestInvokeMethod =
      TestInvokeMethodResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeMethodResponse'
            Core.<$> (x Core..?> "status")
            Core.<*> (x Core..?> "headers" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "body")
            Core.<*> (x Core..?> "log")
            Core.<*> (x Core..?> "multiValueHeaders" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "latency")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TestInvokeMethod

instance Core.NFData TestInvokeMethod

instance Core.ToHeaders TestInvokeMethod where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TestInvokeMethod where
  toJSON TestInvokeMethod' {..} =
    Core.object
      ( Core.catMaybes
          [ ("headers" Core..=) Core.<$> headers,
            ("stageVariables" Core..=) Core.<$> stageVariables,
            ("body" Core..=) Core.<$> body,
            ("clientCertificateId" Core..=)
              Core.<$> clientCertificateId,
            ("multiValueHeaders" Core..=)
              Core.<$> multiValueHeaders,
            ("pathWithQueryString" Core..=)
              Core.<$> pathWithQueryString
          ]
      )

instance Core.ToPath TestInvokeMethod where
  toPath TestInvokeMethod' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod
      ]

instance Core.ToQuery TestInvokeMethod where
  toQuery = Core.const Core.mempty

-- | Represents the response of the test invoke request in the HTTP method.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-test-method.html#how-to-test-method-console Test API using the API Gateway console>
--
-- /See:/ 'newTestInvokeMethodResponse' smart constructor.
data TestInvokeMethodResponse = TestInvokeMethodResponse'
  { -- | The HTTP status code.
    status :: Core.Maybe Core.Int,
    -- | The headers of the HTTP response.
    headers :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The body of the HTTP response.
    body :: Core.Maybe Core.Text,
    -- | The API Gateway execution log for the test invoke request.
    log :: Core.Maybe Core.Text,
    -- | The headers of the HTTP response as a map from string to list of values.
    multiValueHeaders :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The execution latency of the test invoke request.
    latency :: Core.Maybe Core.Integer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestInvokeMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'testInvokeMethodResponse_status' - The HTTP status code.
--
-- 'headers', 'testInvokeMethodResponse_headers' - The headers of the HTTP response.
--
-- 'body', 'testInvokeMethodResponse_body' - The body of the HTTP response.
--
-- 'log', 'testInvokeMethodResponse_log' - The API Gateway execution log for the test invoke request.
--
-- 'multiValueHeaders', 'testInvokeMethodResponse_multiValueHeaders' - The headers of the HTTP response as a map from string to list of values.
--
-- 'latency', 'testInvokeMethodResponse_latency' - The execution latency of the test invoke request.
--
-- 'httpStatus', 'testInvokeMethodResponse_httpStatus' - The response's http status code.
newTestInvokeMethodResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TestInvokeMethodResponse
newTestInvokeMethodResponse pHttpStatus_ =
  TestInvokeMethodResponse'
    { status = Core.Nothing,
      headers = Core.Nothing,
      body = Core.Nothing,
      log = Core.Nothing,
      multiValueHeaders = Core.Nothing,
      latency = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The HTTP status code.
testInvokeMethodResponse_status :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Int)
testInvokeMethodResponse_status = Lens.lens (\TestInvokeMethodResponse' {status} -> status) (\s@TestInvokeMethodResponse' {} a -> s {status = a} :: TestInvokeMethodResponse)

-- | The headers of the HTTP response.
testInvokeMethodResponse_headers :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
testInvokeMethodResponse_headers = Lens.lens (\TestInvokeMethodResponse' {headers} -> headers) (\s@TestInvokeMethodResponse' {} a -> s {headers = a} :: TestInvokeMethodResponse) Core.. Lens.mapping Lens._Coerce

-- | The body of the HTTP response.
testInvokeMethodResponse_body :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Text)
testInvokeMethodResponse_body = Lens.lens (\TestInvokeMethodResponse' {body} -> body) (\s@TestInvokeMethodResponse' {} a -> s {body = a} :: TestInvokeMethodResponse)

-- | The API Gateway execution log for the test invoke request.
testInvokeMethodResponse_log :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Text)
testInvokeMethodResponse_log = Lens.lens (\TestInvokeMethodResponse' {log} -> log) (\s@TestInvokeMethodResponse' {} a -> s {log = a} :: TestInvokeMethodResponse)

-- | The headers of the HTTP response as a map from string to list of values.
testInvokeMethodResponse_multiValueHeaders :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
testInvokeMethodResponse_multiValueHeaders = Lens.lens (\TestInvokeMethodResponse' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeMethodResponse' {} a -> s {multiValueHeaders = a} :: TestInvokeMethodResponse) Core.. Lens.mapping Lens._Coerce

-- | The execution latency of the test invoke request.
testInvokeMethodResponse_latency :: Lens.Lens' TestInvokeMethodResponse (Core.Maybe Core.Integer)
testInvokeMethodResponse_latency = Lens.lens (\TestInvokeMethodResponse' {latency} -> latency) (\s@TestInvokeMethodResponse' {} a -> s {latency = a} :: TestInvokeMethodResponse)

-- | The response's http status code.
testInvokeMethodResponse_httpStatus :: Lens.Lens' TestInvokeMethodResponse Core.Int
testInvokeMethodResponse_httpStatus = Lens.lens (\TestInvokeMethodResponse' {httpStatus} -> httpStatus) (\s@TestInvokeMethodResponse' {} a -> s {httpStatus = a} :: TestInvokeMethodResponse)

instance Core.NFData TestInvokeMethodResponse
