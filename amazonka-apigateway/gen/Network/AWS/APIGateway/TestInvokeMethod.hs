{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Make a request to simulate the execution of a Method.
--
-- /See:/ 'newTestInvokeMethod' smart constructor.
data TestInvokeMethod = TestInvokeMethod'
  { -- | A key-value map of headers to simulate an incoming invocation request.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A key-value map of stage variables to simulate an invocation on a
    -- deployed Stage.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The simulated request body of an incoming invocation request.
    body :: Prelude.Maybe Prelude.Text,
    -- | A ClientCertificate identifier to use in the test invocation. API
    -- Gateway will use the certificate when making the HTTPS request to the
    -- defined back-end endpoint.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The headers as a map from string to list of values to simulate an
    -- incoming invocation request.
    multiValueHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The URI path, including query string, of the simulated invocation
    -- request. Use this to specify path parameters and query string
    -- parameters.
    pathWithQueryString :: Prelude.Maybe Prelude.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies a test invoke method request\'s resource ID.
    resourceId :: Prelude.Text,
    -- | [Required] Specifies a test invoke method request\'s HTTP method.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  TestInvokeMethod
newTestInvokeMethod
  pRestApiId_
  pResourceId_
  pHttpMethod_ =
    TestInvokeMethod'
      { headers = Prelude.Nothing,
        stageVariables = Prelude.Nothing,
        body = Prelude.Nothing,
        clientCertificateId = Prelude.Nothing,
        multiValueHeaders = Prelude.Nothing,
        pathWithQueryString = Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_
      }

-- | A key-value map of headers to simulate an incoming invocation request.
testInvokeMethod_headers :: Lens.Lens' TestInvokeMethod (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeMethod_headers = Lens.lens (\TestInvokeMethod' {headers} -> headers) (\s@TestInvokeMethod' {} a -> s {headers = a} :: TestInvokeMethod) Prelude.. Lens.mapping Prelude._Coerce

-- | A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
testInvokeMethod_stageVariables :: Lens.Lens' TestInvokeMethod (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeMethod_stageVariables = Lens.lens (\TestInvokeMethod' {stageVariables} -> stageVariables) (\s@TestInvokeMethod' {} a -> s {stageVariables = a} :: TestInvokeMethod) Prelude.. Lens.mapping Prelude._Coerce

-- | The simulated request body of an incoming invocation request.
testInvokeMethod_body :: Lens.Lens' TestInvokeMethod (Prelude.Maybe Prelude.Text)
testInvokeMethod_body = Lens.lens (\TestInvokeMethod' {body} -> body) (\s@TestInvokeMethod' {} a -> s {body = a} :: TestInvokeMethod)

-- | A ClientCertificate identifier to use in the test invocation. API
-- Gateway will use the certificate when making the HTTPS request to the
-- defined back-end endpoint.
testInvokeMethod_clientCertificateId :: Lens.Lens' TestInvokeMethod (Prelude.Maybe Prelude.Text)
testInvokeMethod_clientCertificateId = Lens.lens (\TestInvokeMethod' {clientCertificateId} -> clientCertificateId) (\s@TestInvokeMethod' {} a -> s {clientCertificateId = a} :: TestInvokeMethod)

-- | The headers as a map from string to list of values to simulate an
-- incoming invocation request.
testInvokeMethod_multiValueHeaders :: Lens.Lens' TestInvokeMethod (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeMethod_multiValueHeaders = Lens.lens (\TestInvokeMethod' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeMethod' {} a -> s {multiValueHeaders = a} :: TestInvokeMethod) Prelude.. Lens.mapping Prelude._Coerce

-- | The URI path, including query string, of the simulated invocation
-- request. Use this to specify path parameters and query string
-- parameters.
testInvokeMethod_pathWithQueryString :: Lens.Lens' TestInvokeMethod (Prelude.Maybe Prelude.Text)
testInvokeMethod_pathWithQueryString = Lens.lens (\TestInvokeMethod' {pathWithQueryString} -> pathWithQueryString) (\s@TestInvokeMethod' {} a -> s {pathWithQueryString = a} :: TestInvokeMethod)

-- | [Required] The string identifier of the associated RestApi.
testInvokeMethod_restApiId :: Lens.Lens' TestInvokeMethod Prelude.Text
testInvokeMethod_restApiId = Lens.lens (\TestInvokeMethod' {restApiId} -> restApiId) (\s@TestInvokeMethod' {} a -> s {restApiId = a} :: TestInvokeMethod)

-- | [Required] Specifies a test invoke method request\'s resource ID.
testInvokeMethod_resourceId :: Lens.Lens' TestInvokeMethod Prelude.Text
testInvokeMethod_resourceId = Lens.lens (\TestInvokeMethod' {resourceId} -> resourceId) (\s@TestInvokeMethod' {} a -> s {resourceId = a} :: TestInvokeMethod)

-- | [Required] Specifies a test invoke method request\'s HTTP method.
testInvokeMethod_httpMethod :: Lens.Lens' TestInvokeMethod Prelude.Text
testInvokeMethod_httpMethod = Lens.lens (\TestInvokeMethod' {httpMethod} -> httpMethod) (\s@TestInvokeMethod' {} a -> s {httpMethod = a} :: TestInvokeMethod)

instance Prelude.AWSRequest TestInvokeMethod where
  type Rs TestInvokeMethod = TestInvokeMethodResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeMethodResponse'
            Prelude.<$> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "headers" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "body")
            Prelude.<*> (x Prelude..?> "log")
            Prelude.<*> ( x Prelude..?> "multiValueHeaders"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "latency")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestInvokeMethod

instance Prelude.NFData TestInvokeMethod

instance Prelude.ToHeaders TestInvokeMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON TestInvokeMethod where
  toJSON TestInvokeMethod' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("headers" Prelude..=) Prelude.<$> headers,
            ("stageVariables" Prelude..=)
              Prelude.<$> stageVariables,
            ("body" Prelude..=) Prelude.<$> body,
            ("clientCertificateId" Prelude..=)
              Prelude.<$> clientCertificateId,
            ("multiValueHeaders" Prelude..=)
              Prelude.<$> multiValueHeaders,
            ("pathWithQueryString" Prelude..=)
              Prelude.<$> pathWithQueryString
          ]
      )

instance Prelude.ToPath TestInvokeMethod where
  toPath TestInvokeMethod' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId,
        "/methods/",
        Prelude.toBS httpMethod
      ]

instance Prelude.ToQuery TestInvokeMethod where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response of the test invoke request in the HTTP method.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-test-method.html#how-to-test-method-console Test API using the API Gateway console>
--
-- /See:/ 'newTestInvokeMethodResponse' smart constructor.
data TestInvokeMethodResponse = TestInvokeMethodResponse'
  { -- | The HTTP status code.
    status :: Prelude.Maybe Prelude.Int,
    -- | The headers of the HTTP response.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The body of the HTTP response.
    body :: Prelude.Maybe Prelude.Text,
    -- | The API Gateway execution log for the test invoke request.
    log :: Prelude.Maybe Prelude.Text,
    -- | The headers of the HTTP response as a map from string to list of values.
    multiValueHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The execution latency of the test invoke request.
    latency :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  TestInvokeMethodResponse
newTestInvokeMethodResponse pHttpStatus_ =
  TestInvokeMethodResponse'
    { status = Prelude.Nothing,
      headers = Prelude.Nothing,
      body = Prelude.Nothing,
      log = Prelude.Nothing,
      multiValueHeaders = Prelude.Nothing,
      latency = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The HTTP status code.
testInvokeMethodResponse_status :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Int)
testInvokeMethodResponse_status = Lens.lens (\TestInvokeMethodResponse' {status} -> status) (\s@TestInvokeMethodResponse' {} a -> s {status = a} :: TestInvokeMethodResponse)

-- | The headers of the HTTP response.
testInvokeMethodResponse_headers :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeMethodResponse_headers = Lens.lens (\TestInvokeMethodResponse' {headers} -> headers) (\s@TestInvokeMethodResponse' {} a -> s {headers = a} :: TestInvokeMethodResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The body of the HTTP response.
testInvokeMethodResponse_body :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Text)
testInvokeMethodResponse_body = Lens.lens (\TestInvokeMethodResponse' {body} -> body) (\s@TestInvokeMethodResponse' {} a -> s {body = a} :: TestInvokeMethodResponse)

-- | The API Gateway execution log for the test invoke request.
testInvokeMethodResponse_log :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Text)
testInvokeMethodResponse_log = Lens.lens (\TestInvokeMethodResponse' {log} -> log) (\s@TestInvokeMethodResponse' {} a -> s {log = a} :: TestInvokeMethodResponse)

-- | The headers of the HTTP response as a map from string to list of values.
testInvokeMethodResponse_multiValueHeaders :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeMethodResponse_multiValueHeaders = Lens.lens (\TestInvokeMethodResponse' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeMethodResponse' {} a -> s {multiValueHeaders = a} :: TestInvokeMethodResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The execution latency of the test invoke request.
testInvokeMethodResponse_latency :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Integer)
testInvokeMethodResponse_latency = Lens.lens (\TestInvokeMethodResponse' {latency} -> latency) (\s@TestInvokeMethodResponse' {} a -> s {latency = a} :: TestInvokeMethodResponse)

-- | The response's http status code.
testInvokeMethodResponse_httpStatus :: Lens.Lens' TestInvokeMethodResponse Prelude.Int
testInvokeMethodResponse_httpStatus = Lens.lens (\TestInvokeMethodResponse' {httpStatus} -> httpStatus) (\s@TestInvokeMethodResponse' {} a -> s {httpStatus = a} :: TestInvokeMethodResponse)

instance Prelude.NFData TestInvokeMethodResponse
