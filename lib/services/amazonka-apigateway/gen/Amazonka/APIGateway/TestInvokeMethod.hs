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
-- Module      : Amazonka.APIGateway.TestInvokeMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the invocation of a Method in your RestApi with headers,
-- parameters, and an incoming request body.
module Amazonka.APIGateway.TestInvokeMethod
  ( -- * Creating a Request
    TestInvokeMethod (..),
    newTestInvokeMethod,

    -- * Request Lenses
    testInvokeMethod_body,
    testInvokeMethod_clientCertificateId,
    testInvokeMethod_headers,
    testInvokeMethod_multiValueHeaders,
    testInvokeMethod_pathWithQueryString,
    testInvokeMethod_stageVariables,
    testInvokeMethod_restApiId,
    testInvokeMethod_resourceId,
    testInvokeMethod_httpMethod,

    -- * Destructuring the Response
    TestInvokeMethodResponse (..),
    newTestInvokeMethodResponse,

    -- * Response Lenses
    testInvokeMethodResponse_body,
    testInvokeMethodResponse_headers,
    testInvokeMethodResponse_latency,
    testInvokeMethodResponse_log,
    testInvokeMethodResponse_multiValueHeaders,
    testInvokeMethodResponse_status,
    testInvokeMethodResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Make a request to simulate the invocation of a Method.
--
-- /See:/ 'newTestInvokeMethod' smart constructor.
data TestInvokeMethod = TestInvokeMethod'
  { -- | The simulated request body of an incoming invocation request.
    body :: Prelude.Maybe Prelude.Text,
    -- | A ClientCertificate identifier to use in the test invocation. API
    -- Gateway will use the certificate when making the HTTPS request to the
    -- defined back-end endpoint.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | A key-value map of headers to simulate an incoming invocation request.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The headers as a map from string to list of values to simulate an
    -- incoming invocation request.
    multiValueHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The URI path, including query string, of the simulated invocation
    -- request. Use this to specify path parameters and query string
    -- parameters.
    pathWithQueryString :: Prelude.Maybe Prelude.Text,
    -- | A key-value map of stage variables to simulate an invocation on a
    -- deployed Stage.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Specifies a test invoke method request\'s resource ID.
    resourceId :: Prelude.Text,
    -- | Specifies a test invoke method request\'s HTTP method.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestInvokeMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'testInvokeMethod_body' - The simulated request body of an incoming invocation request.
--
-- 'clientCertificateId', 'testInvokeMethod_clientCertificateId' - A ClientCertificate identifier to use in the test invocation. API
-- Gateway will use the certificate when making the HTTPS request to the
-- defined back-end endpoint.
--
-- 'headers', 'testInvokeMethod_headers' - A key-value map of headers to simulate an incoming invocation request.
--
-- 'multiValueHeaders', 'testInvokeMethod_multiValueHeaders' - The headers as a map from string to list of values to simulate an
-- incoming invocation request.
--
-- 'pathWithQueryString', 'testInvokeMethod_pathWithQueryString' - The URI path, including query string, of the simulated invocation
-- request. Use this to specify path parameters and query string
-- parameters.
--
-- 'stageVariables', 'testInvokeMethod_stageVariables' - A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
--
-- 'restApiId', 'testInvokeMethod_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'testInvokeMethod_resourceId' - Specifies a test invoke method request\'s resource ID.
--
-- 'httpMethod', 'testInvokeMethod_httpMethod' - Specifies a test invoke method request\'s HTTP method.
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
      { body = Prelude.Nothing,
        clientCertificateId = Prelude.Nothing,
        headers = Prelude.Nothing,
        multiValueHeaders = Prelude.Nothing,
        pathWithQueryString = Prelude.Nothing,
        stageVariables = Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_
      }

-- | The simulated request body of an incoming invocation request.
testInvokeMethod_body :: Lens.Lens' TestInvokeMethod (Prelude.Maybe Prelude.Text)
testInvokeMethod_body = Lens.lens (\TestInvokeMethod' {body} -> body) (\s@TestInvokeMethod' {} a -> s {body = a} :: TestInvokeMethod)

-- | A ClientCertificate identifier to use in the test invocation. API
-- Gateway will use the certificate when making the HTTPS request to the
-- defined back-end endpoint.
testInvokeMethod_clientCertificateId :: Lens.Lens' TestInvokeMethod (Prelude.Maybe Prelude.Text)
testInvokeMethod_clientCertificateId = Lens.lens (\TestInvokeMethod' {clientCertificateId} -> clientCertificateId) (\s@TestInvokeMethod' {} a -> s {clientCertificateId = a} :: TestInvokeMethod)

-- | A key-value map of headers to simulate an incoming invocation request.
testInvokeMethod_headers :: Lens.Lens' TestInvokeMethod (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeMethod_headers = Lens.lens (\TestInvokeMethod' {headers} -> headers) (\s@TestInvokeMethod' {} a -> s {headers = a} :: TestInvokeMethod) Prelude.. Lens.mapping Lens.coerced

-- | The headers as a map from string to list of values to simulate an
-- incoming invocation request.
testInvokeMethod_multiValueHeaders :: Lens.Lens' TestInvokeMethod (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeMethod_multiValueHeaders = Lens.lens (\TestInvokeMethod' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeMethod' {} a -> s {multiValueHeaders = a} :: TestInvokeMethod) Prelude.. Lens.mapping Lens.coerced

-- | The URI path, including query string, of the simulated invocation
-- request. Use this to specify path parameters and query string
-- parameters.
testInvokeMethod_pathWithQueryString :: Lens.Lens' TestInvokeMethod (Prelude.Maybe Prelude.Text)
testInvokeMethod_pathWithQueryString = Lens.lens (\TestInvokeMethod' {pathWithQueryString} -> pathWithQueryString) (\s@TestInvokeMethod' {} a -> s {pathWithQueryString = a} :: TestInvokeMethod)

-- | A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
testInvokeMethod_stageVariables :: Lens.Lens' TestInvokeMethod (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeMethod_stageVariables = Lens.lens (\TestInvokeMethod' {stageVariables} -> stageVariables) (\s@TestInvokeMethod' {} a -> s {stageVariables = a} :: TestInvokeMethod) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
testInvokeMethod_restApiId :: Lens.Lens' TestInvokeMethod Prelude.Text
testInvokeMethod_restApiId = Lens.lens (\TestInvokeMethod' {restApiId} -> restApiId) (\s@TestInvokeMethod' {} a -> s {restApiId = a} :: TestInvokeMethod)

-- | Specifies a test invoke method request\'s resource ID.
testInvokeMethod_resourceId :: Lens.Lens' TestInvokeMethod Prelude.Text
testInvokeMethod_resourceId = Lens.lens (\TestInvokeMethod' {resourceId} -> resourceId) (\s@TestInvokeMethod' {} a -> s {resourceId = a} :: TestInvokeMethod)

-- | Specifies a test invoke method request\'s HTTP method.
testInvokeMethod_httpMethod :: Lens.Lens' TestInvokeMethod Prelude.Text
testInvokeMethod_httpMethod = Lens.lens (\TestInvokeMethod' {httpMethod} -> httpMethod) (\s@TestInvokeMethod' {} a -> s {httpMethod = a} :: TestInvokeMethod)

instance Core.AWSRequest TestInvokeMethod where
  type
    AWSResponse TestInvokeMethod =
      TestInvokeMethodResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeMethodResponse'
            Prelude.<$> (x Data..?> "body")
            Prelude.<*> (x Data..?> "headers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "latency")
            Prelude.<*> (x Data..?> "log")
            Prelude.<*> ( x Data..?> "multiValueHeaders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestInvokeMethod where
  hashWithSalt _salt TestInvokeMethod' {..} =
    _salt `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` multiValueHeaders
      `Prelude.hashWithSalt` pathWithQueryString
      `Prelude.hashWithSalt` stageVariables
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod

instance Prelude.NFData TestInvokeMethod where
  rnf TestInvokeMethod' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf clientCertificateId
      `Prelude.seq` Prelude.rnf headers
      `Prelude.seq` Prelude.rnf multiValueHeaders
      `Prelude.seq` Prelude.rnf pathWithQueryString
      `Prelude.seq` Prelude.rnf stageVariables
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod

instance Data.ToHeaders TestInvokeMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON TestInvokeMethod where
  toJSON TestInvokeMethod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("body" Data..=) Prelude.<$> body,
            ("clientCertificateId" Data..=)
              Prelude.<$> clientCertificateId,
            ("headers" Data..=) Prelude.<$> headers,
            ("multiValueHeaders" Data..=)
              Prelude.<$> multiValueHeaders,
            ("pathWithQueryString" Data..=)
              Prelude.<$> pathWithQueryString,
            ("stageVariables" Data..=)
              Prelude.<$> stageVariables
          ]
      )

instance Data.ToPath TestInvokeMethod where
  toPath TestInvokeMethod' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod
      ]

instance Data.ToQuery TestInvokeMethod where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response of the test invoke request in the HTTP method.
--
-- /See:/ 'newTestInvokeMethodResponse' smart constructor.
data TestInvokeMethodResponse = TestInvokeMethodResponse'
  { -- | The body of the HTTP response.
    body :: Prelude.Maybe Prelude.Text,
    -- | The headers of the HTTP response.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The execution latency of the test invoke request.
    latency :: Prelude.Maybe Prelude.Integer,
    -- | The API Gateway execution log for the test invoke request.
    log :: Prelude.Maybe Prelude.Text,
    -- | The headers of the HTTP response as a map from string to list of values.
    multiValueHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The HTTP status code.
    status :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestInvokeMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'testInvokeMethodResponse_body' - The body of the HTTP response.
--
-- 'headers', 'testInvokeMethodResponse_headers' - The headers of the HTTP response.
--
-- 'latency', 'testInvokeMethodResponse_latency' - The execution latency of the test invoke request.
--
-- 'log', 'testInvokeMethodResponse_log' - The API Gateway execution log for the test invoke request.
--
-- 'multiValueHeaders', 'testInvokeMethodResponse_multiValueHeaders' - The headers of the HTTP response as a map from string to list of values.
--
-- 'status', 'testInvokeMethodResponse_status' - The HTTP status code.
--
-- 'httpStatus', 'testInvokeMethodResponse_httpStatus' - The response's http status code.
newTestInvokeMethodResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestInvokeMethodResponse
newTestInvokeMethodResponse pHttpStatus_ =
  TestInvokeMethodResponse'
    { body = Prelude.Nothing,
      headers = Prelude.Nothing,
      latency = Prelude.Nothing,
      log = Prelude.Nothing,
      multiValueHeaders = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The body of the HTTP response.
testInvokeMethodResponse_body :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Text)
testInvokeMethodResponse_body = Lens.lens (\TestInvokeMethodResponse' {body} -> body) (\s@TestInvokeMethodResponse' {} a -> s {body = a} :: TestInvokeMethodResponse)

-- | The headers of the HTTP response.
testInvokeMethodResponse_headers :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeMethodResponse_headers = Lens.lens (\TestInvokeMethodResponse' {headers} -> headers) (\s@TestInvokeMethodResponse' {} a -> s {headers = a} :: TestInvokeMethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | The execution latency of the test invoke request.
testInvokeMethodResponse_latency :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Integer)
testInvokeMethodResponse_latency = Lens.lens (\TestInvokeMethodResponse' {latency} -> latency) (\s@TestInvokeMethodResponse' {} a -> s {latency = a} :: TestInvokeMethodResponse)

-- | The API Gateway execution log for the test invoke request.
testInvokeMethodResponse_log :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Text)
testInvokeMethodResponse_log = Lens.lens (\TestInvokeMethodResponse' {log} -> log) (\s@TestInvokeMethodResponse' {} a -> s {log = a} :: TestInvokeMethodResponse)

-- | The headers of the HTTP response as a map from string to list of values.
testInvokeMethodResponse_multiValueHeaders :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeMethodResponse_multiValueHeaders = Lens.lens (\TestInvokeMethodResponse' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeMethodResponse' {} a -> s {multiValueHeaders = a} :: TestInvokeMethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status code.
testInvokeMethodResponse_status :: Lens.Lens' TestInvokeMethodResponse (Prelude.Maybe Prelude.Int)
testInvokeMethodResponse_status = Lens.lens (\TestInvokeMethodResponse' {status} -> status) (\s@TestInvokeMethodResponse' {} a -> s {status = a} :: TestInvokeMethodResponse)

-- | The response's http status code.
testInvokeMethodResponse_httpStatus :: Lens.Lens' TestInvokeMethodResponse Prelude.Int
testInvokeMethodResponse_httpStatus = Lens.lens (\TestInvokeMethodResponse' {httpStatus} -> httpStatus) (\s@TestInvokeMethodResponse' {} a -> s {httpStatus = a} :: TestInvokeMethodResponse)

instance Prelude.NFData TestInvokeMethodResponse where
  rnf TestInvokeMethodResponse' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf headers
      `Prelude.seq` Prelude.rnf latency
      `Prelude.seq` Prelude.rnf log
      `Prelude.seq` Prelude.rnf multiValueHeaders
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
