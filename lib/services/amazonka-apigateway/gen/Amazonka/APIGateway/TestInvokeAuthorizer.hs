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
-- Module      : Amazonka.APIGateway.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of an Authorizer in your RestApi with headers,
-- parameters, and an incoming request body.
module Amazonka.APIGateway.TestInvokeAuthorizer
  ( -- * Creating a Request
    TestInvokeAuthorizer (..),
    newTestInvokeAuthorizer,

    -- * Request Lenses
    testInvokeAuthorizer_additionalContext,
    testInvokeAuthorizer_body,
    testInvokeAuthorizer_headers,
    testInvokeAuthorizer_multiValueHeaders,
    testInvokeAuthorizer_pathWithQueryString,
    testInvokeAuthorizer_stageVariables,
    testInvokeAuthorizer_restApiId,
    testInvokeAuthorizer_authorizerId,

    -- * Destructuring the Response
    TestInvokeAuthorizerResponse (..),
    newTestInvokeAuthorizerResponse,

    -- * Response Lenses
    testInvokeAuthorizerResponse_authorization,
    testInvokeAuthorizerResponse_claims,
    testInvokeAuthorizerResponse_clientStatus,
    testInvokeAuthorizerResponse_latency,
    testInvokeAuthorizerResponse_log,
    testInvokeAuthorizerResponse_policy,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Make a request to simulate the invocation of an Authorizer.
--
-- /See:/ 'newTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | A key-value map of additional context variables.
    additionalContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The simulated request body of an incoming invocation request.
    body :: Prelude.Maybe Prelude.Text,
    -- | A key-value map of headers to simulate an incoming invocation request.
    -- This is where the incoming authorization token, or identity source,
    -- should be specified.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The headers as a map from string to list of values to simulate an
    -- incoming invocation request. This is where the incoming authorization
    -- token, or identity source, may be specified.
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
    -- | Specifies a test invoke authorizer request\'s Authorizer ID.
    authorizerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestInvokeAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalContext', 'testInvokeAuthorizer_additionalContext' - A key-value map of additional context variables.
--
-- 'body', 'testInvokeAuthorizer_body' - The simulated request body of an incoming invocation request.
--
-- 'headers', 'testInvokeAuthorizer_headers' - A key-value map of headers to simulate an incoming invocation request.
-- This is where the incoming authorization token, or identity source,
-- should be specified.
--
-- 'multiValueHeaders', 'testInvokeAuthorizer_multiValueHeaders' - The headers as a map from string to list of values to simulate an
-- incoming invocation request. This is where the incoming authorization
-- token, or identity source, may be specified.
--
-- 'pathWithQueryString', 'testInvokeAuthorizer_pathWithQueryString' - The URI path, including query string, of the simulated invocation
-- request. Use this to specify path parameters and query string
-- parameters.
--
-- 'stageVariables', 'testInvokeAuthorizer_stageVariables' - A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
--
-- 'restApiId', 'testInvokeAuthorizer_restApiId' - The string identifier of the associated RestApi.
--
-- 'authorizerId', 'testInvokeAuthorizer_authorizerId' - Specifies a test invoke authorizer request\'s Authorizer ID.
newTestInvokeAuthorizer ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'authorizerId'
  Prelude.Text ->
  TestInvokeAuthorizer
newTestInvokeAuthorizer pRestApiId_ pAuthorizerId_ =
  TestInvokeAuthorizer'
    { additionalContext =
        Prelude.Nothing,
      body = Prelude.Nothing,
      headers = Prelude.Nothing,
      multiValueHeaders = Prelude.Nothing,
      pathWithQueryString = Prelude.Nothing,
      stageVariables = Prelude.Nothing,
      restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | A key-value map of additional context variables.
testInvokeAuthorizer_additionalContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_additionalContext = Lens.lens (\TestInvokeAuthorizer' {additionalContext} -> additionalContext) (\s@TestInvokeAuthorizer' {} a -> s {additionalContext = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | The simulated request body of an incoming invocation request.
testInvokeAuthorizer_body :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_body = Lens.lens (\TestInvokeAuthorizer' {body} -> body) (\s@TestInvokeAuthorizer' {} a -> s {body = a} :: TestInvokeAuthorizer)

-- | A key-value map of headers to simulate an incoming invocation request.
-- This is where the incoming authorization token, or identity source,
-- should be specified.
testInvokeAuthorizer_headers :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_headers = Lens.lens (\TestInvokeAuthorizer' {headers} -> headers) (\s@TestInvokeAuthorizer' {} a -> s {headers = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | The headers as a map from string to list of values to simulate an
-- incoming invocation request. This is where the incoming authorization
-- token, or identity source, may be specified.
testInvokeAuthorizer_multiValueHeaders :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeAuthorizer_multiValueHeaders = Lens.lens (\TestInvokeAuthorizer' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeAuthorizer' {} a -> s {multiValueHeaders = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | The URI path, including query string, of the simulated invocation
-- request. Use this to specify path parameters and query string
-- parameters.
testInvokeAuthorizer_pathWithQueryString :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_pathWithQueryString = Lens.lens (\TestInvokeAuthorizer' {pathWithQueryString} -> pathWithQueryString) (\s@TestInvokeAuthorizer' {} a -> s {pathWithQueryString = a} :: TestInvokeAuthorizer)

-- | A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
testInvokeAuthorizer_stageVariables :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_stageVariables = Lens.lens (\TestInvokeAuthorizer' {stageVariables} -> stageVariables) (\s@TestInvokeAuthorizer' {} a -> s {stageVariables = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
testInvokeAuthorizer_restApiId :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_restApiId = Lens.lens (\TestInvokeAuthorizer' {restApiId} -> restApiId) (\s@TestInvokeAuthorizer' {} a -> s {restApiId = a} :: TestInvokeAuthorizer)

-- | Specifies a test invoke authorizer request\'s Authorizer ID.
testInvokeAuthorizer_authorizerId :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_authorizerId = Lens.lens (\TestInvokeAuthorizer' {authorizerId} -> authorizerId) (\s@TestInvokeAuthorizer' {} a -> s {authorizerId = a} :: TestInvokeAuthorizer)

instance Core.AWSRequest TestInvokeAuthorizer where
  type
    AWSResponse TestInvokeAuthorizer =
      TestInvokeAuthorizerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Prelude.<$> (x Data..?> "authorization" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "claims" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "clientStatus")
            Prelude.<*> (x Data..?> "latency")
            Prelude.<*> (x Data..?> "log")
            Prelude.<*> (x Data..?> "policy")
            Prelude.<*> (x Data..?> "principalId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestInvokeAuthorizer where
  hashWithSalt _salt TestInvokeAuthorizer' {..} =
    _salt `Prelude.hashWithSalt` additionalContext
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` multiValueHeaders
      `Prelude.hashWithSalt` pathWithQueryString
      `Prelude.hashWithSalt` stageVariables
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` authorizerId

instance Prelude.NFData TestInvokeAuthorizer where
  rnf TestInvokeAuthorizer' {..} =
    Prelude.rnf additionalContext
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf headers
      `Prelude.seq` Prelude.rnf multiValueHeaders
      `Prelude.seq` Prelude.rnf pathWithQueryString
      `Prelude.seq` Prelude.rnf stageVariables
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf authorizerId

instance Data.ToHeaders TestInvokeAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalContext" Data..=)
              Prelude.<$> additionalContext,
            ("body" Data..=) Prelude.<$> body,
            ("headers" Data..=) Prelude.<$> headers,
            ("multiValueHeaders" Data..=)
              Prelude.<$> multiValueHeaders,
            ("pathWithQueryString" Data..=)
              Prelude.<$> pathWithQueryString,
            ("stageVariables" Data..=)
              Prelude.<$> stageVariables
          ]
      )

instance Data.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/authorizers/",
        Data.toBS authorizerId
      ]

instance Data.ToQuery TestInvokeAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response of the test invoke request for a custom
-- Authorizer
--
-- /See:/ 'newTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The authorization response.
    authorization :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The open identity claims, with any supported custom attributes, returned
    -- from the Cognito Your User Pool configured for the API.
    claims :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The HTTP status code that the client would have received. Value is 0 if
    -- the authorizer succeeded.
    clientStatus :: Prelude.Maybe Prelude.Int,
    -- | The execution latency of the test authorizer request.
    latency :: Prelude.Maybe Prelude.Integer,
    -- | The API Gateway execution log for the test authorizer request.
    log :: Prelude.Maybe Prelude.Text,
    -- | The JSON policy document returned by the Authorizer
    policy :: Prelude.Maybe Prelude.Text,
    -- | The principal identity returned by the Authorizer
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestInvokeAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorization', 'testInvokeAuthorizerResponse_authorization' - The authorization response.
--
-- 'claims', 'testInvokeAuthorizerResponse_claims' - The open identity claims, with any supported custom attributes, returned
-- from the Cognito Your User Pool configured for the API.
--
-- 'clientStatus', 'testInvokeAuthorizerResponse_clientStatus' - The HTTP status code that the client would have received. Value is 0 if
-- the authorizer succeeded.
--
-- 'latency', 'testInvokeAuthorizerResponse_latency' - The execution latency of the test authorizer request.
--
-- 'log', 'testInvokeAuthorizerResponse_log' - The API Gateway execution log for the test authorizer request.
--
-- 'policy', 'testInvokeAuthorizerResponse_policy' - The JSON policy document returned by the Authorizer
--
-- 'principalId', 'testInvokeAuthorizerResponse_principalId' - The principal identity returned by the Authorizer
--
-- 'httpStatus', 'testInvokeAuthorizerResponse_httpStatus' - The response's http status code.
newTestInvokeAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestInvokeAuthorizerResponse
newTestInvokeAuthorizerResponse pHttpStatus_ =
  TestInvokeAuthorizerResponse'
    { authorization =
        Prelude.Nothing,
      claims = Prelude.Nothing,
      clientStatus = Prelude.Nothing,
      latency = Prelude.Nothing,
      log = Prelude.Nothing,
      policy = Prelude.Nothing,
      principalId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorization response.
testInvokeAuthorizerResponse_authorization :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeAuthorizerResponse_authorization = Lens.lens (\TestInvokeAuthorizerResponse' {authorization} -> authorization) (\s@TestInvokeAuthorizerResponse' {} a -> s {authorization = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The open identity claims, with any supported custom attributes, returned
-- from the Cognito Your User Pool configured for the API.
testInvokeAuthorizerResponse_claims :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizerResponse_claims = Lens.lens (\TestInvokeAuthorizerResponse' {claims} -> claims) (\s@TestInvokeAuthorizerResponse' {} a -> s {claims = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status code that the client would have received. Value is 0 if
-- the authorizer succeeded.
testInvokeAuthorizerResponse_clientStatus :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Int)
testInvokeAuthorizerResponse_clientStatus = Lens.lens (\TestInvokeAuthorizerResponse' {clientStatus} -> clientStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {clientStatus = a} :: TestInvokeAuthorizerResponse)

-- | The execution latency of the test authorizer request.
testInvokeAuthorizerResponse_latency :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Integer)
testInvokeAuthorizerResponse_latency = Lens.lens (\TestInvokeAuthorizerResponse' {latency} -> latency) (\s@TestInvokeAuthorizerResponse' {} a -> s {latency = a} :: TestInvokeAuthorizerResponse)

-- | The API Gateway execution log for the test authorizer request.
testInvokeAuthorizerResponse_log :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_log = Lens.lens (\TestInvokeAuthorizerResponse' {log} -> log) (\s@TestInvokeAuthorizerResponse' {} a -> s {log = a} :: TestInvokeAuthorizerResponse)

-- | The JSON policy document returned by the Authorizer
testInvokeAuthorizerResponse_policy :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_policy = Lens.lens (\TestInvokeAuthorizerResponse' {policy} -> policy) (\s@TestInvokeAuthorizerResponse' {} a -> s {policy = a} :: TestInvokeAuthorizerResponse)

-- | The principal identity returned by the Authorizer
testInvokeAuthorizerResponse_principalId :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_principalId = Lens.lens (\TestInvokeAuthorizerResponse' {principalId} -> principalId) (\s@TestInvokeAuthorizerResponse' {} a -> s {principalId = a} :: TestInvokeAuthorizerResponse)

-- | The response's http status code.
testInvokeAuthorizerResponse_httpStatus :: Lens.Lens' TestInvokeAuthorizerResponse Prelude.Int
testInvokeAuthorizerResponse_httpStatus = Lens.lens (\TestInvokeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {httpStatus = a} :: TestInvokeAuthorizerResponse)

instance Prelude.NFData TestInvokeAuthorizerResponse where
  rnf TestInvokeAuthorizerResponse' {..} =
    Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf claims
      `Prelude.seq` Prelude.rnf clientStatus
      `Prelude.seq` Prelude.rnf latency
      `Prelude.seq` Prelude.rnf log
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf httpStatus
