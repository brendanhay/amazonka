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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of an Authorizer in your RestApi with headers,
-- parameters, and an incoming request body.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer>
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer>
module Amazonka.APIGateway.TestInvokeAuthorizer
  ( -- * Creating a Request
    TestInvokeAuthorizer (..),
    newTestInvokeAuthorizer,

    -- * Request Lenses
    testInvokeAuthorizer_pathWithQueryString,
    testInvokeAuthorizer_body,
    testInvokeAuthorizer_additionalContext,
    testInvokeAuthorizer_stageVariables,
    testInvokeAuthorizer_headers,
    testInvokeAuthorizer_multiValueHeaders,
    testInvokeAuthorizer_restApiId,
    testInvokeAuthorizer_authorizerId,

    -- * Destructuring the Response
    TestInvokeAuthorizerResponse (..),
    newTestInvokeAuthorizerResponse,

    -- * Response Lenses
    testInvokeAuthorizerResponse_log,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_latency,
    testInvokeAuthorizerResponse_authorization,
    testInvokeAuthorizerResponse_claims,
    testInvokeAuthorizerResponse_clientStatus,
    testInvokeAuthorizerResponse_policy,
    testInvokeAuthorizerResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Make a request to simulate the execution of an Authorizer.
--
-- /See:/ 'newTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | [Optional] The URI path, including query string, of the simulated
    -- invocation request. Use this to specify path parameters and query string
    -- parameters.
    pathWithQueryString :: Prelude.Maybe Prelude.Text,
    -- | [Optional] The simulated request body of an incoming invocation request.
    body :: Prelude.Maybe Prelude.Text,
    -- | [Optional] A key-value map of additional context variables.
    additionalContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A key-value map of stage variables to simulate an invocation on a
    -- deployed Stage.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Required] A key-value map of headers to simulate an incoming invocation
    -- request. This is where the incoming authorization token, or identity
    -- source, should be specified.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Optional] The headers as a map from string to list of values to
    -- simulate an incoming invocation request. This is where the incoming
    -- authorization token, or identity source, may be specified.
    multiValueHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
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
-- 'pathWithQueryString', 'testInvokeAuthorizer_pathWithQueryString' - [Optional] The URI path, including query string, of the simulated
-- invocation request. Use this to specify path parameters and query string
-- parameters.
--
-- 'body', 'testInvokeAuthorizer_body' - [Optional] The simulated request body of an incoming invocation request.
--
-- 'additionalContext', 'testInvokeAuthorizer_additionalContext' - [Optional] A key-value map of additional context variables.
--
-- 'stageVariables', 'testInvokeAuthorizer_stageVariables' - A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
--
-- 'headers', 'testInvokeAuthorizer_headers' - [Required] A key-value map of headers to simulate an incoming invocation
-- request. This is where the incoming authorization token, or identity
-- source, should be specified.
--
-- 'multiValueHeaders', 'testInvokeAuthorizer_multiValueHeaders' - [Optional] The headers as a map from string to list of values to
-- simulate an incoming invocation request. This is where the incoming
-- authorization token, or identity source, may be specified.
--
-- 'restApiId', 'testInvokeAuthorizer_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'authorizerId', 'testInvokeAuthorizer_authorizerId' - [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
newTestInvokeAuthorizer ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'authorizerId'
  Prelude.Text ->
  TestInvokeAuthorizer
newTestInvokeAuthorizer pRestApiId_ pAuthorizerId_ =
  TestInvokeAuthorizer'
    { pathWithQueryString =
        Prelude.Nothing,
      body = Prelude.Nothing,
      additionalContext = Prelude.Nothing,
      stageVariables = Prelude.Nothing,
      headers = Prelude.Nothing,
      multiValueHeaders = Prelude.Nothing,
      restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | [Optional] The URI path, including query string, of the simulated
-- invocation request. Use this to specify path parameters and query string
-- parameters.
testInvokeAuthorizer_pathWithQueryString :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_pathWithQueryString = Lens.lens (\TestInvokeAuthorizer' {pathWithQueryString} -> pathWithQueryString) (\s@TestInvokeAuthorizer' {} a -> s {pathWithQueryString = a} :: TestInvokeAuthorizer)

-- | [Optional] The simulated request body of an incoming invocation request.
testInvokeAuthorizer_body :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_body = Lens.lens (\TestInvokeAuthorizer' {body} -> body) (\s@TestInvokeAuthorizer' {} a -> s {body = a} :: TestInvokeAuthorizer)

-- | [Optional] A key-value map of additional context variables.
testInvokeAuthorizer_additionalContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_additionalContext = Lens.lens (\TestInvokeAuthorizer' {additionalContext} -> additionalContext) (\s@TestInvokeAuthorizer' {} a -> s {additionalContext = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
testInvokeAuthorizer_stageVariables :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_stageVariables = Lens.lens (\TestInvokeAuthorizer' {stageVariables} -> stageVariables) (\s@TestInvokeAuthorizer' {} a -> s {stageVariables = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | [Required] A key-value map of headers to simulate an incoming invocation
-- request. This is where the incoming authorization token, or identity
-- source, should be specified.
testInvokeAuthorizer_headers :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_headers = Lens.lens (\TestInvokeAuthorizer' {headers} -> headers) (\s@TestInvokeAuthorizer' {} a -> s {headers = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | [Optional] The headers as a map from string to list of values to
-- simulate an incoming invocation request. This is where the incoming
-- authorization token, or identity source, may be specified.
testInvokeAuthorizer_multiValueHeaders :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeAuthorizer_multiValueHeaders = Lens.lens (\TestInvokeAuthorizer' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeAuthorizer' {} a -> s {multiValueHeaders = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | [Required] The string identifier of the associated RestApi.
testInvokeAuthorizer_restApiId :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_restApiId = Lens.lens (\TestInvokeAuthorizer' {restApiId} -> restApiId) (\s@TestInvokeAuthorizer' {} a -> s {restApiId = a} :: TestInvokeAuthorizer)

-- | [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
testInvokeAuthorizer_authorizerId :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_authorizerId = Lens.lens (\TestInvokeAuthorizer' {authorizerId} -> authorizerId) (\s@TestInvokeAuthorizer' {} a -> s {authorizerId = a} :: TestInvokeAuthorizer)

instance Core.AWSRequest TestInvokeAuthorizer where
  type
    AWSResponse TestInvokeAuthorizer =
      TestInvokeAuthorizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Prelude.<$> (x Core..?> "log")
            Prelude.<*> (x Core..?> "principalId")
            Prelude.<*> (x Core..?> "latency")
            Prelude.<*> (x Core..?> "authorization" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "claims" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "clientStatus")
            Prelude.<*> (x Core..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestInvokeAuthorizer

instance Prelude.NFData TestInvokeAuthorizer

instance Core.ToHeaders TestInvokeAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pathWithQueryString" Core..=)
              Prelude.<$> pathWithQueryString,
            ("body" Core..=) Prelude.<$> body,
            ("additionalContext" Core..=)
              Prelude.<$> additionalContext,
            ("stageVariables" Core..=)
              Prelude.<$> stageVariables,
            ("headers" Core..=) Prelude.<$> headers,
            ("multiValueHeaders" Core..=)
              Prelude.<$> multiValueHeaders
          ]
      )

instance Core.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/authorizers/",
        Core.toBS authorizerId
      ]

instance Core.ToQuery TestInvokeAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response of the test invoke request for a custom
-- Authorizer
--
-- /See:/ 'newTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The API Gateway execution log for the test authorizer request.
    log :: Prelude.Maybe Prelude.Text,
    -- | The principal identity returned by the Authorizer
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The execution latency of the test authorizer request.
    latency :: Prelude.Maybe Prelude.Integer,
    authorization :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The
    -- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
    -- with any supported custom attributes, returned from the Cognito Your
    -- User Pool configured for the API.
    claims :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The HTTP status code that the client would have received. Value is 0 if
    -- the authorizer succeeded.
    clientStatus :: Prelude.Maybe Prelude.Int,
    -- | The JSON policy document returned by the Authorizer
    policy :: Prelude.Maybe Prelude.Text,
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
-- 'log', 'testInvokeAuthorizerResponse_log' - The API Gateway execution log for the test authorizer request.
--
-- 'principalId', 'testInvokeAuthorizerResponse_principalId' - The principal identity returned by the Authorizer
--
-- 'latency', 'testInvokeAuthorizerResponse_latency' - The execution latency of the test authorizer request.
--
-- 'authorization', 'testInvokeAuthorizerResponse_authorization' - Undocumented member.
--
-- 'claims', 'testInvokeAuthorizerResponse_claims' - The
-- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
-- with any supported custom attributes, returned from the Cognito Your
-- User Pool configured for the API.
--
-- 'clientStatus', 'testInvokeAuthorizerResponse_clientStatus' - The HTTP status code that the client would have received. Value is 0 if
-- the authorizer succeeded.
--
-- 'policy', 'testInvokeAuthorizerResponse_policy' - The JSON policy document returned by the Authorizer
--
-- 'httpStatus', 'testInvokeAuthorizerResponse_httpStatus' - The response's http status code.
newTestInvokeAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestInvokeAuthorizerResponse
newTestInvokeAuthorizerResponse pHttpStatus_ =
  TestInvokeAuthorizerResponse'
    { log =
        Prelude.Nothing,
      principalId = Prelude.Nothing,
      latency = Prelude.Nothing,
      authorization = Prelude.Nothing,
      claims = Prelude.Nothing,
      clientStatus = Prelude.Nothing,
      policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API Gateway execution log for the test authorizer request.
testInvokeAuthorizerResponse_log :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_log = Lens.lens (\TestInvokeAuthorizerResponse' {log} -> log) (\s@TestInvokeAuthorizerResponse' {} a -> s {log = a} :: TestInvokeAuthorizerResponse)

-- | The principal identity returned by the Authorizer
testInvokeAuthorizerResponse_principalId :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_principalId = Lens.lens (\TestInvokeAuthorizerResponse' {principalId} -> principalId) (\s@TestInvokeAuthorizerResponse' {} a -> s {principalId = a} :: TestInvokeAuthorizerResponse)

-- | The execution latency of the test authorizer request.
testInvokeAuthorizerResponse_latency :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Integer)
testInvokeAuthorizerResponse_latency = Lens.lens (\TestInvokeAuthorizerResponse' {latency} -> latency) (\s@TestInvokeAuthorizerResponse' {} a -> s {latency = a} :: TestInvokeAuthorizerResponse)

-- | Undocumented member.
testInvokeAuthorizerResponse_authorization :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeAuthorizerResponse_authorization = Lens.lens (\TestInvokeAuthorizerResponse' {authorization} -> authorization) (\s@TestInvokeAuthorizerResponse' {} a -> s {authorization = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
-- with any supported custom attributes, returned from the Cognito Your
-- User Pool configured for the API.
testInvokeAuthorizerResponse_claims :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizerResponse_claims = Lens.lens (\TestInvokeAuthorizerResponse' {claims} -> claims) (\s@TestInvokeAuthorizerResponse' {} a -> s {claims = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status code that the client would have received. Value is 0 if
-- the authorizer succeeded.
testInvokeAuthorizerResponse_clientStatus :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Int)
testInvokeAuthorizerResponse_clientStatus = Lens.lens (\TestInvokeAuthorizerResponse' {clientStatus} -> clientStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {clientStatus = a} :: TestInvokeAuthorizerResponse)

-- | The JSON policy document returned by the Authorizer
testInvokeAuthorizerResponse_policy :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_policy = Lens.lens (\TestInvokeAuthorizerResponse' {policy} -> policy) (\s@TestInvokeAuthorizerResponse' {} a -> s {policy = a} :: TestInvokeAuthorizerResponse)

-- | The response's http status code.
testInvokeAuthorizerResponse_httpStatus :: Lens.Lens' TestInvokeAuthorizerResponse Prelude.Int
testInvokeAuthorizerResponse_httpStatus = Lens.lens (\TestInvokeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {httpStatus = a} :: TestInvokeAuthorizerResponse)

instance Prelude.NFData TestInvokeAuthorizerResponse
