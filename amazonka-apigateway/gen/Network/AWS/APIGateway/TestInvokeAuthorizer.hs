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
-- Module      : Network.AWS.APIGateway.TestInvokeAuthorizer
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
module Network.AWS.APIGateway.TestInvokeAuthorizer
  ( -- * Creating a Request
    TestInvokeAuthorizer (..),
    newTestInvokeAuthorizer,

    -- * Request Lenses
    testInvokeAuthorizer_headers,
    testInvokeAuthorizer_stageVariables,
    testInvokeAuthorizer_additionalContext,
    testInvokeAuthorizer_body,
    testInvokeAuthorizer_multiValueHeaders,
    testInvokeAuthorizer_pathWithQueryString,
    testInvokeAuthorizer_restApiId,
    testInvokeAuthorizer_authorizerId,

    -- * Destructuring the Response
    TestInvokeAuthorizerResponse (..),
    newTestInvokeAuthorizerResponse,

    -- * Response Lenses
    testInvokeAuthorizerResponse_claims,
    testInvokeAuthorizerResponse_clientStatus,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_log,
    testInvokeAuthorizerResponse_authorization,
    testInvokeAuthorizerResponse_policy,
    testInvokeAuthorizerResponse_latency,
    testInvokeAuthorizerResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Make a request to simulate the execution of an Authorizer.
--
-- /See:/ 'newTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | [Required] A key-value map of headers to simulate an incoming invocation
    -- request. This is where the incoming authorization token, or identity
    -- source, should be specified.
    headers :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A key-value map of stage variables to simulate an invocation on a
    -- deployed Stage.
    stageVariables :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | [Optional] A key-value map of additional context variables.
    additionalContext :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | [Optional] The simulated request body of an incoming invocation request.
    body :: Core.Maybe Core.Text,
    -- | [Optional] The headers as a map from string to list of values to
    -- simulate an incoming invocation request. This is where the incoming
    -- authorization token, or identity source, may be specified.
    multiValueHeaders :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | [Optional] The URI path, including query string, of the simulated
    -- invocation request. Use this to specify path parameters and query string
    -- parameters.
    pathWithQueryString :: Core.Maybe Core.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
    authorizerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestInvokeAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'testInvokeAuthorizer_headers' - [Required] A key-value map of headers to simulate an incoming invocation
-- request. This is where the incoming authorization token, or identity
-- source, should be specified.
--
-- 'stageVariables', 'testInvokeAuthorizer_stageVariables' - A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
--
-- 'additionalContext', 'testInvokeAuthorizer_additionalContext' - [Optional] A key-value map of additional context variables.
--
-- 'body', 'testInvokeAuthorizer_body' - [Optional] The simulated request body of an incoming invocation request.
--
-- 'multiValueHeaders', 'testInvokeAuthorizer_multiValueHeaders' - [Optional] The headers as a map from string to list of values to
-- simulate an incoming invocation request. This is where the incoming
-- authorization token, or identity source, may be specified.
--
-- 'pathWithQueryString', 'testInvokeAuthorizer_pathWithQueryString' - [Optional] The URI path, including query string, of the simulated
-- invocation request. Use this to specify path parameters and query string
-- parameters.
--
-- 'restApiId', 'testInvokeAuthorizer_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'authorizerId', 'testInvokeAuthorizer_authorizerId' - [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
newTestInvokeAuthorizer ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'authorizerId'
  Core.Text ->
  TestInvokeAuthorizer
newTestInvokeAuthorizer pRestApiId_ pAuthorizerId_ =
  TestInvokeAuthorizer'
    { headers = Core.Nothing,
      stageVariables = Core.Nothing,
      additionalContext = Core.Nothing,
      body = Core.Nothing,
      multiValueHeaders = Core.Nothing,
      pathWithQueryString = Core.Nothing,
      restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | [Required] A key-value map of headers to simulate an incoming invocation
-- request. This is where the incoming authorization token, or identity
-- source, should be specified.
testInvokeAuthorizer_headers :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
testInvokeAuthorizer_headers = Lens.lens (\TestInvokeAuthorizer' {headers} -> headers) (\s@TestInvokeAuthorizer' {} a -> s {headers = a} :: TestInvokeAuthorizer) Core.. Lens.mapping Lens._Coerce

-- | A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
testInvokeAuthorizer_stageVariables :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
testInvokeAuthorizer_stageVariables = Lens.lens (\TestInvokeAuthorizer' {stageVariables} -> stageVariables) (\s@TestInvokeAuthorizer' {} a -> s {stageVariables = a} :: TestInvokeAuthorizer) Core.. Lens.mapping Lens._Coerce

-- | [Optional] A key-value map of additional context variables.
testInvokeAuthorizer_additionalContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
testInvokeAuthorizer_additionalContext = Lens.lens (\TestInvokeAuthorizer' {additionalContext} -> additionalContext) (\s@TestInvokeAuthorizer' {} a -> s {additionalContext = a} :: TestInvokeAuthorizer) Core.. Lens.mapping Lens._Coerce

-- | [Optional] The simulated request body of an incoming invocation request.
testInvokeAuthorizer_body :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Core.Text)
testInvokeAuthorizer_body = Lens.lens (\TestInvokeAuthorizer' {body} -> body) (\s@TestInvokeAuthorizer' {} a -> s {body = a} :: TestInvokeAuthorizer)

-- | [Optional] The headers as a map from string to list of values to
-- simulate an incoming invocation request. This is where the incoming
-- authorization token, or identity source, may be specified.
testInvokeAuthorizer_multiValueHeaders :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
testInvokeAuthorizer_multiValueHeaders = Lens.lens (\TestInvokeAuthorizer' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeAuthorizer' {} a -> s {multiValueHeaders = a} :: TestInvokeAuthorizer) Core.. Lens.mapping Lens._Coerce

-- | [Optional] The URI path, including query string, of the simulated
-- invocation request. Use this to specify path parameters and query string
-- parameters.
testInvokeAuthorizer_pathWithQueryString :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Core.Text)
testInvokeAuthorizer_pathWithQueryString = Lens.lens (\TestInvokeAuthorizer' {pathWithQueryString} -> pathWithQueryString) (\s@TestInvokeAuthorizer' {} a -> s {pathWithQueryString = a} :: TestInvokeAuthorizer)

-- | [Required] The string identifier of the associated RestApi.
testInvokeAuthorizer_restApiId :: Lens.Lens' TestInvokeAuthorizer Core.Text
testInvokeAuthorizer_restApiId = Lens.lens (\TestInvokeAuthorizer' {restApiId} -> restApiId) (\s@TestInvokeAuthorizer' {} a -> s {restApiId = a} :: TestInvokeAuthorizer)

-- | [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
testInvokeAuthorizer_authorizerId :: Lens.Lens' TestInvokeAuthorizer Core.Text
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
            Core.<$> (x Core..?> "claims" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "clientStatus")
            Core.<*> (x Core..?> "principalId")
            Core.<*> (x Core..?> "log")
            Core.<*> (x Core..?> "authorization" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "policy")
            Core.<*> (x Core..?> "latency")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TestInvokeAuthorizer

instance Core.NFData TestInvokeAuthorizer

instance Core.ToHeaders TestInvokeAuthorizer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("headers" Core..=) Core.<$> headers,
            ("stageVariables" Core..=) Core.<$> stageVariables,
            ("additionalContext" Core..=)
              Core.<$> additionalContext,
            ("body" Core..=) Core.<$> body,
            ("multiValueHeaders" Core..=)
              Core.<$> multiValueHeaders,
            ("pathWithQueryString" Core..=)
              Core.<$> pathWithQueryString
          ]
      )

instance Core.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/authorizers/",
        Core.toBS authorizerId
      ]

instance Core.ToQuery TestInvokeAuthorizer where
  toQuery = Core.const Core.mempty

-- | Represents the response of the test invoke request for a custom
-- Authorizer
--
-- /See:/ 'newTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The
    -- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
    -- with any supported custom attributes, returned from the Cognito Your
    -- User Pool configured for the API.
    claims :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The HTTP status code that the client would have received. Value is 0 if
    -- the authorizer succeeded.
    clientStatus :: Core.Maybe Core.Int,
    -- | The principal identity returned by the Authorizer
    principalId :: Core.Maybe Core.Text,
    -- | The API Gateway execution log for the test authorizer request.
    log :: Core.Maybe Core.Text,
    authorization :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The JSON policy document returned by the Authorizer
    policy :: Core.Maybe Core.Text,
    -- | The execution latency of the test authorizer request.
    latency :: Core.Maybe Core.Integer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestInvokeAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'claims', 'testInvokeAuthorizerResponse_claims' - The
-- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
-- with any supported custom attributes, returned from the Cognito Your
-- User Pool configured for the API.
--
-- 'clientStatus', 'testInvokeAuthorizerResponse_clientStatus' - The HTTP status code that the client would have received. Value is 0 if
-- the authorizer succeeded.
--
-- 'principalId', 'testInvokeAuthorizerResponse_principalId' - The principal identity returned by the Authorizer
--
-- 'log', 'testInvokeAuthorizerResponse_log' - The API Gateway execution log for the test authorizer request.
--
-- 'authorization', 'testInvokeAuthorizerResponse_authorization' - Undocumented member.
--
-- 'policy', 'testInvokeAuthorizerResponse_policy' - The JSON policy document returned by the Authorizer
--
-- 'latency', 'testInvokeAuthorizerResponse_latency' - The execution latency of the test authorizer request.
--
-- 'httpStatus', 'testInvokeAuthorizerResponse_httpStatus' - The response's http status code.
newTestInvokeAuthorizerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TestInvokeAuthorizerResponse
newTestInvokeAuthorizerResponse pHttpStatus_ =
  TestInvokeAuthorizerResponse'
    { claims =
        Core.Nothing,
      clientStatus = Core.Nothing,
      principalId = Core.Nothing,
      log = Core.Nothing,
      authorization = Core.Nothing,
      policy = Core.Nothing,
      latency = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
-- with any supported custom attributes, returned from the Cognito Your
-- User Pool configured for the API.
testInvokeAuthorizerResponse_claims :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
testInvokeAuthorizerResponse_claims = Lens.lens (\TestInvokeAuthorizerResponse' {claims} -> claims) (\s@TestInvokeAuthorizerResponse' {} a -> s {claims = a} :: TestInvokeAuthorizerResponse) Core.. Lens.mapping Lens._Coerce

-- | The HTTP status code that the client would have received. Value is 0 if
-- the authorizer succeeded.
testInvokeAuthorizerResponse_clientStatus :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Int)
testInvokeAuthorizerResponse_clientStatus = Lens.lens (\TestInvokeAuthorizerResponse' {clientStatus} -> clientStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {clientStatus = a} :: TestInvokeAuthorizerResponse)

-- | The principal identity returned by the Authorizer
testInvokeAuthorizerResponse_principalId :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Text)
testInvokeAuthorizerResponse_principalId = Lens.lens (\TestInvokeAuthorizerResponse' {principalId} -> principalId) (\s@TestInvokeAuthorizerResponse' {} a -> s {principalId = a} :: TestInvokeAuthorizerResponse)

-- | The API Gateway execution log for the test authorizer request.
testInvokeAuthorizerResponse_log :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Text)
testInvokeAuthorizerResponse_log = Lens.lens (\TestInvokeAuthorizerResponse' {log} -> log) (\s@TestInvokeAuthorizerResponse' {} a -> s {log = a} :: TestInvokeAuthorizerResponse)

-- | Undocumented member.
testInvokeAuthorizerResponse_authorization :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
testInvokeAuthorizerResponse_authorization = Lens.lens (\TestInvokeAuthorizerResponse' {authorization} -> authorization) (\s@TestInvokeAuthorizerResponse' {} a -> s {authorization = a} :: TestInvokeAuthorizerResponse) Core.. Lens.mapping Lens._Coerce

-- | The JSON policy document returned by the Authorizer
testInvokeAuthorizerResponse_policy :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Text)
testInvokeAuthorizerResponse_policy = Lens.lens (\TestInvokeAuthorizerResponse' {policy} -> policy) (\s@TestInvokeAuthorizerResponse' {} a -> s {policy = a} :: TestInvokeAuthorizerResponse)

-- | The execution latency of the test authorizer request.
testInvokeAuthorizerResponse_latency :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Integer)
testInvokeAuthorizerResponse_latency = Lens.lens (\TestInvokeAuthorizerResponse' {latency} -> latency) (\s@TestInvokeAuthorizerResponse' {} a -> s {latency = a} :: TestInvokeAuthorizerResponse)

-- | The response's http status code.
testInvokeAuthorizerResponse_httpStatus :: Lens.Lens' TestInvokeAuthorizerResponse Core.Int
testInvokeAuthorizerResponse_httpStatus = Lens.lens (\TestInvokeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {httpStatus = a} :: TestInvokeAuthorizerResponse)

instance Core.NFData TestInvokeAuthorizerResponse
