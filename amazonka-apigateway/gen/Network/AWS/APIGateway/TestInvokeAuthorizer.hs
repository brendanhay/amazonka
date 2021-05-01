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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Make a request to simulate the execution of an Authorizer.
--
-- /See:/ 'newTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | [Required] A key-value map of headers to simulate an incoming invocation
    -- request. This is where the incoming authorization token, or identity
    -- source, should be specified.
    headers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A key-value map of stage variables to simulate an invocation on a
    -- deployed Stage.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Optional] A key-value map of additional context variables.
    additionalContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Optional] The simulated request body of an incoming invocation request.
    body :: Prelude.Maybe Prelude.Text,
    -- | [Optional] The headers as a map from string to list of values to
    -- simulate an incoming invocation request. This is where the incoming
    -- authorization token, or identity source, may be specified.
    multiValueHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | [Optional] The URI path, including query string, of the simulated
    -- invocation request. Use this to specify path parameters and query string
    -- parameters.
    pathWithQueryString :: Prelude.Maybe Prelude.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
    authorizerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'authorizerId'
  Prelude.Text ->
  TestInvokeAuthorizer
newTestInvokeAuthorizer pRestApiId_ pAuthorizerId_ =
  TestInvokeAuthorizer'
    { headers = Prelude.Nothing,
      stageVariables = Prelude.Nothing,
      additionalContext = Prelude.Nothing,
      body = Prelude.Nothing,
      multiValueHeaders = Prelude.Nothing,
      pathWithQueryString = Prelude.Nothing,
      restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | [Required] A key-value map of headers to simulate an incoming invocation
-- request. This is where the incoming authorization token, or identity
-- source, should be specified.
testInvokeAuthorizer_headers :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_headers = Lens.lens (\TestInvokeAuthorizer' {headers} -> headers) (\s@TestInvokeAuthorizer' {} a -> s {headers = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Prelude._Coerce

-- | A key-value map of stage variables to simulate an invocation on a
-- deployed Stage.
testInvokeAuthorizer_stageVariables :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_stageVariables = Lens.lens (\TestInvokeAuthorizer' {stageVariables} -> stageVariables) (\s@TestInvokeAuthorizer' {} a -> s {stageVariables = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Prelude._Coerce

-- | [Optional] A key-value map of additional context variables.
testInvokeAuthorizer_additionalContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizer_additionalContext = Lens.lens (\TestInvokeAuthorizer' {additionalContext} -> additionalContext) (\s@TestInvokeAuthorizer' {} a -> s {additionalContext = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Prelude._Coerce

-- | [Optional] The simulated request body of an incoming invocation request.
testInvokeAuthorizer_body :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_body = Lens.lens (\TestInvokeAuthorizer' {body} -> body) (\s@TestInvokeAuthorizer' {} a -> s {body = a} :: TestInvokeAuthorizer)

-- | [Optional] The headers as a map from string to list of values to
-- simulate an incoming invocation request. This is where the incoming
-- authorization token, or identity source, may be specified.
testInvokeAuthorizer_multiValueHeaders :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeAuthorizer_multiValueHeaders = Lens.lens (\TestInvokeAuthorizer' {multiValueHeaders} -> multiValueHeaders) (\s@TestInvokeAuthorizer' {} a -> s {multiValueHeaders = a} :: TestInvokeAuthorizer) Prelude.. Lens.mapping Prelude._Coerce

-- | [Optional] The URI path, including query string, of the simulated
-- invocation request. Use this to specify path parameters and query string
-- parameters.
testInvokeAuthorizer_pathWithQueryString :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_pathWithQueryString = Lens.lens (\TestInvokeAuthorizer' {pathWithQueryString} -> pathWithQueryString) (\s@TestInvokeAuthorizer' {} a -> s {pathWithQueryString = a} :: TestInvokeAuthorizer)

-- | [Required] The string identifier of the associated RestApi.
testInvokeAuthorizer_restApiId :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_restApiId = Lens.lens (\TestInvokeAuthorizer' {restApiId} -> restApiId) (\s@TestInvokeAuthorizer' {} a -> s {restApiId = a} :: TestInvokeAuthorizer)

-- | [Required] Specifies a test invoke authorizer request\'s Authorizer ID.
testInvokeAuthorizer_authorizerId :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_authorizerId = Lens.lens (\TestInvokeAuthorizer' {authorizerId} -> authorizerId) (\s@TestInvokeAuthorizer' {} a -> s {authorizerId = a} :: TestInvokeAuthorizer)

instance Prelude.AWSRequest TestInvokeAuthorizer where
  type
    Rs TestInvokeAuthorizer =
      TestInvokeAuthorizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Prelude.<$> (x Prelude..?> "claims" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "clientStatus")
            Prelude.<*> (x Prelude..?> "principalId")
            Prelude.<*> (x Prelude..?> "log")
            Prelude.<*> ( x Prelude..?> "authorization"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "policy")
            Prelude.<*> (x Prelude..?> "latency")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestInvokeAuthorizer

instance Prelude.NFData TestInvokeAuthorizer

instance Prelude.ToHeaders TestInvokeAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("headers" Prelude..=) Prelude.<$> headers,
            ("stageVariables" Prelude..=)
              Prelude.<$> stageVariables,
            ("additionalContext" Prelude..=)
              Prelude.<$> additionalContext,
            ("body" Prelude..=) Prelude.<$> body,
            ("multiValueHeaders" Prelude..=)
              Prelude.<$> multiValueHeaders,
            ("pathWithQueryString" Prelude..=)
              Prelude.<$> pathWithQueryString
          ]
      )

instance Prelude.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/authorizers/",
        Prelude.toBS authorizerId
      ]

instance Prelude.ToQuery TestInvokeAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response of the test invoke request for a custom
-- Authorizer
--
-- /See:/ 'newTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The
    -- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
    -- with any supported custom attributes, returned from the Cognito Your
    -- User Pool configured for the API.
    claims :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The HTTP status code that the client would have received. Value is 0 if
    -- the authorizer succeeded.
    clientStatus :: Prelude.Maybe Prelude.Int,
    -- | The principal identity returned by the Authorizer
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The API Gateway execution log for the test authorizer request.
    log :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The JSON policy document returned by the Authorizer
    policy :: Prelude.Maybe Prelude.Text,
    -- | The execution latency of the test authorizer request.
    latency :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  TestInvokeAuthorizerResponse
newTestInvokeAuthorizerResponse pHttpStatus_ =
  TestInvokeAuthorizerResponse'
    { claims =
        Prelude.Nothing,
      clientStatus = Prelude.Nothing,
      principalId = Prelude.Nothing,
      log = Prelude.Nothing,
      authorization = Prelude.Nothing,
      policy = Prelude.Nothing,
      latency = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims>,
-- with any supported custom attributes, returned from the Cognito Your
-- User Pool configured for the API.
testInvokeAuthorizerResponse_claims :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testInvokeAuthorizerResponse_claims = Lens.lens (\TestInvokeAuthorizerResponse' {claims} -> claims) (\s@TestInvokeAuthorizerResponse' {} a -> s {claims = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The HTTP status code that the client would have received. Value is 0 if
-- the authorizer succeeded.
testInvokeAuthorizerResponse_clientStatus :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Int)
testInvokeAuthorizerResponse_clientStatus = Lens.lens (\TestInvokeAuthorizerResponse' {clientStatus} -> clientStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {clientStatus = a} :: TestInvokeAuthorizerResponse)

-- | The principal identity returned by the Authorizer
testInvokeAuthorizerResponse_principalId :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_principalId = Lens.lens (\TestInvokeAuthorizerResponse' {principalId} -> principalId) (\s@TestInvokeAuthorizerResponse' {} a -> s {principalId = a} :: TestInvokeAuthorizerResponse)

-- | The API Gateway execution log for the test authorizer request.
testInvokeAuthorizerResponse_log :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_log = Lens.lens (\TestInvokeAuthorizerResponse' {log} -> log) (\s@TestInvokeAuthorizerResponse' {} a -> s {log = a} :: TestInvokeAuthorizerResponse)

-- | Undocumented member.
testInvokeAuthorizerResponse_authorization :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
testInvokeAuthorizerResponse_authorization = Lens.lens (\TestInvokeAuthorizerResponse' {authorization} -> authorization) (\s@TestInvokeAuthorizerResponse' {} a -> s {authorization = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The JSON policy document returned by the Authorizer
testInvokeAuthorizerResponse_policy :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_policy = Lens.lens (\TestInvokeAuthorizerResponse' {policy} -> policy) (\s@TestInvokeAuthorizerResponse' {} a -> s {policy = a} :: TestInvokeAuthorizerResponse)

-- | The execution latency of the test authorizer request.
testInvokeAuthorizerResponse_latency :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Integer)
testInvokeAuthorizerResponse_latency = Lens.lens (\TestInvokeAuthorizerResponse' {latency} -> latency) (\s@TestInvokeAuthorizerResponse' {} a -> s {latency = a} :: TestInvokeAuthorizerResponse)

-- | The response's http status code.
testInvokeAuthorizerResponse_httpStatus :: Lens.Lens' TestInvokeAuthorizerResponse Prelude.Int
testInvokeAuthorizerResponse_httpStatus = Lens.lens (\TestInvokeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {httpStatus = a} :: TestInvokeAuthorizerResponse)

instance Prelude.NFData TestInvokeAuthorizerResponse
