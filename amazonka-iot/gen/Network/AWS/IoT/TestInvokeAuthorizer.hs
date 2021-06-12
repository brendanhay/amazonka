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
-- Module      : Network.AWS.IoT.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a custom authorization behavior by invoking a specified custom
-- authorizer. Use this to test and debug the custom authorization behavior
-- of devices that connect to the AWS IoT device gateway.
module Network.AWS.IoT.TestInvokeAuthorizer
  ( -- * Creating a Request
    TestInvokeAuthorizer (..),
    newTestInvokeAuthorizer,

    -- * Request Lenses
    testInvokeAuthorizer_httpContext,
    testInvokeAuthorizer_mqttContext,
    testInvokeAuthorizer_tokenSignature,
    testInvokeAuthorizer_tlsContext,
    testInvokeAuthorizer_token,
    testInvokeAuthorizer_authorizerName,

    -- * Destructuring the Response
    TestInvokeAuthorizerResponse (..),
    newTestInvokeAuthorizerResponse,

    -- * Response Lenses
    testInvokeAuthorizerResponse_disconnectAfterInSeconds,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_policyDocuments,
    testInvokeAuthorizerResponse_isAuthenticated,
    testInvokeAuthorizerResponse_refreshAfterInSeconds,
    testInvokeAuthorizerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | Specifies a test HTTP authorization request.
    httpContext :: Core.Maybe HttpContext,
    -- | Specifies a test MQTT authorization request.
    mqttContext :: Core.Maybe MqttContext,
    -- | The signature made with the token and your custom authentication
    -- service\'s private key. This value must be Base-64-encoded.
    tokenSignature :: Core.Maybe Core.Text,
    -- | Specifies a test TLS authorization request.
    tlsContext :: Core.Maybe TlsContext,
    -- | The token returned by your custom authentication service.
    token :: Core.Maybe Core.Text,
    -- | The custom authorizer name.
    authorizerName :: Core.Text
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
-- 'httpContext', 'testInvokeAuthorizer_httpContext' - Specifies a test HTTP authorization request.
--
-- 'mqttContext', 'testInvokeAuthorizer_mqttContext' - Specifies a test MQTT authorization request.
--
-- 'tokenSignature', 'testInvokeAuthorizer_tokenSignature' - The signature made with the token and your custom authentication
-- service\'s private key. This value must be Base-64-encoded.
--
-- 'tlsContext', 'testInvokeAuthorizer_tlsContext' - Specifies a test TLS authorization request.
--
-- 'token', 'testInvokeAuthorizer_token' - The token returned by your custom authentication service.
--
-- 'authorizerName', 'testInvokeAuthorizer_authorizerName' - The custom authorizer name.
newTestInvokeAuthorizer ::
  -- | 'authorizerName'
  Core.Text ->
  TestInvokeAuthorizer
newTestInvokeAuthorizer pAuthorizerName_ =
  TestInvokeAuthorizer'
    { httpContext = Core.Nothing,
      mqttContext = Core.Nothing,
      tokenSignature = Core.Nothing,
      tlsContext = Core.Nothing,
      token = Core.Nothing,
      authorizerName = pAuthorizerName_
    }

-- | Specifies a test HTTP authorization request.
testInvokeAuthorizer_httpContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe HttpContext)
testInvokeAuthorizer_httpContext = Lens.lens (\TestInvokeAuthorizer' {httpContext} -> httpContext) (\s@TestInvokeAuthorizer' {} a -> s {httpContext = a} :: TestInvokeAuthorizer)

-- | Specifies a test MQTT authorization request.
testInvokeAuthorizer_mqttContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe MqttContext)
testInvokeAuthorizer_mqttContext = Lens.lens (\TestInvokeAuthorizer' {mqttContext} -> mqttContext) (\s@TestInvokeAuthorizer' {} a -> s {mqttContext = a} :: TestInvokeAuthorizer)

-- | The signature made with the token and your custom authentication
-- service\'s private key. This value must be Base-64-encoded.
testInvokeAuthorizer_tokenSignature :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Core.Text)
testInvokeAuthorizer_tokenSignature = Lens.lens (\TestInvokeAuthorizer' {tokenSignature} -> tokenSignature) (\s@TestInvokeAuthorizer' {} a -> s {tokenSignature = a} :: TestInvokeAuthorizer)

-- | Specifies a test TLS authorization request.
testInvokeAuthorizer_tlsContext :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe TlsContext)
testInvokeAuthorizer_tlsContext = Lens.lens (\TestInvokeAuthorizer' {tlsContext} -> tlsContext) (\s@TestInvokeAuthorizer' {} a -> s {tlsContext = a} :: TestInvokeAuthorizer)

-- | The token returned by your custom authentication service.
testInvokeAuthorizer_token :: Lens.Lens' TestInvokeAuthorizer (Core.Maybe Core.Text)
testInvokeAuthorizer_token = Lens.lens (\TestInvokeAuthorizer' {token} -> token) (\s@TestInvokeAuthorizer' {} a -> s {token = a} :: TestInvokeAuthorizer)

-- | The custom authorizer name.
testInvokeAuthorizer_authorizerName :: Lens.Lens' TestInvokeAuthorizer Core.Text
testInvokeAuthorizer_authorizerName = Lens.lens (\TestInvokeAuthorizer' {authorizerName} -> authorizerName) (\s@TestInvokeAuthorizer' {} a -> s {authorizerName = a} :: TestInvokeAuthorizer)

instance Core.AWSRequest TestInvokeAuthorizer where
  type
    AWSResponse TestInvokeAuthorizer =
      TestInvokeAuthorizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Core.<$> (x Core..?> "disconnectAfterInSeconds")
            Core.<*> (x Core..?> "principalId")
            Core.<*> (x Core..?> "policyDocuments" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "isAuthenticated")
            Core.<*> (x Core..?> "refreshAfterInSeconds")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TestInvokeAuthorizer

instance Core.NFData TestInvokeAuthorizer

instance Core.ToHeaders TestInvokeAuthorizer where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("httpContext" Core..=) Core.<$> httpContext,
            ("mqttContext" Core..=) Core.<$> mqttContext,
            ("tokenSignature" Core..=) Core.<$> tokenSignature,
            ("tlsContext" Core..=) Core.<$> tlsContext,
            ("token" Core..=) Core.<$> token
          ]
      )

instance Core.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Core.mconcat
      ["/authorizer/", Core.toBS authorizerName, "/test"]

instance Core.ToQuery TestInvokeAuthorizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The number of seconds after which the connection is terminated.
    disconnectAfterInSeconds :: Core.Maybe Core.Int,
    -- | The principal ID.
    principalId :: Core.Maybe Core.Text,
    -- | IAM policy documents.
    policyDocuments :: Core.Maybe [Core.Text],
    -- | True if the token is authenticated, otherwise false.
    isAuthenticated :: Core.Maybe Core.Bool,
    -- | The number of seconds after which the temporary credentials are
    -- refreshed.
    refreshAfterInSeconds :: Core.Maybe Core.Int,
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
-- 'disconnectAfterInSeconds', 'testInvokeAuthorizerResponse_disconnectAfterInSeconds' - The number of seconds after which the connection is terminated.
--
-- 'principalId', 'testInvokeAuthorizerResponse_principalId' - The principal ID.
--
-- 'policyDocuments', 'testInvokeAuthorizerResponse_policyDocuments' - IAM policy documents.
--
-- 'isAuthenticated', 'testInvokeAuthorizerResponse_isAuthenticated' - True if the token is authenticated, otherwise false.
--
-- 'refreshAfterInSeconds', 'testInvokeAuthorizerResponse_refreshAfterInSeconds' - The number of seconds after which the temporary credentials are
-- refreshed.
--
-- 'httpStatus', 'testInvokeAuthorizerResponse_httpStatus' - The response's http status code.
newTestInvokeAuthorizerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TestInvokeAuthorizerResponse
newTestInvokeAuthorizerResponse pHttpStatus_ =
  TestInvokeAuthorizerResponse'
    { disconnectAfterInSeconds =
        Core.Nothing,
      principalId = Core.Nothing,
      policyDocuments = Core.Nothing,
      isAuthenticated = Core.Nothing,
      refreshAfterInSeconds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of seconds after which the connection is terminated.
testInvokeAuthorizerResponse_disconnectAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Int)
testInvokeAuthorizerResponse_disconnectAfterInSeconds = Lens.lens (\TestInvokeAuthorizerResponse' {disconnectAfterInSeconds} -> disconnectAfterInSeconds) (\s@TestInvokeAuthorizerResponse' {} a -> s {disconnectAfterInSeconds = a} :: TestInvokeAuthorizerResponse)

-- | The principal ID.
testInvokeAuthorizerResponse_principalId :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Text)
testInvokeAuthorizerResponse_principalId = Lens.lens (\TestInvokeAuthorizerResponse' {principalId} -> principalId) (\s@TestInvokeAuthorizerResponse' {} a -> s {principalId = a} :: TestInvokeAuthorizerResponse)

-- | IAM policy documents.
testInvokeAuthorizerResponse_policyDocuments :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe [Core.Text])
testInvokeAuthorizerResponse_policyDocuments = Lens.lens (\TestInvokeAuthorizerResponse' {policyDocuments} -> policyDocuments) (\s@TestInvokeAuthorizerResponse' {} a -> s {policyDocuments = a} :: TestInvokeAuthorizerResponse) Core.. Lens.mapping Lens._Coerce

-- | True if the token is authenticated, otherwise false.
testInvokeAuthorizerResponse_isAuthenticated :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Bool)
testInvokeAuthorizerResponse_isAuthenticated = Lens.lens (\TestInvokeAuthorizerResponse' {isAuthenticated} -> isAuthenticated) (\s@TestInvokeAuthorizerResponse' {} a -> s {isAuthenticated = a} :: TestInvokeAuthorizerResponse)

-- | The number of seconds after which the temporary credentials are
-- refreshed.
testInvokeAuthorizerResponse_refreshAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Core.Maybe Core.Int)
testInvokeAuthorizerResponse_refreshAfterInSeconds = Lens.lens (\TestInvokeAuthorizerResponse' {refreshAfterInSeconds} -> refreshAfterInSeconds) (\s@TestInvokeAuthorizerResponse' {} a -> s {refreshAfterInSeconds = a} :: TestInvokeAuthorizerResponse)

-- | The response's http status code.
testInvokeAuthorizerResponse_httpStatus :: Lens.Lens' TestInvokeAuthorizerResponse Core.Int
testInvokeAuthorizerResponse_httpStatus = Lens.lens (\TestInvokeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {httpStatus = a} :: TestInvokeAuthorizerResponse)

instance Core.NFData TestInvokeAuthorizerResponse
