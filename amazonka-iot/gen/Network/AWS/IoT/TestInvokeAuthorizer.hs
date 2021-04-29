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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | Specifies a test HTTP authorization request.
    httpContext :: Prelude.Maybe HttpContext,
    -- | Specifies a test MQTT authorization request.
    mqttContext :: Prelude.Maybe MqttContext,
    -- | The signature made with the token and your custom authentication
    -- service\'s private key. This value must be Base-64-encoded.
    tokenSignature :: Prelude.Maybe Prelude.Text,
    -- | Specifies a test TLS authorization request.
    tlsContext :: Prelude.Maybe TlsContext,
    -- | The token returned by your custom authentication service.
    token :: Prelude.Maybe Prelude.Text,
    -- | The custom authorizer name.
    authorizerName :: Prelude.Text
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
  Prelude.Text ->
  TestInvokeAuthorizer
newTestInvokeAuthorizer pAuthorizerName_ =
  TestInvokeAuthorizer'
    { httpContext =
        Prelude.Nothing,
      mqttContext = Prelude.Nothing,
      tokenSignature = Prelude.Nothing,
      tlsContext = Prelude.Nothing,
      token = Prelude.Nothing,
      authorizerName = pAuthorizerName_
    }

-- | Specifies a test HTTP authorization request.
testInvokeAuthorizer_httpContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe HttpContext)
testInvokeAuthorizer_httpContext = Lens.lens (\TestInvokeAuthorizer' {httpContext} -> httpContext) (\s@TestInvokeAuthorizer' {} a -> s {httpContext = a} :: TestInvokeAuthorizer)

-- | Specifies a test MQTT authorization request.
testInvokeAuthorizer_mqttContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe MqttContext)
testInvokeAuthorizer_mqttContext = Lens.lens (\TestInvokeAuthorizer' {mqttContext} -> mqttContext) (\s@TestInvokeAuthorizer' {} a -> s {mqttContext = a} :: TestInvokeAuthorizer)

-- | The signature made with the token and your custom authentication
-- service\'s private key. This value must be Base-64-encoded.
testInvokeAuthorizer_tokenSignature :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_tokenSignature = Lens.lens (\TestInvokeAuthorizer' {tokenSignature} -> tokenSignature) (\s@TestInvokeAuthorizer' {} a -> s {tokenSignature = a} :: TestInvokeAuthorizer)

-- | Specifies a test TLS authorization request.
testInvokeAuthorizer_tlsContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe TlsContext)
testInvokeAuthorizer_tlsContext = Lens.lens (\TestInvokeAuthorizer' {tlsContext} -> tlsContext) (\s@TestInvokeAuthorizer' {} a -> s {tlsContext = a} :: TestInvokeAuthorizer)

-- | The token returned by your custom authentication service.
testInvokeAuthorizer_token :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_token = Lens.lens (\TestInvokeAuthorizer' {token} -> token) (\s@TestInvokeAuthorizer' {} a -> s {token = a} :: TestInvokeAuthorizer)

-- | The custom authorizer name.
testInvokeAuthorizer_authorizerName :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_authorizerName = Lens.lens (\TestInvokeAuthorizer' {authorizerName} -> authorizerName) (\s@TestInvokeAuthorizer' {} a -> s {authorizerName = a} :: TestInvokeAuthorizer)

instance Prelude.AWSRequest TestInvokeAuthorizer where
  type
    Rs TestInvokeAuthorizer =
      TestInvokeAuthorizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            Prelude.<$> (x Prelude..?> "disconnectAfterInSeconds")
            Prelude.<*> (x Prelude..?> "principalId")
            Prelude.<*> ( x Prelude..?> "policyDocuments"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "isAuthenticated")
            Prelude.<*> (x Prelude..?> "refreshAfterInSeconds")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestInvokeAuthorizer

instance Prelude.NFData TestInvokeAuthorizer

instance Prelude.ToHeaders TestInvokeAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("httpContext" Prelude..=) Prelude.<$> httpContext,
            ("mqttContext" Prelude..=) Prelude.<$> mqttContext,
            ("tokenSignature" Prelude..=)
              Prelude.<$> tokenSignature,
            ("tlsContext" Prelude..=) Prelude.<$> tlsContext,
            ("token" Prelude..=) Prelude.<$> token
          ]
      )

instance Prelude.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Prelude.mconcat
      [ "/authorizer/",
        Prelude.toBS authorizerName,
        "/test"
      ]

instance Prelude.ToQuery TestInvokeAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The number of seconds after which the connection is terminated.
    disconnectAfterInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The principal ID.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | IAM policy documents.
    policyDocuments :: Prelude.Maybe [Prelude.Text],
    -- | True if the token is authenticated, otherwise false.
    isAuthenticated :: Prelude.Maybe Prelude.Bool,
    -- | The number of seconds after which the temporary credentials are
    -- refreshed.
    refreshAfterInSeconds :: Prelude.Maybe Prelude.Int,
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
  Prelude.Int ->
  TestInvokeAuthorizerResponse
newTestInvokeAuthorizerResponse pHttpStatus_ =
  TestInvokeAuthorizerResponse'
    { disconnectAfterInSeconds =
        Prelude.Nothing,
      principalId = Prelude.Nothing,
      policyDocuments = Prelude.Nothing,
      isAuthenticated = Prelude.Nothing,
      refreshAfterInSeconds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of seconds after which the connection is terminated.
testInvokeAuthorizerResponse_disconnectAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Int)
testInvokeAuthorizerResponse_disconnectAfterInSeconds = Lens.lens (\TestInvokeAuthorizerResponse' {disconnectAfterInSeconds} -> disconnectAfterInSeconds) (\s@TestInvokeAuthorizerResponse' {} a -> s {disconnectAfterInSeconds = a} :: TestInvokeAuthorizerResponse)

-- | The principal ID.
testInvokeAuthorizerResponse_principalId :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_principalId = Lens.lens (\TestInvokeAuthorizerResponse' {principalId} -> principalId) (\s@TestInvokeAuthorizerResponse' {} a -> s {principalId = a} :: TestInvokeAuthorizerResponse)

-- | IAM policy documents.
testInvokeAuthorizerResponse_policyDocuments :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe [Prelude.Text])
testInvokeAuthorizerResponse_policyDocuments = Lens.lens (\TestInvokeAuthorizerResponse' {policyDocuments} -> policyDocuments) (\s@TestInvokeAuthorizerResponse' {} a -> s {policyDocuments = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | True if the token is authenticated, otherwise false.
testInvokeAuthorizerResponse_isAuthenticated :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Bool)
testInvokeAuthorizerResponse_isAuthenticated = Lens.lens (\TestInvokeAuthorizerResponse' {isAuthenticated} -> isAuthenticated) (\s@TestInvokeAuthorizerResponse' {} a -> s {isAuthenticated = a} :: TestInvokeAuthorizerResponse)

-- | The number of seconds after which the temporary credentials are
-- refreshed.
testInvokeAuthorizerResponse_refreshAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Int)
testInvokeAuthorizerResponse_refreshAfterInSeconds = Lens.lens (\TestInvokeAuthorizerResponse' {refreshAfterInSeconds} -> refreshAfterInSeconds) (\s@TestInvokeAuthorizerResponse' {} a -> s {refreshAfterInSeconds = a} :: TestInvokeAuthorizerResponse)

-- | The response's http status code.
testInvokeAuthorizerResponse_httpStatus :: Lens.Lens' TestInvokeAuthorizerResponse Prelude.Int
testInvokeAuthorizerResponse_httpStatus = Lens.lens (\TestInvokeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {httpStatus = a} :: TestInvokeAuthorizerResponse)

instance Prelude.NFData TestInvokeAuthorizerResponse
