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
-- Module      : Amazonka.IoT.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a custom authorization behavior by invoking a specified custom
-- authorizer. Use this to test and debug the custom authorization behavior
-- of devices that connect to the IoT device gateway.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions TestInvokeAuthorizer>
-- action.
module Amazonka.IoT.TestInvokeAuthorizer
  ( -- * Creating a Request
    TestInvokeAuthorizer (..),
    newTestInvokeAuthorizer,

    -- * Request Lenses
    testInvokeAuthorizer_mqttContext,
    testInvokeAuthorizer_httpContext,
    testInvokeAuthorizer_tlsContext,
    testInvokeAuthorizer_tokenSignature,
    testInvokeAuthorizer_token,
    testInvokeAuthorizer_authorizerName,

    -- * Destructuring the Response
    TestInvokeAuthorizerResponse (..),
    newTestInvokeAuthorizerResponse,

    -- * Response Lenses
    testInvokeAuthorizerResponse_refreshAfterInSeconds,
    testInvokeAuthorizerResponse_isAuthenticated,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_policyDocuments,
    testInvokeAuthorizerResponse_disconnectAfterInSeconds,
    testInvokeAuthorizerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { -- | Specifies a test MQTT authorization request.
    mqttContext :: Prelude.Maybe MqttContext,
    -- | Specifies a test HTTP authorization request.
    httpContext :: Prelude.Maybe HttpContext,
    -- | Specifies a test TLS authorization request.
    tlsContext :: Prelude.Maybe TlsContext,
    -- | The signature made with the token and your custom authentication
    -- service\'s private key. This value must be Base-64-encoded.
    tokenSignature :: Prelude.Maybe Prelude.Text,
    -- | The token returned by your custom authentication service.
    token :: Prelude.Maybe Prelude.Text,
    -- | The custom authorizer name.
    authorizerName :: Prelude.Text
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
-- 'mqttContext', 'testInvokeAuthorizer_mqttContext' - Specifies a test MQTT authorization request.
--
-- 'httpContext', 'testInvokeAuthorizer_httpContext' - Specifies a test HTTP authorization request.
--
-- 'tlsContext', 'testInvokeAuthorizer_tlsContext' - Specifies a test TLS authorization request.
--
-- 'tokenSignature', 'testInvokeAuthorizer_tokenSignature' - The signature made with the token and your custom authentication
-- service\'s private key. This value must be Base-64-encoded.
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
    { mqttContext =
        Prelude.Nothing,
      httpContext = Prelude.Nothing,
      tlsContext = Prelude.Nothing,
      tokenSignature = Prelude.Nothing,
      token = Prelude.Nothing,
      authorizerName = pAuthorizerName_
    }

-- | Specifies a test MQTT authorization request.
testInvokeAuthorizer_mqttContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe MqttContext)
testInvokeAuthorizer_mqttContext = Lens.lens (\TestInvokeAuthorizer' {mqttContext} -> mqttContext) (\s@TestInvokeAuthorizer' {} a -> s {mqttContext = a} :: TestInvokeAuthorizer)

-- | Specifies a test HTTP authorization request.
testInvokeAuthorizer_httpContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe HttpContext)
testInvokeAuthorizer_httpContext = Lens.lens (\TestInvokeAuthorizer' {httpContext} -> httpContext) (\s@TestInvokeAuthorizer' {} a -> s {httpContext = a} :: TestInvokeAuthorizer)

-- | Specifies a test TLS authorization request.
testInvokeAuthorizer_tlsContext :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe TlsContext)
testInvokeAuthorizer_tlsContext = Lens.lens (\TestInvokeAuthorizer' {tlsContext} -> tlsContext) (\s@TestInvokeAuthorizer' {} a -> s {tlsContext = a} :: TestInvokeAuthorizer)

-- | The signature made with the token and your custom authentication
-- service\'s private key. This value must be Base-64-encoded.
testInvokeAuthorizer_tokenSignature :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_tokenSignature = Lens.lens (\TestInvokeAuthorizer' {tokenSignature} -> tokenSignature) (\s@TestInvokeAuthorizer' {} a -> s {tokenSignature = a} :: TestInvokeAuthorizer)

-- | The token returned by your custom authentication service.
testInvokeAuthorizer_token :: Lens.Lens' TestInvokeAuthorizer (Prelude.Maybe Prelude.Text)
testInvokeAuthorizer_token = Lens.lens (\TestInvokeAuthorizer' {token} -> token) (\s@TestInvokeAuthorizer' {} a -> s {token = a} :: TestInvokeAuthorizer)

-- | The custom authorizer name.
testInvokeAuthorizer_authorizerName :: Lens.Lens' TestInvokeAuthorizer Prelude.Text
testInvokeAuthorizer_authorizerName = Lens.lens (\TestInvokeAuthorizer' {authorizerName} -> authorizerName) (\s@TestInvokeAuthorizer' {} a -> s {authorizerName = a} :: TestInvokeAuthorizer)

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
            Prelude.<$> (x Data..?> "refreshAfterInSeconds")
            Prelude.<*> (x Data..?> "isAuthenticated")
            Prelude.<*> (x Data..?> "principalId")
            Prelude.<*> ( x Data..?> "policyDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "disconnectAfterInSeconds")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestInvokeAuthorizer where
  hashWithSalt _salt TestInvokeAuthorizer' {..} =
    _salt `Prelude.hashWithSalt` mqttContext
      `Prelude.hashWithSalt` httpContext
      `Prelude.hashWithSalt` tlsContext
      `Prelude.hashWithSalt` tokenSignature
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` authorizerName

instance Prelude.NFData TestInvokeAuthorizer where
  rnf TestInvokeAuthorizer' {..} =
    Prelude.rnf mqttContext
      `Prelude.seq` Prelude.rnf httpContext
      `Prelude.seq` Prelude.rnf tlsContext
      `Prelude.seq` Prelude.rnf tokenSignature
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf authorizerName

instance Data.ToHeaders TestInvokeAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mqttContext" Data..=) Prelude.<$> mqttContext,
            ("httpContext" Data..=) Prelude.<$> httpContext,
            ("tlsContext" Data..=) Prelude.<$> tlsContext,
            ("tokenSignature" Data..=)
              Prelude.<$> tokenSignature,
            ("token" Data..=) Prelude.<$> token
          ]
      )

instance Data.ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    Prelude.mconcat
      ["/authorizer/", Data.toBS authorizerName, "/test"]

instance Data.ToQuery TestInvokeAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { -- | The number of seconds after which the temporary credentials are
    -- refreshed.
    refreshAfterInSeconds :: Prelude.Maybe Prelude.Int,
    -- | True if the token is authenticated, otherwise false.
    isAuthenticated :: Prelude.Maybe Prelude.Bool,
    -- | The principal ID.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | IAM policy documents.
    policyDocuments :: Prelude.Maybe [Prelude.Text],
    -- | The number of seconds after which the connection is terminated.
    disconnectAfterInSeconds :: Prelude.Maybe Prelude.Int,
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
-- 'refreshAfterInSeconds', 'testInvokeAuthorizerResponse_refreshAfterInSeconds' - The number of seconds after which the temporary credentials are
-- refreshed.
--
-- 'isAuthenticated', 'testInvokeAuthorizerResponse_isAuthenticated' - True if the token is authenticated, otherwise false.
--
-- 'principalId', 'testInvokeAuthorizerResponse_principalId' - The principal ID.
--
-- 'policyDocuments', 'testInvokeAuthorizerResponse_policyDocuments' - IAM policy documents.
--
-- 'disconnectAfterInSeconds', 'testInvokeAuthorizerResponse_disconnectAfterInSeconds' - The number of seconds after which the connection is terminated.
--
-- 'httpStatus', 'testInvokeAuthorizerResponse_httpStatus' - The response's http status code.
newTestInvokeAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestInvokeAuthorizerResponse
newTestInvokeAuthorizerResponse pHttpStatus_ =
  TestInvokeAuthorizerResponse'
    { refreshAfterInSeconds =
        Prelude.Nothing,
      isAuthenticated = Prelude.Nothing,
      principalId = Prelude.Nothing,
      policyDocuments = Prelude.Nothing,
      disconnectAfterInSeconds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of seconds after which the temporary credentials are
-- refreshed.
testInvokeAuthorizerResponse_refreshAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Int)
testInvokeAuthorizerResponse_refreshAfterInSeconds = Lens.lens (\TestInvokeAuthorizerResponse' {refreshAfterInSeconds} -> refreshAfterInSeconds) (\s@TestInvokeAuthorizerResponse' {} a -> s {refreshAfterInSeconds = a} :: TestInvokeAuthorizerResponse)

-- | True if the token is authenticated, otherwise false.
testInvokeAuthorizerResponse_isAuthenticated :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Bool)
testInvokeAuthorizerResponse_isAuthenticated = Lens.lens (\TestInvokeAuthorizerResponse' {isAuthenticated} -> isAuthenticated) (\s@TestInvokeAuthorizerResponse' {} a -> s {isAuthenticated = a} :: TestInvokeAuthorizerResponse)

-- | The principal ID.
testInvokeAuthorizerResponse_principalId :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Text)
testInvokeAuthorizerResponse_principalId = Lens.lens (\TestInvokeAuthorizerResponse' {principalId} -> principalId) (\s@TestInvokeAuthorizerResponse' {} a -> s {principalId = a} :: TestInvokeAuthorizerResponse)

-- | IAM policy documents.
testInvokeAuthorizerResponse_policyDocuments :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe [Prelude.Text])
testInvokeAuthorizerResponse_policyDocuments = Lens.lens (\TestInvokeAuthorizerResponse' {policyDocuments} -> policyDocuments) (\s@TestInvokeAuthorizerResponse' {} a -> s {policyDocuments = a} :: TestInvokeAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds after which the connection is terminated.
testInvokeAuthorizerResponse_disconnectAfterInSeconds :: Lens.Lens' TestInvokeAuthorizerResponse (Prelude.Maybe Prelude.Int)
testInvokeAuthorizerResponse_disconnectAfterInSeconds = Lens.lens (\TestInvokeAuthorizerResponse' {disconnectAfterInSeconds} -> disconnectAfterInSeconds) (\s@TestInvokeAuthorizerResponse' {} a -> s {disconnectAfterInSeconds = a} :: TestInvokeAuthorizerResponse)

-- | The response's http status code.
testInvokeAuthorizerResponse_httpStatus :: Lens.Lens' TestInvokeAuthorizerResponse Prelude.Int
testInvokeAuthorizerResponse_httpStatus = Lens.lens (\TestInvokeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@TestInvokeAuthorizerResponse' {} a -> s {httpStatus = a} :: TestInvokeAuthorizerResponse)

instance Prelude.NFData TestInvokeAuthorizerResponse where
  rnf TestInvokeAuthorizerResponse' {..} =
    Prelude.rnf refreshAfterInSeconds
      `Prelude.seq` Prelude.rnf isAuthenticated
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf policyDocuments
      `Prelude.seq` Prelude.rnf disconnectAfterInSeconds
      `Prelude.seq` Prelude.rnf httpStatus
