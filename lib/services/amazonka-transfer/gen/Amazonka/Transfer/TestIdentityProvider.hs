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
-- Module      : Amazonka.Transfer.TestIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the @IdentityProviderType@ of a file transfer protocol-enabled server
-- is @AWS_DIRECTORY_SERVICE@ or @API_Gateway@, tests whether your identity
-- provider is set up successfully. We highly recommend that you call this
-- operation to test your authentication method as soon as you create your
-- server. By doing so, you can troubleshoot issues with the identity
-- provider integration to ensure that your users can successfully use the
-- service.
--
-- The @ServerId@ and @UserName@ parameters are required. The
-- @ServerProtocol@, @SourceIp@, and @UserPassword@ are all optional.
--
-- You cannot use @TestIdentityProvider@ if the @IdentityProviderType@ of
-- your server is @SERVICE_MANAGED@.
--
-- -   If you provide any incorrect values for any parameters, the
--     @Response@ field is empty.
--
-- -   If you provide a server ID for a server that uses service-managed
--     users, you get an error:
--
--     @ An error occurred (InvalidRequestException) when calling the TestIdentityProvider operation: s-@/@server-ID@/@ not configured for external auth @
--
-- -   If you enter a Server ID for the @--server-id@ parameter that does
--     not identify an actual Transfer server, you receive the following
--     error:
--
--     @An error occurred (ResourceNotFoundException) when calling the TestIdentityProvider operation: Unknown server@
module Amazonka.Transfer.TestIdentityProvider
  ( -- * Creating a Request
    TestIdentityProvider (..),
    newTestIdentityProvider,

    -- * Request Lenses
    testIdentityProvider_serverProtocol,
    testIdentityProvider_sourceIp,
    testIdentityProvider_userPassword,
    testIdentityProvider_serverId,
    testIdentityProvider_userName,

    -- * Destructuring the Response
    TestIdentityProviderResponse (..),
    newTestIdentityProviderResponse,

    -- * Response Lenses
    testIdentityProviderResponse_message,
    testIdentityProviderResponse_response,
    testIdentityProviderResponse_httpStatus,
    testIdentityProviderResponse_statusCode,
    testIdentityProviderResponse_url,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newTestIdentityProvider' smart constructor.
data TestIdentityProvider = TestIdentityProvider'
  { -- | The type of file transfer protocol to be tested.
    --
    -- The available protocols are:
    --
    -- -   Secure Shell (SSH) File Transfer Protocol (SFTP)
    --
    -- -   File Transfer Protocol Secure (FTPS)
    --
    -- -   File Transfer Protocol (FTP)
    serverProtocol :: Prelude.Maybe Protocol,
    -- | The source IP address of the user account to be tested.
    sourceIp :: Prelude.Maybe Prelude.Text,
    -- | The password of the user account to be tested.
    userPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A system-assigned identifier for a specific server. That server\'s user
    -- authentication method is tested with a user name and password.
    serverId :: Prelude.Text,
    -- | The name of the user account to be tested.
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverProtocol', 'testIdentityProvider_serverProtocol' - The type of file transfer protocol to be tested.
--
-- The available protocols are:
--
-- -   Secure Shell (SSH) File Transfer Protocol (SFTP)
--
-- -   File Transfer Protocol Secure (FTPS)
--
-- -   File Transfer Protocol (FTP)
--
-- 'sourceIp', 'testIdentityProvider_sourceIp' - The source IP address of the user account to be tested.
--
-- 'userPassword', 'testIdentityProvider_userPassword' - The password of the user account to be tested.
--
-- 'serverId', 'testIdentityProvider_serverId' - A system-assigned identifier for a specific server. That server\'s user
-- authentication method is tested with a user name and password.
--
-- 'userName', 'testIdentityProvider_userName' - The name of the user account to be tested.
newTestIdentityProvider ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  TestIdentityProvider
newTestIdentityProvider pServerId_ pUserName_ =
  TestIdentityProvider'
    { serverProtocol =
        Prelude.Nothing,
      sourceIp = Prelude.Nothing,
      userPassword = Prelude.Nothing,
      serverId = pServerId_,
      userName = pUserName_
    }

-- | The type of file transfer protocol to be tested.
--
-- The available protocols are:
--
-- -   Secure Shell (SSH) File Transfer Protocol (SFTP)
--
-- -   File Transfer Protocol Secure (FTPS)
--
-- -   File Transfer Protocol (FTP)
testIdentityProvider_serverProtocol :: Lens.Lens' TestIdentityProvider (Prelude.Maybe Protocol)
testIdentityProvider_serverProtocol = Lens.lens (\TestIdentityProvider' {serverProtocol} -> serverProtocol) (\s@TestIdentityProvider' {} a -> s {serverProtocol = a} :: TestIdentityProvider)

-- | The source IP address of the user account to be tested.
testIdentityProvider_sourceIp :: Lens.Lens' TestIdentityProvider (Prelude.Maybe Prelude.Text)
testIdentityProvider_sourceIp = Lens.lens (\TestIdentityProvider' {sourceIp} -> sourceIp) (\s@TestIdentityProvider' {} a -> s {sourceIp = a} :: TestIdentityProvider)

-- | The password of the user account to be tested.
testIdentityProvider_userPassword :: Lens.Lens' TestIdentityProvider (Prelude.Maybe Prelude.Text)
testIdentityProvider_userPassword = Lens.lens (\TestIdentityProvider' {userPassword} -> userPassword) (\s@TestIdentityProvider' {} a -> s {userPassword = a} :: TestIdentityProvider) Prelude.. Lens.mapping Data._Sensitive

-- | A system-assigned identifier for a specific server. That server\'s user
-- authentication method is tested with a user name and password.
testIdentityProvider_serverId :: Lens.Lens' TestIdentityProvider Prelude.Text
testIdentityProvider_serverId = Lens.lens (\TestIdentityProvider' {serverId} -> serverId) (\s@TestIdentityProvider' {} a -> s {serverId = a} :: TestIdentityProvider)

-- | The name of the user account to be tested.
testIdentityProvider_userName :: Lens.Lens' TestIdentityProvider Prelude.Text
testIdentityProvider_userName = Lens.lens (\TestIdentityProvider' {userName} -> userName) (\s@TestIdentityProvider' {} a -> s {userName = a} :: TestIdentityProvider)

instance Core.AWSRequest TestIdentityProvider where
  type
    AWSResponse TestIdentityProvider =
      TestIdentityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestIdentityProviderResponse'
            Prelude.<$> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "Response")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "StatusCode")
            Prelude.<*> (x Data..:> "Url")
      )

instance Prelude.Hashable TestIdentityProvider where
  hashWithSalt _salt TestIdentityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` serverProtocol
      `Prelude.hashWithSalt` sourceIp
      `Prelude.hashWithSalt` userPassword
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` userName

instance Prelude.NFData TestIdentityProvider where
  rnf TestIdentityProvider' {..} =
    Prelude.rnf serverProtocol `Prelude.seq`
      Prelude.rnf sourceIp `Prelude.seq`
        Prelude.rnf userPassword `Prelude.seq`
          Prelude.rnf serverId `Prelude.seq`
            Prelude.rnf userName

instance Data.ToHeaders TestIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.TestIdentityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TestIdentityProvider where
  toJSON TestIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServerProtocol" Data..=)
              Prelude.<$> serverProtocol,
            ("SourceIp" Data..=) Prelude.<$> sourceIp,
            ("UserPassword" Data..=) Prelude.<$> userPassword,
            Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just ("UserName" Data..= userName)
          ]
      )

instance Data.ToPath TestIdentityProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery TestIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestIdentityProviderResponse' smart constructor.
data TestIdentityProviderResponse = TestIdentityProviderResponse'
  { -- | A message that indicates whether the test was successful or not.
    --
    -- If an empty string is returned, the most likely cause is that the
    -- authentication failed due to an incorrect username or password.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response that is returned from your API Gateway.
    response :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The HTTP status code that is the response from your API Gateway.
    statusCode :: Prelude.Int,
    -- | The endpoint of the service used to authenticate a user.
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'testIdentityProviderResponse_message' - A message that indicates whether the test was successful or not.
--
-- If an empty string is returned, the most likely cause is that the
-- authentication failed due to an incorrect username or password.
--
-- 'response', 'testIdentityProviderResponse_response' - The response that is returned from your API Gateway.
--
-- 'httpStatus', 'testIdentityProviderResponse_httpStatus' - The response's http status code.
--
-- 'statusCode', 'testIdentityProviderResponse_statusCode' - The HTTP status code that is the response from your API Gateway.
--
-- 'url', 'testIdentityProviderResponse_url' - The endpoint of the service used to authenticate a user.
newTestIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'statusCode'
  Prelude.Int ->
  -- | 'url'
  Prelude.Text ->
  TestIdentityProviderResponse
newTestIdentityProviderResponse
  pHttpStatus_
  pStatusCode_
  pUrl_ =
    TestIdentityProviderResponse'
      { message =
          Prelude.Nothing,
        response = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        statusCode = pStatusCode_,
        url = pUrl_
      }

-- | A message that indicates whether the test was successful or not.
--
-- If an empty string is returned, the most likely cause is that the
-- authentication failed due to an incorrect username or password.
testIdentityProviderResponse_message :: Lens.Lens' TestIdentityProviderResponse (Prelude.Maybe Prelude.Text)
testIdentityProviderResponse_message = Lens.lens (\TestIdentityProviderResponse' {message} -> message) (\s@TestIdentityProviderResponse' {} a -> s {message = a} :: TestIdentityProviderResponse)

-- | The response that is returned from your API Gateway.
testIdentityProviderResponse_response :: Lens.Lens' TestIdentityProviderResponse (Prelude.Maybe Prelude.Text)
testIdentityProviderResponse_response = Lens.lens (\TestIdentityProviderResponse' {response} -> response) (\s@TestIdentityProviderResponse' {} a -> s {response = a} :: TestIdentityProviderResponse)

-- | The response's http status code.
testIdentityProviderResponse_httpStatus :: Lens.Lens' TestIdentityProviderResponse Prelude.Int
testIdentityProviderResponse_httpStatus = Lens.lens (\TestIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@TestIdentityProviderResponse' {} a -> s {httpStatus = a} :: TestIdentityProviderResponse)

-- | The HTTP status code that is the response from your API Gateway.
testIdentityProviderResponse_statusCode :: Lens.Lens' TestIdentityProviderResponse Prelude.Int
testIdentityProviderResponse_statusCode = Lens.lens (\TestIdentityProviderResponse' {statusCode} -> statusCode) (\s@TestIdentityProviderResponse' {} a -> s {statusCode = a} :: TestIdentityProviderResponse)

-- | The endpoint of the service used to authenticate a user.
testIdentityProviderResponse_url :: Lens.Lens' TestIdentityProviderResponse Prelude.Text
testIdentityProviderResponse_url = Lens.lens (\TestIdentityProviderResponse' {url} -> url) (\s@TestIdentityProviderResponse' {} a -> s {url = a} :: TestIdentityProviderResponse)

instance Prelude.NFData TestIdentityProviderResponse where
  rnf TestIdentityProviderResponse' {..} =
    Prelude.rnf message `Prelude.seq`
      Prelude.rnf response `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf statusCode `Prelude.seq`
            Prelude.rnf url
