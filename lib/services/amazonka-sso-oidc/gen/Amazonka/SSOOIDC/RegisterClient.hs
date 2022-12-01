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
-- Module      : Amazonka.SSOOIDC.RegisterClient
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a client with IAM Identity Center. This allows clients to
-- initiate device authorization. The output should be persisted for reuse
-- through many authentication requests.
module Amazonka.SSOOIDC.RegisterClient
  ( -- * Creating a Request
    RegisterClient (..),
    newRegisterClient,

    -- * Request Lenses
    registerClient_scopes,
    registerClient_clientName,
    registerClient_clientType,

    -- * Destructuring the Response
    RegisterClientResponse (..),
    newRegisterClientResponse,

    -- * Response Lenses
    registerClientResponse_clientSecret,
    registerClientResponse_authorizationEndpoint,
    registerClientResponse_clientId,
    registerClientResponse_clientIdIssuedAt,
    registerClientResponse_clientSecretExpiresAt,
    registerClientResponse_tokenEndpoint,
    registerClientResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOOIDC.Types

-- | /See:/ 'newRegisterClient' smart constructor.
data RegisterClient = RegisterClient'
  { -- | The list of scopes that are defined by the client. Upon authorization,
    -- this list is used to restrict permissions when granting an access token.
    scopes :: Prelude.Maybe [Prelude.Text],
    -- | The friendly name of the client.
    clientName :: Prelude.Text,
    -- | The type of client. The service supports only @public@ as a client type.
    -- Anything other than public will be rejected by the service.
    clientType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scopes', 'registerClient_scopes' - The list of scopes that are defined by the client. Upon authorization,
-- this list is used to restrict permissions when granting an access token.
--
-- 'clientName', 'registerClient_clientName' - The friendly name of the client.
--
-- 'clientType', 'registerClient_clientType' - The type of client. The service supports only @public@ as a client type.
-- Anything other than public will be rejected by the service.
newRegisterClient ::
  -- | 'clientName'
  Prelude.Text ->
  -- | 'clientType'
  Prelude.Text ->
  RegisterClient
newRegisterClient pClientName_ pClientType_ =
  RegisterClient'
    { scopes = Prelude.Nothing,
      clientName = pClientName_,
      clientType = pClientType_
    }

-- | The list of scopes that are defined by the client. Upon authorization,
-- this list is used to restrict permissions when granting an access token.
registerClient_scopes :: Lens.Lens' RegisterClient (Prelude.Maybe [Prelude.Text])
registerClient_scopes = Lens.lens (\RegisterClient' {scopes} -> scopes) (\s@RegisterClient' {} a -> s {scopes = a} :: RegisterClient) Prelude.. Lens.mapping Lens.coerced

-- | The friendly name of the client.
registerClient_clientName :: Lens.Lens' RegisterClient Prelude.Text
registerClient_clientName = Lens.lens (\RegisterClient' {clientName} -> clientName) (\s@RegisterClient' {} a -> s {clientName = a} :: RegisterClient)

-- | The type of client. The service supports only @public@ as a client type.
-- Anything other than public will be rejected by the service.
registerClient_clientType :: Lens.Lens' RegisterClient Prelude.Text
registerClient_clientType = Lens.lens (\RegisterClient' {clientType} -> clientType) (\s@RegisterClient' {} a -> s {clientType = a} :: RegisterClient)

instance Core.AWSRequest RegisterClient where
  type
    AWSResponse RegisterClient =
      RegisterClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterClientResponse'
            Prelude.<$> (x Core..?> "clientSecret")
            Prelude.<*> (x Core..?> "authorizationEndpoint")
            Prelude.<*> (x Core..?> "clientId")
            Prelude.<*> (x Core..?> "clientIdIssuedAt")
            Prelude.<*> (x Core..?> "clientSecretExpiresAt")
            Prelude.<*> (x Core..?> "tokenEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterClient where
  hashWithSalt _salt RegisterClient' {..} =
    _salt `Prelude.hashWithSalt` scopes
      `Prelude.hashWithSalt` clientName
      `Prelude.hashWithSalt` clientType

instance Prelude.NFData RegisterClient where
  rnf RegisterClient' {..} =
    Prelude.rnf scopes
      `Prelude.seq` Prelude.rnf clientName
      `Prelude.seq` Prelude.rnf clientType

instance Core.ToHeaders RegisterClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterClient where
  toJSON RegisterClient' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scopes" Core..=) Prelude.<$> scopes,
            Prelude.Just ("clientName" Core..= clientName),
            Prelude.Just ("clientType" Core..= clientType)
          ]
      )

instance Core.ToPath RegisterClient where
  toPath = Prelude.const "/client/register"

instance Core.ToQuery RegisterClient where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterClientResponse' smart constructor.
data RegisterClientResponse = RegisterClientResponse'
  { -- | A secret string generated for the client. The client will use this
    -- string to get authenticated by the service in subsequent calls.
    clientSecret :: Prelude.Maybe Prelude.Text,
    -- | The endpoint where the client can request authorization.
    authorizationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier string for each client. This client uses this
    -- identifier to get authenticated by the service in subsequent calls.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the time at which the @clientId@ and @clientSecret@ were
    -- issued.
    clientIdIssuedAt :: Prelude.Maybe Prelude.Integer,
    -- | Indicates the time at which the @clientId@ and @clientSecret@ will
    -- become invalid.
    clientSecretExpiresAt :: Prelude.Maybe Prelude.Integer,
    -- | The endpoint where the client can get an access token.
    tokenEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientSecret', 'registerClientResponse_clientSecret' - A secret string generated for the client. The client will use this
-- string to get authenticated by the service in subsequent calls.
--
-- 'authorizationEndpoint', 'registerClientResponse_authorizationEndpoint' - The endpoint where the client can request authorization.
--
-- 'clientId', 'registerClientResponse_clientId' - The unique identifier string for each client. This client uses this
-- identifier to get authenticated by the service in subsequent calls.
--
-- 'clientIdIssuedAt', 'registerClientResponse_clientIdIssuedAt' - Indicates the time at which the @clientId@ and @clientSecret@ were
-- issued.
--
-- 'clientSecretExpiresAt', 'registerClientResponse_clientSecretExpiresAt' - Indicates the time at which the @clientId@ and @clientSecret@ will
-- become invalid.
--
-- 'tokenEndpoint', 'registerClientResponse_tokenEndpoint' - The endpoint where the client can get an access token.
--
-- 'httpStatus', 'registerClientResponse_httpStatus' - The response's http status code.
newRegisterClientResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterClientResponse
newRegisterClientResponse pHttpStatus_ =
  RegisterClientResponse'
    { clientSecret =
        Prelude.Nothing,
      authorizationEndpoint = Prelude.Nothing,
      clientId = Prelude.Nothing,
      clientIdIssuedAt = Prelude.Nothing,
      clientSecretExpiresAt = Prelude.Nothing,
      tokenEndpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A secret string generated for the client. The client will use this
-- string to get authenticated by the service in subsequent calls.
registerClientResponse_clientSecret :: Lens.Lens' RegisterClientResponse (Prelude.Maybe Prelude.Text)
registerClientResponse_clientSecret = Lens.lens (\RegisterClientResponse' {clientSecret} -> clientSecret) (\s@RegisterClientResponse' {} a -> s {clientSecret = a} :: RegisterClientResponse)

-- | The endpoint where the client can request authorization.
registerClientResponse_authorizationEndpoint :: Lens.Lens' RegisterClientResponse (Prelude.Maybe Prelude.Text)
registerClientResponse_authorizationEndpoint = Lens.lens (\RegisterClientResponse' {authorizationEndpoint} -> authorizationEndpoint) (\s@RegisterClientResponse' {} a -> s {authorizationEndpoint = a} :: RegisterClientResponse)

-- | The unique identifier string for each client. This client uses this
-- identifier to get authenticated by the service in subsequent calls.
registerClientResponse_clientId :: Lens.Lens' RegisterClientResponse (Prelude.Maybe Prelude.Text)
registerClientResponse_clientId = Lens.lens (\RegisterClientResponse' {clientId} -> clientId) (\s@RegisterClientResponse' {} a -> s {clientId = a} :: RegisterClientResponse)

-- | Indicates the time at which the @clientId@ and @clientSecret@ were
-- issued.
registerClientResponse_clientIdIssuedAt :: Lens.Lens' RegisterClientResponse (Prelude.Maybe Prelude.Integer)
registerClientResponse_clientIdIssuedAt = Lens.lens (\RegisterClientResponse' {clientIdIssuedAt} -> clientIdIssuedAt) (\s@RegisterClientResponse' {} a -> s {clientIdIssuedAt = a} :: RegisterClientResponse)

-- | Indicates the time at which the @clientId@ and @clientSecret@ will
-- become invalid.
registerClientResponse_clientSecretExpiresAt :: Lens.Lens' RegisterClientResponse (Prelude.Maybe Prelude.Integer)
registerClientResponse_clientSecretExpiresAt = Lens.lens (\RegisterClientResponse' {clientSecretExpiresAt} -> clientSecretExpiresAt) (\s@RegisterClientResponse' {} a -> s {clientSecretExpiresAt = a} :: RegisterClientResponse)

-- | The endpoint where the client can get an access token.
registerClientResponse_tokenEndpoint :: Lens.Lens' RegisterClientResponse (Prelude.Maybe Prelude.Text)
registerClientResponse_tokenEndpoint = Lens.lens (\RegisterClientResponse' {tokenEndpoint} -> tokenEndpoint) (\s@RegisterClientResponse' {} a -> s {tokenEndpoint = a} :: RegisterClientResponse)

-- | The response's http status code.
registerClientResponse_httpStatus :: Lens.Lens' RegisterClientResponse Prelude.Int
registerClientResponse_httpStatus = Lens.lens (\RegisterClientResponse' {httpStatus} -> httpStatus) (\s@RegisterClientResponse' {} a -> s {httpStatus = a} :: RegisterClientResponse)

instance Prelude.NFData RegisterClientResponse where
  rnf RegisterClientResponse' {..} =
    Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientIdIssuedAt
      `Prelude.seq` Prelude.rnf clientSecretExpiresAt
      `Prelude.seq` Prelude.rnf tokenEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
