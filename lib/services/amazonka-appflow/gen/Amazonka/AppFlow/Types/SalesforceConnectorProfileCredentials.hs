{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.Types.SalesforceConnectorProfileCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceConnectorProfileCredentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import Amazonka.AppFlow.Types.OAuth2GrantType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using
-- Salesforce.
--
-- /See:/ 'newSalesforceConnectorProfileCredentials' smart constructor.
data SalesforceConnectorProfileCredentials = SalesforceConnectorProfileCredentials'
  { -- | The credentials used to access protected Salesforce resources.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The secret manager ARN, which contains the client ID and client secret
    -- of the connected app.
    clientCredentialsArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A JSON web token (JWT) that authorizes Amazon AppFlow to access your
    -- Salesforce records.
    jwtToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the OAuth 2.0 grant type that Amazon AppFlow uses when it
    -- requests an access token from Salesforce. Amazon AppFlow requires an
    -- access token each time it attempts to access your Salesforce records.
    --
    -- You can specify one of the following values:
    --
    -- [AUTHORIZATION_CODE]
    --     Amazon AppFlow passes an authorization code when it requests the
    --     access token from Salesforce. Amazon AppFlow receives the
    --     authorization code from Salesforce after you log in to your
    --     Salesforce account and authorize Amazon AppFlow to access your
    --     records.
    --
    -- [CLIENT_CREDENTIALS]
    --     Amazon AppFlow passes client credentials (a client ID and client
    --     secret) when it requests the access token from Salesforce. You
    --     provide these credentials to Amazon AppFlow when you define the
    --     connection to your Salesforce account.
    --
    -- [JWT_BEARER]
    --     Amazon AppFlow passes a JSON web token (JWT) when it requests the
    --     access token from Salesforce. You provide the JWT to Amazon AppFlow
    --     when you define the connection to your Salesforce account. When you
    --     use this grant type, you don\'t need to log in to your Salesforce
    --     account to authorize Amazon AppFlow to access your records.
    oAuth2GrantType :: Prelude.Maybe OAuth2GrantType,
    -- | The OAuth requirement needed to request security tokens from the
    -- connector endpoint.
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The credentials used to acquire new access tokens.
    refreshToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'salesforceConnectorProfileCredentials_accessToken' - The credentials used to access protected Salesforce resources.
--
-- 'clientCredentialsArn', 'salesforceConnectorProfileCredentials_clientCredentialsArn' - The secret manager ARN, which contains the client ID and client secret
-- of the connected app.
--
-- 'jwtToken', 'salesforceConnectorProfileCredentials_jwtToken' - A JSON web token (JWT) that authorizes Amazon AppFlow to access your
-- Salesforce records.
--
-- 'oAuth2GrantType', 'salesforceConnectorProfileCredentials_oAuth2GrantType' - Specifies the OAuth 2.0 grant type that Amazon AppFlow uses when it
-- requests an access token from Salesforce. Amazon AppFlow requires an
-- access token each time it attempts to access your Salesforce records.
--
-- You can specify one of the following values:
--
-- [AUTHORIZATION_CODE]
--     Amazon AppFlow passes an authorization code when it requests the
--     access token from Salesforce. Amazon AppFlow receives the
--     authorization code from Salesforce after you log in to your
--     Salesforce account and authorize Amazon AppFlow to access your
--     records.
--
-- [CLIENT_CREDENTIALS]
--     Amazon AppFlow passes client credentials (a client ID and client
--     secret) when it requests the access token from Salesforce. You
--     provide these credentials to Amazon AppFlow when you define the
--     connection to your Salesforce account.
--
-- [JWT_BEARER]
--     Amazon AppFlow passes a JSON web token (JWT) when it requests the
--     access token from Salesforce. You provide the JWT to Amazon AppFlow
--     when you define the connection to your Salesforce account. When you
--     use this grant type, you don\'t need to log in to your Salesforce
--     account to authorize Amazon AppFlow to access your records.
--
-- 'oAuthRequest', 'salesforceConnectorProfileCredentials_oAuthRequest' - The OAuth requirement needed to request security tokens from the
-- connector endpoint.
--
-- 'refreshToken', 'salesforceConnectorProfileCredentials_refreshToken' - The credentials used to acquire new access tokens.
newSalesforceConnectorProfileCredentials ::
  SalesforceConnectorProfileCredentials
newSalesforceConnectorProfileCredentials =
  SalesforceConnectorProfileCredentials'
    { accessToken =
        Prelude.Nothing,
      clientCredentialsArn =
        Prelude.Nothing,
      jwtToken = Prelude.Nothing,
      oAuth2GrantType = Prelude.Nothing,
      oAuthRequest = Prelude.Nothing,
      refreshToken = Prelude.Nothing
    }

-- | The credentials used to access protected Salesforce resources.
salesforceConnectorProfileCredentials_accessToken :: Lens.Lens' SalesforceConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
salesforceConnectorProfileCredentials_accessToken = Lens.lens (\SalesforceConnectorProfileCredentials' {accessToken} -> accessToken) (\s@SalesforceConnectorProfileCredentials' {} a -> s {accessToken = a} :: SalesforceConnectorProfileCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | The secret manager ARN, which contains the client ID and client secret
-- of the connected app.
salesforceConnectorProfileCredentials_clientCredentialsArn :: Lens.Lens' SalesforceConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
salesforceConnectorProfileCredentials_clientCredentialsArn = Lens.lens (\SalesforceConnectorProfileCredentials' {clientCredentialsArn} -> clientCredentialsArn) (\s@SalesforceConnectorProfileCredentials' {} a -> s {clientCredentialsArn = a} :: SalesforceConnectorProfileCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | A JSON web token (JWT) that authorizes Amazon AppFlow to access your
-- Salesforce records.
salesforceConnectorProfileCredentials_jwtToken :: Lens.Lens' SalesforceConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
salesforceConnectorProfileCredentials_jwtToken = Lens.lens (\SalesforceConnectorProfileCredentials' {jwtToken} -> jwtToken) (\s@SalesforceConnectorProfileCredentials' {} a -> s {jwtToken = a} :: SalesforceConnectorProfileCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the OAuth 2.0 grant type that Amazon AppFlow uses when it
-- requests an access token from Salesforce. Amazon AppFlow requires an
-- access token each time it attempts to access your Salesforce records.
--
-- You can specify one of the following values:
--
-- [AUTHORIZATION_CODE]
--     Amazon AppFlow passes an authorization code when it requests the
--     access token from Salesforce. Amazon AppFlow receives the
--     authorization code from Salesforce after you log in to your
--     Salesforce account and authorize Amazon AppFlow to access your
--     records.
--
-- [CLIENT_CREDENTIALS]
--     Amazon AppFlow passes client credentials (a client ID and client
--     secret) when it requests the access token from Salesforce. You
--     provide these credentials to Amazon AppFlow when you define the
--     connection to your Salesforce account.
--
-- [JWT_BEARER]
--     Amazon AppFlow passes a JSON web token (JWT) when it requests the
--     access token from Salesforce. You provide the JWT to Amazon AppFlow
--     when you define the connection to your Salesforce account. When you
--     use this grant type, you don\'t need to log in to your Salesforce
--     account to authorize Amazon AppFlow to access your records.
salesforceConnectorProfileCredentials_oAuth2GrantType :: Lens.Lens' SalesforceConnectorProfileCredentials (Prelude.Maybe OAuth2GrantType)
salesforceConnectorProfileCredentials_oAuth2GrantType = Lens.lens (\SalesforceConnectorProfileCredentials' {oAuth2GrantType} -> oAuth2GrantType) (\s@SalesforceConnectorProfileCredentials' {} a -> s {oAuth2GrantType = a} :: SalesforceConnectorProfileCredentials)

-- | The OAuth requirement needed to request security tokens from the
-- connector endpoint.
salesforceConnectorProfileCredentials_oAuthRequest :: Lens.Lens' SalesforceConnectorProfileCredentials (Prelude.Maybe ConnectorOAuthRequest)
salesforceConnectorProfileCredentials_oAuthRequest = Lens.lens (\SalesforceConnectorProfileCredentials' {oAuthRequest} -> oAuthRequest) (\s@SalesforceConnectorProfileCredentials' {} a -> s {oAuthRequest = a} :: SalesforceConnectorProfileCredentials)

-- | The credentials used to acquire new access tokens.
salesforceConnectorProfileCredentials_refreshToken :: Lens.Lens' SalesforceConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
salesforceConnectorProfileCredentials_refreshToken = Lens.lens (\SalesforceConnectorProfileCredentials' {refreshToken} -> refreshToken) (\s@SalesforceConnectorProfileCredentials' {} a -> s {refreshToken = a} :: SalesforceConnectorProfileCredentials)

instance
  Prelude.Hashable
    SalesforceConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    SalesforceConnectorProfileCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` accessToken
        `Prelude.hashWithSalt` clientCredentialsArn
        `Prelude.hashWithSalt` jwtToken
        `Prelude.hashWithSalt` oAuth2GrantType
        `Prelude.hashWithSalt` oAuthRequest
        `Prelude.hashWithSalt` refreshToken

instance
  Prelude.NFData
    SalesforceConnectorProfileCredentials
  where
  rnf SalesforceConnectorProfileCredentials' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf clientCredentialsArn
      `Prelude.seq` Prelude.rnf jwtToken
      `Prelude.seq` Prelude.rnf oAuth2GrantType
      `Prelude.seq` Prelude.rnf oAuthRequest
      `Prelude.seq` Prelude.rnf refreshToken

instance
  Data.ToJSON
    SalesforceConnectorProfileCredentials
  where
  toJSON SalesforceConnectorProfileCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessToken" Data..=) Prelude.<$> accessToken,
            ("clientCredentialsArn" Data..=)
              Prelude.<$> clientCredentialsArn,
            ("jwtToken" Data..=) Prelude.<$> jwtToken,
            ("oAuth2GrantType" Data..=)
              Prelude.<$> oAuth2GrantType,
            ("oAuthRequest" Data..=) Prelude.<$> oAuthRequest,
            ("refreshToken" Data..=) Prelude.<$> refreshToken
          ]
      )
