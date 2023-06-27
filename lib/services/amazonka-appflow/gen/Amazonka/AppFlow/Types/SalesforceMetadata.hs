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
-- Module      : Amazonka.AppFlow.Types.SalesforceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceMetadata where

import Amazonka.AppFlow.Types.OAuth2GrantType
import Amazonka.AppFlow.Types.SalesforceDataTransferApi
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Salesforce.
--
-- /See:/ 'newSalesforceMetadata' smart constructor.
data SalesforceMetadata = SalesforceMetadata'
  { -- | The Salesforce APIs that you can have Amazon AppFlow use when your flows
    -- transfers data to or from Salesforce.
    dataTransferApis :: Prelude.Maybe [SalesforceDataTransferApi],
    -- | The desired authorization scope for the Salesforce account.
    oAuthScopes :: Prelude.Maybe [Prelude.Text],
    -- | The OAuth 2.0 grant types that Amazon AppFlow can use when it requests
    -- an access token from Salesforce. Amazon AppFlow requires an access token
    -- each time it attempts to access your Salesforce records.
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
    oauth2GrantTypesSupported :: Prelude.Maybe [OAuth2GrantType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTransferApis', 'salesforceMetadata_dataTransferApis' - The Salesforce APIs that you can have Amazon AppFlow use when your flows
-- transfers data to or from Salesforce.
--
-- 'oAuthScopes', 'salesforceMetadata_oAuthScopes' - The desired authorization scope for the Salesforce account.
--
-- 'oauth2GrantTypesSupported', 'salesforceMetadata_oauth2GrantTypesSupported' - The OAuth 2.0 grant types that Amazon AppFlow can use when it requests
-- an access token from Salesforce. Amazon AppFlow requires an access token
-- each time it attempts to access your Salesforce records.
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
newSalesforceMetadata ::
  SalesforceMetadata
newSalesforceMetadata =
  SalesforceMetadata'
    { dataTransferApis =
        Prelude.Nothing,
      oAuthScopes = Prelude.Nothing,
      oauth2GrantTypesSupported = Prelude.Nothing
    }

-- | The Salesforce APIs that you can have Amazon AppFlow use when your flows
-- transfers data to or from Salesforce.
salesforceMetadata_dataTransferApis :: Lens.Lens' SalesforceMetadata (Prelude.Maybe [SalesforceDataTransferApi])
salesforceMetadata_dataTransferApis = Lens.lens (\SalesforceMetadata' {dataTransferApis} -> dataTransferApis) (\s@SalesforceMetadata' {} a -> s {dataTransferApis = a} :: SalesforceMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The desired authorization scope for the Salesforce account.
salesforceMetadata_oAuthScopes :: Lens.Lens' SalesforceMetadata (Prelude.Maybe [Prelude.Text])
salesforceMetadata_oAuthScopes = Lens.lens (\SalesforceMetadata' {oAuthScopes} -> oAuthScopes) (\s@SalesforceMetadata' {} a -> s {oAuthScopes = a} :: SalesforceMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The OAuth 2.0 grant types that Amazon AppFlow can use when it requests
-- an access token from Salesforce. Amazon AppFlow requires an access token
-- each time it attempts to access your Salesforce records.
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
salesforceMetadata_oauth2GrantTypesSupported :: Lens.Lens' SalesforceMetadata (Prelude.Maybe [OAuth2GrantType])
salesforceMetadata_oauth2GrantTypesSupported = Lens.lens (\SalesforceMetadata' {oauth2GrantTypesSupported} -> oauth2GrantTypesSupported) (\s@SalesforceMetadata' {} a -> s {oauth2GrantTypesSupported = a} :: SalesforceMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SalesforceMetadata where
  parseJSON =
    Data.withObject
      "SalesforceMetadata"
      ( \x ->
          SalesforceMetadata'
            Prelude.<$> ( x
                            Data..:? "dataTransferApis"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "oAuthScopes" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "oauth2GrantTypesSupported"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SalesforceMetadata where
  hashWithSalt _salt SalesforceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransferApis
      `Prelude.hashWithSalt` oAuthScopes
      `Prelude.hashWithSalt` oauth2GrantTypesSupported

instance Prelude.NFData SalesforceMetadata where
  rnf SalesforceMetadata' {..} =
    Prelude.rnf dataTransferApis
      `Prelude.seq` Prelude.rnf oAuthScopes
      `Prelude.seq` Prelude.rnf oauth2GrantTypesSupported
