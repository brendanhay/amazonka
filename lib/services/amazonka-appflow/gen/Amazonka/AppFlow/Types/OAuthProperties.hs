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
-- Module      : Amazonka.AppFlow.Types.OAuthProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OAuthProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The OAuth properties required for OAuth type authentication.
--
-- /See:/ 'newOAuthProperties' smart constructor.
data OAuthProperties = OAuthProperties'
  { -- | The token url required to fetch access\/refresh tokens using
    -- authorization code and also to refresh expired access token using
    -- refresh token.
    tokenUrl :: Prelude.Text,
    -- | The authorization code url required to redirect to SAP Login Page to
    -- fetch authorization code for OAuth type authentication.
    authCodeUrl :: Prelude.Text,
    -- | The OAuth scopes required for OAuth type authentication.
    oAuthScopes :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OAuthProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenUrl', 'oAuthProperties_tokenUrl' - The token url required to fetch access\/refresh tokens using
-- authorization code and also to refresh expired access token using
-- refresh token.
--
-- 'authCodeUrl', 'oAuthProperties_authCodeUrl' - The authorization code url required to redirect to SAP Login Page to
-- fetch authorization code for OAuth type authentication.
--
-- 'oAuthScopes', 'oAuthProperties_oAuthScopes' - The OAuth scopes required for OAuth type authentication.
newOAuthProperties ::
  -- | 'tokenUrl'
  Prelude.Text ->
  -- | 'authCodeUrl'
  Prelude.Text ->
  OAuthProperties
newOAuthProperties pTokenUrl_ pAuthCodeUrl_ =
  OAuthProperties'
    { tokenUrl = pTokenUrl_,
      authCodeUrl = pAuthCodeUrl_,
      oAuthScopes = Prelude.mempty
    }

-- | The token url required to fetch access\/refresh tokens using
-- authorization code and also to refresh expired access token using
-- refresh token.
oAuthProperties_tokenUrl :: Lens.Lens' OAuthProperties Prelude.Text
oAuthProperties_tokenUrl = Lens.lens (\OAuthProperties' {tokenUrl} -> tokenUrl) (\s@OAuthProperties' {} a -> s {tokenUrl = a} :: OAuthProperties)

-- | The authorization code url required to redirect to SAP Login Page to
-- fetch authorization code for OAuth type authentication.
oAuthProperties_authCodeUrl :: Lens.Lens' OAuthProperties Prelude.Text
oAuthProperties_authCodeUrl = Lens.lens (\OAuthProperties' {authCodeUrl} -> authCodeUrl) (\s@OAuthProperties' {} a -> s {authCodeUrl = a} :: OAuthProperties)

-- | The OAuth scopes required for OAuth type authentication.
oAuthProperties_oAuthScopes :: Lens.Lens' OAuthProperties [Prelude.Text]
oAuthProperties_oAuthScopes = Lens.lens (\OAuthProperties' {oAuthScopes} -> oAuthScopes) (\s@OAuthProperties' {} a -> s {oAuthScopes = a} :: OAuthProperties) Prelude.. Lens.coerced

instance Data.FromJSON OAuthProperties where
  parseJSON =
    Data.withObject
      "OAuthProperties"
      ( \x ->
          OAuthProperties'
            Prelude.<$> (x Data..: "tokenUrl")
            Prelude.<*> (x Data..: "authCodeUrl")
            Prelude.<*> (x Data..:? "oAuthScopes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OAuthProperties where
  hashWithSalt _salt OAuthProperties' {..} =
    _salt
      `Prelude.hashWithSalt` tokenUrl
      `Prelude.hashWithSalt` authCodeUrl
      `Prelude.hashWithSalt` oAuthScopes

instance Prelude.NFData OAuthProperties where
  rnf OAuthProperties' {..} =
    Prelude.rnf tokenUrl
      `Prelude.seq` Prelude.rnf authCodeUrl
      `Prelude.seq` Prelude.rnf oAuthScopes

instance Data.ToJSON OAuthProperties where
  toJSON OAuthProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tokenUrl" Data..= tokenUrl),
            Prelude.Just ("authCodeUrl" Data..= authCodeUrl),
            Prelude.Just ("oAuthScopes" Data..= oAuthScopes)
          ]
      )
