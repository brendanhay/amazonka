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
-- Module      : Amazonka.Grafana.Types.AuthenticationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.AuthenticationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types.AuthenticationProviderTypes
import Amazonka.Grafana.Types.AwsSsoAuthentication
import Amazonka.Grafana.Types.SamlAuthentication
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information about the user authentication methods
-- used by the workspace.
--
-- /See:/ 'newAuthenticationDescription' smart constructor.
data AuthenticationDescription = AuthenticationDescription'
  { -- | A structure containing information about how this workspace works with
    -- IAM Identity Center.
    awsSso :: Prelude.Maybe AwsSsoAuthentication,
    -- | A structure containing information about how this workspace works with
    -- SAML, including what attributes within the assertion are to be mapped to
    -- user information in the workspace.
    saml :: Prelude.Maybe SamlAuthentication,
    -- | Specifies whether this workspace uses IAM Identity Center, SAML, or both
    -- methods to authenticate users to use the Grafana console in the Amazon
    -- Managed Grafana workspace.
    providers :: [AuthenticationProviderTypes]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsSso', 'authenticationDescription_awsSso' - A structure containing information about how this workspace works with
-- IAM Identity Center.
--
-- 'saml', 'authenticationDescription_saml' - A structure containing information about how this workspace works with
-- SAML, including what attributes within the assertion are to be mapped to
-- user information in the workspace.
--
-- 'providers', 'authenticationDescription_providers' - Specifies whether this workspace uses IAM Identity Center, SAML, or both
-- methods to authenticate users to use the Grafana console in the Amazon
-- Managed Grafana workspace.
newAuthenticationDescription ::
  AuthenticationDescription
newAuthenticationDescription =
  AuthenticationDescription'
    { awsSso =
        Prelude.Nothing,
      saml = Prelude.Nothing,
      providers = Prelude.mempty
    }

-- | A structure containing information about how this workspace works with
-- IAM Identity Center.
authenticationDescription_awsSso :: Lens.Lens' AuthenticationDescription (Prelude.Maybe AwsSsoAuthentication)
authenticationDescription_awsSso = Lens.lens (\AuthenticationDescription' {awsSso} -> awsSso) (\s@AuthenticationDescription' {} a -> s {awsSso = a} :: AuthenticationDescription)

-- | A structure containing information about how this workspace works with
-- SAML, including what attributes within the assertion are to be mapped to
-- user information in the workspace.
authenticationDescription_saml :: Lens.Lens' AuthenticationDescription (Prelude.Maybe SamlAuthentication)
authenticationDescription_saml = Lens.lens (\AuthenticationDescription' {saml} -> saml) (\s@AuthenticationDescription' {} a -> s {saml = a} :: AuthenticationDescription)

-- | Specifies whether this workspace uses IAM Identity Center, SAML, or both
-- methods to authenticate users to use the Grafana console in the Amazon
-- Managed Grafana workspace.
authenticationDescription_providers :: Lens.Lens' AuthenticationDescription [AuthenticationProviderTypes]
authenticationDescription_providers = Lens.lens (\AuthenticationDescription' {providers} -> providers) (\s@AuthenticationDescription' {} a -> s {providers = a} :: AuthenticationDescription) Prelude.. Lens.coerced

instance Data.FromJSON AuthenticationDescription where
  parseJSON =
    Data.withObject
      "AuthenticationDescription"
      ( \x ->
          AuthenticationDescription'
            Prelude.<$> (x Data..:? "awsSso")
            Prelude.<*> (x Data..:? "saml")
            Prelude.<*> (x Data..:? "providers" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AuthenticationDescription where
  hashWithSalt _salt AuthenticationDescription' {..} =
    _salt `Prelude.hashWithSalt` awsSso
      `Prelude.hashWithSalt` saml
      `Prelude.hashWithSalt` providers

instance Prelude.NFData AuthenticationDescription where
  rnf AuthenticationDescription' {..} =
    Prelude.rnf awsSso
      `Prelude.seq` Prelude.rnf saml
      `Prelude.seq` Prelude.rnf providers
