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
-- Module      : Amazonka.Grafana.Types.AuthenticationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.AuthenticationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types.AuthenticationProviderTypes
import Amazonka.Grafana.Types.SamlConfigurationStatus
import qualified Amazonka.Prelude as Prelude

-- | A structure that describes whether the workspace uses SAML, IAM Identity
-- Center, or both methods for user authentication, and whether that
-- authentication is fully configured.
--
-- /See:/ 'newAuthenticationSummary' smart constructor.
data AuthenticationSummary = AuthenticationSummary'
  { -- | Specifies whether the workplace\'s user authentication method is fully
    -- configured.
    samlConfigurationStatus :: Prelude.Maybe SamlConfigurationStatus,
    -- | Specifies whether the workspace uses SAML, IAM Identity Center, or both
    -- methods for user authentication.
    providers :: [AuthenticationProviderTypes]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samlConfigurationStatus', 'authenticationSummary_samlConfigurationStatus' - Specifies whether the workplace\'s user authentication method is fully
-- configured.
--
-- 'providers', 'authenticationSummary_providers' - Specifies whether the workspace uses SAML, IAM Identity Center, or both
-- methods for user authentication.
newAuthenticationSummary ::
  AuthenticationSummary
newAuthenticationSummary =
  AuthenticationSummary'
    { samlConfigurationStatus =
        Prelude.Nothing,
      providers = Prelude.mempty
    }

-- | Specifies whether the workplace\'s user authentication method is fully
-- configured.
authenticationSummary_samlConfigurationStatus :: Lens.Lens' AuthenticationSummary (Prelude.Maybe SamlConfigurationStatus)
authenticationSummary_samlConfigurationStatus = Lens.lens (\AuthenticationSummary' {samlConfigurationStatus} -> samlConfigurationStatus) (\s@AuthenticationSummary' {} a -> s {samlConfigurationStatus = a} :: AuthenticationSummary)

-- | Specifies whether the workspace uses SAML, IAM Identity Center, or both
-- methods for user authentication.
authenticationSummary_providers :: Lens.Lens' AuthenticationSummary [AuthenticationProviderTypes]
authenticationSummary_providers = Lens.lens (\AuthenticationSummary' {providers} -> providers) (\s@AuthenticationSummary' {} a -> s {providers = a} :: AuthenticationSummary) Prelude.. Lens.coerced

instance Data.FromJSON AuthenticationSummary where
  parseJSON =
    Data.withObject
      "AuthenticationSummary"
      ( \x ->
          AuthenticationSummary'
            Prelude.<$> (x Data..:? "samlConfigurationStatus")
            Prelude.<*> (x Data..:? "providers" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AuthenticationSummary where
  hashWithSalt _salt AuthenticationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` samlConfigurationStatus
      `Prelude.hashWithSalt` providers

instance Prelude.NFData AuthenticationSummary where
  rnf AuthenticationSummary' {..} =
    Prelude.rnf samlConfigurationStatus `Prelude.seq`
      Prelude.rnf providers
