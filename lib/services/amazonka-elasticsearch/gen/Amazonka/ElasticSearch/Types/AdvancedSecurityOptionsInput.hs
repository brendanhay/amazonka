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
-- Module      : Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.MasterUserOptions
import Amazonka.ElasticSearch.Types.SAMLOptionsInput
import qualified Amazonka.Prelude as Prelude

-- | Specifies the advanced security configuration: whether advanced security
-- is enabled, whether the internal database option is enabled, master
-- username and password (if internal database is enabled), and master user
-- ARN (if IAM is enabled).
--
-- /See:/ 'newAdvancedSecurityOptionsInput' smart constructor.
data AdvancedSecurityOptionsInput = AdvancedSecurityOptionsInput'
  { -- | True if the internal user database is enabled.
    internalUserDatabaseEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the SAML application configuration for the domain.
    sAMLOptions :: Prelude.Maybe SAMLOptionsInput,
    -- | True if Anonymous auth is enabled. Anonymous auth can be enabled only
    -- when AdvancedSecurity is enabled on existing domains.
    anonymousAuthEnabled :: Prelude.Maybe Prelude.Bool,
    -- | True if advanced security is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Credentials for the master user: username and password, ARN, or both.
    masterUserOptions :: Prelude.Maybe MasterUserOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedSecurityOptionsInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'internalUserDatabaseEnabled', 'advancedSecurityOptionsInput_internalUserDatabaseEnabled' - True if the internal user database is enabled.
--
-- 'sAMLOptions', 'advancedSecurityOptionsInput_sAMLOptions' - Specifies the SAML application configuration for the domain.
--
-- 'anonymousAuthEnabled', 'advancedSecurityOptionsInput_anonymousAuthEnabled' - True if Anonymous auth is enabled. Anonymous auth can be enabled only
-- when AdvancedSecurity is enabled on existing domains.
--
-- 'enabled', 'advancedSecurityOptionsInput_enabled' - True if advanced security is enabled.
--
-- 'masterUserOptions', 'advancedSecurityOptionsInput_masterUserOptions' - Credentials for the master user: username and password, ARN, or both.
newAdvancedSecurityOptionsInput ::
  AdvancedSecurityOptionsInput
newAdvancedSecurityOptionsInput =
  AdvancedSecurityOptionsInput'
    { internalUserDatabaseEnabled =
        Prelude.Nothing,
      sAMLOptions = Prelude.Nothing,
      anonymousAuthEnabled = Prelude.Nothing,
      enabled = Prelude.Nothing,
      masterUserOptions = Prelude.Nothing
    }

-- | True if the internal user database is enabled.
advancedSecurityOptionsInput_internalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe Prelude.Bool)
advancedSecurityOptionsInput_internalUserDatabaseEnabled = Lens.lens (\AdvancedSecurityOptionsInput' {internalUserDatabaseEnabled} -> internalUserDatabaseEnabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {internalUserDatabaseEnabled = a} :: AdvancedSecurityOptionsInput)

-- | Specifies the SAML application configuration for the domain.
advancedSecurityOptionsInput_sAMLOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe SAMLOptionsInput)
advancedSecurityOptionsInput_sAMLOptions = Lens.lens (\AdvancedSecurityOptionsInput' {sAMLOptions} -> sAMLOptions) (\s@AdvancedSecurityOptionsInput' {} a -> s {sAMLOptions = a} :: AdvancedSecurityOptionsInput)

-- | True if Anonymous auth is enabled. Anonymous auth can be enabled only
-- when AdvancedSecurity is enabled on existing domains.
advancedSecurityOptionsInput_anonymousAuthEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe Prelude.Bool)
advancedSecurityOptionsInput_anonymousAuthEnabled = Lens.lens (\AdvancedSecurityOptionsInput' {anonymousAuthEnabled} -> anonymousAuthEnabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {anonymousAuthEnabled = a} :: AdvancedSecurityOptionsInput)

-- | True if advanced security is enabled.
advancedSecurityOptionsInput_enabled :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe Prelude.Bool)
advancedSecurityOptionsInput_enabled = Lens.lens (\AdvancedSecurityOptionsInput' {enabled} -> enabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {enabled = a} :: AdvancedSecurityOptionsInput)

-- | Credentials for the master user: username and password, ARN, or both.
advancedSecurityOptionsInput_masterUserOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe MasterUserOptions)
advancedSecurityOptionsInput_masterUserOptions = Lens.lens (\AdvancedSecurityOptionsInput' {masterUserOptions} -> masterUserOptions) (\s@AdvancedSecurityOptionsInput' {} a -> s {masterUserOptions = a} :: AdvancedSecurityOptionsInput)

instance
  Prelude.Hashable
    AdvancedSecurityOptionsInput
  where
  hashWithSalt _salt AdvancedSecurityOptionsInput' {..} =
    _salt
      `Prelude.hashWithSalt` internalUserDatabaseEnabled
      `Prelude.hashWithSalt` sAMLOptions
      `Prelude.hashWithSalt` anonymousAuthEnabled
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` masterUserOptions

instance Prelude.NFData AdvancedSecurityOptionsInput where
  rnf AdvancedSecurityOptionsInput' {..} =
    Prelude.rnf internalUserDatabaseEnabled
      `Prelude.seq` Prelude.rnf sAMLOptions
      `Prelude.seq` Prelude.rnf anonymousAuthEnabled
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf masterUserOptions

instance Data.ToJSON AdvancedSecurityOptionsInput where
  toJSON AdvancedSecurityOptionsInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InternalUserDatabaseEnabled" Data..=)
              Prelude.<$> internalUserDatabaseEnabled,
            ("SAMLOptions" Data..=) Prelude.<$> sAMLOptions,
            ("AnonymousAuthEnabled" Data..=)
              Prelude.<$> anonymousAuthEnabled,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("MasterUserOptions" Data..=)
              Prelude.<$> masterUserOptions
          ]
      )
