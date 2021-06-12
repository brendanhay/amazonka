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
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.MasterUserOptions
import Network.AWS.ElasticSearch.Types.SAMLOptionsInput
import qualified Network.AWS.Lens as Lens

-- | Specifies the advanced security configuration: whether advanced security
-- is enabled, whether the internal database option is enabled, master
-- username and password (if internal database is enabled), and master user
-- ARN (if IAM is enabled).
--
-- /See:/ 'newAdvancedSecurityOptionsInput' smart constructor.
data AdvancedSecurityOptionsInput = AdvancedSecurityOptionsInput'
  { -- | True if the internal user database is enabled.
    internalUserDatabaseEnabled :: Core.Maybe Core.Bool,
    -- | Specifies the SAML application configuration for the domain.
    sAMLOptions :: Core.Maybe SAMLOptionsInput,
    -- | True if advanced security is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | Credentials for the master user: username and password, ARN, or both.
    masterUserOptions :: Core.Maybe MasterUserOptions
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'enabled', 'advancedSecurityOptionsInput_enabled' - True if advanced security is enabled.
--
-- 'masterUserOptions', 'advancedSecurityOptionsInput_masterUserOptions' - Credentials for the master user: username and password, ARN, or both.
newAdvancedSecurityOptionsInput ::
  AdvancedSecurityOptionsInput
newAdvancedSecurityOptionsInput =
  AdvancedSecurityOptionsInput'
    { internalUserDatabaseEnabled =
        Core.Nothing,
      sAMLOptions = Core.Nothing,
      enabled = Core.Nothing,
      masterUserOptions = Core.Nothing
    }

-- | True if the internal user database is enabled.
advancedSecurityOptionsInput_internalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe Core.Bool)
advancedSecurityOptionsInput_internalUserDatabaseEnabled = Lens.lens (\AdvancedSecurityOptionsInput' {internalUserDatabaseEnabled} -> internalUserDatabaseEnabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {internalUserDatabaseEnabled = a} :: AdvancedSecurityOptionsInput)

-- | Specifies the SAML application configuration for the domain.
advancedSecurityOptionsInput_sAMLOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe SAMLOptionsInput)
advancedSecurityOptionsInput_sAMLOptions = Lens.lens (\AdvancedSecurityOptionsInput' {sAMLOptions} -> sAMLOptions) (\s@AdvancedSecurityOptionsInput' {} a -> s {sAMLOptions = a} :: AdvancedSecurityOptionsInput)

-- | True if advanced security is enabled.
advancedSecurityOptionsInput_enabled :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe Core.Bool)
advancedSecurityOptionsInput_enabled = Lens.lens (\AdvancedSecurityOptionsInput' {enabled} -> enabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {enabled = a} :: AdvancedSecurityOptionsInput)

-- | Credentials for the master user: username and password, ARN, or both.
advancedSecurityOptionsInput_masterUserOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Core.Maybe MasterUserOptions)
advancedSecurityOptionsInput_masterUserOptions = Lens.lens (\AdvancedSecurityOptionsInput' {masterUserOptions} -> masterUserOptions) (\s@AdvancedSecurityOptionsInput' {} a -> s {masterUserOptions = a} :: AdvancedSecurityOptionsInput)

instance Core.Hashable AdvancedSecurityOptionsInput

instance Core.NFData AdvancedSecurityOptionsInput

instance Core.ToJSON AdvancedSecurityOptionsInput where
  toJSON AdvancedSecurityOptionsInput' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InternalUserDatabaseEnabled" Core..=)
              Core.<$> internalUserDatabaseEnabled,
            ("SAMLOptions" Core..=) Core.<$> sAMLOptions,
            ("Enabled" Core..=) Core.<$> enabled,
            ("MasterUserOptions" Core..=)
              Core.<$> masterUserOptions
          ]
      )
