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
-- Module      : Amazonka.OpenSearch.Types.AdvancedSecurityOptionsInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AdvancedSecurityOptionsInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.MasterUserOptions
import Amazonka.OpenSearch.Types.SAMLOptionsInput
import qualified Amazonka.Prelude as Prelude

-- | Options for enabling and configuring fine-grained access control. For
-- more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html Fine-grained access control in Amazon OpenSearch Service>.
--
-- /See:/ 'newAdvancedSecurityOptionsInput' smart constructor.
data AdvancedSecurityOptionsInput = AdvancedSecurityOptionsInput'
  { -- | True to enable the internal user database.
    internalUserDatabaseEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Container for information about the SAML configuration for OpenSearch
    -- Dashboards.
    sAMLOptions :: Prelude.Maybe SAMLOptionsInput,
    -- | True to enable a 30-day migration period during which administrators can
    -- create role mappings. Only necessary when
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
    anonymousAuthEnabled :: Prelude.Maybe Prelude.Bool,
    -- | True to enable fine-grained access control.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Container for information about the master user.
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
-- 'internalUserDatabaseEnabled', 'advancedSecurityOptionsInput_internalUserDatabaseEnabled' - True to enable the internal user database.
--
-- 'sAMLOptions', 'advancedSecurityOptionsInput_sAMLOptions' - Container for information about the SAML configuration for OpenSearch
-- Dashboards.
--
-- 'anonymousAuthEnabled', 'advancedSecurityOptionsInput_anonymousAuthEnabled' - True to enable a 30-day migration period during which administrators can
-- create role mappings. Only necessary when
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
--
-- 'enabled', 'advancedSecurityOptionsInput_enabled' - True to enable fine-grained access control.
--
-- 'masterUserOptions', 'advancedSecurityOptionsInput_masterUserOptions' - Container for information about the master user.
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

-- | True to enable the internal user database.
advancedSecurityOptionsInput_internalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe Prelude.Bool)
advancedSecurityOptionsInput_internalUserDatabaseEnabled = Lens.lens (\AdvancedSecurityOptionsInput' {internalUserDatabaseEnabled} -> internalUserDatabaseEnabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {internalUserDatabaseEnabled = a} :: AdvancedSecurityOptionsInput)

-- | Container for information about the SAML configuration for OpenSearch
-- Dashboards.
advancedSecurityOptionsInput_sAMLOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe SAMLOptionsInput)
advancedSecurityOptionsInput_sAMLOptions = Lens.lens (\AdvancedSecurityOptionsInput' {sAMLOptions} -> sAMLOptions) (\s@AdvancedSecurityOptionsInput' {} a -> s {sAMLOptions = a} :: AdvancedSecurityOptionsInput)

-- | True to enable a 30-day migration period during which administrators can
-- create role mappings. Only necessary when
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
advancedSecurityOptionsInput_anonymousAuthEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe Prelude.Bool)
advancedSecurityOptionsInput_anonymousAuthEnabled = Lens.lens (\AdvancedSecurityOptionsInput' {anonymousAuthEnabled} -> anonymousAuthEnabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {anonymousAuthEnabled = a} :: AdvancedSecurityOptionsInput)

-- | True to enable fine-grained access control.
advancedSecurityOptionsInput_enabled :: Lens.Lens' AdvancedSecurityOptionsInput (Prelude.Maybe Prelude.Bool)
advancedSecurityOptionsInput_enabled = Lens.lens (\AdvancedSecurityOptionsInput' {enabled} -> enabled) (\s@AdvancedSecurityOptionsInput' {} a -> s {enabled = a} :: AdvancedSecurityOptionsInput)

-- | Container for information about the master user.
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
