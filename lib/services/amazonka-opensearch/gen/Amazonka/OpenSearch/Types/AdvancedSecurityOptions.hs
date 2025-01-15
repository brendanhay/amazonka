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
-- Module      : Amazonka.OpenSearch.Types.AdvancedSecurityOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AdvancedSecurityOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.SAMLOptionsOutput
import qualified Amazonka.Prelude as Prelude

-- | Container for fine-grained access control settings.
--
-- /See:/ 'newAdvancedSecurityOptions' smart constructor.
data AdvancedSecurityOptions = AdvancedSecurityOptions'
  { -- | Date and time when the migration period will be disabled. Only necessary
    -- when
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
    anonymousAuthDisableDate :: Prelude.Maybe Data.POSIX,
    -- | True if a 30-day migration period is enabled, during which
    -- administrators can create role mappings. Only necessary when
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
    anonymousAuthEnabled :: Prelude.Maybe Prelude.Bool,
    -- | True if fine-grained access control is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | True if the internal user database is enabled.
    internalUserDatabaseEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Container for information about the SAML configuration for OpenSearch
    -- Dashboards.
    sAMLOptions :: Prelude.Maybe SAMLOptionsOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedSecurityOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anonymousAuthDisableDate', 'advancedSecurityOptions_anonymousAuthDisableDate' - Date and time when the migration period will be disabled. Only necessary
-- when
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
--
-- 'anonymousAuthEnabled', 'advancedSecurityOptions_anonymousAuthEnabled' - True if a 30-day migration period is enabled, during which
-- administrators can create role mappings. Only necessary when
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
--
-- 'enabled', 'advancedSecurityOptions_enabled' - True if fine-grained access control is enabled.
--
-- 'internalUserDatabaseEnabled', 'advancedSecurityOptions_internalUserDatabaseEnabled' - True if the internal user database is enabled.
--
-- 'sAMLOptions', 'advancedSecurityOptions_sAMLOptions' - Container for information about the SAML configuration for OpenSearch
-- Dashboards.
newAdvancedSecurityOptions ::
  AdvancedSecurityOptions
newAdvancedSecurityOptions =
  AdvancedSecurityOptions'
    { anonymousAuthDisableDate =
        Prelude.Nothing,
      anonymousAuthEnabled = Prelude.Nothing,
      enabled = Prelude.Nothing,
      internalUserDatabaseEnabled = Prelude.Nothing,
      sAMLOptions = Prelude.Nothing
    }

-- | Date and time when the migration period will be disabled. Only necessary
-- when
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
advancedSecurityOptions_anonymousAuthDisableDate :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe Prelude.UTCTime)
advancedSecurityOptions_anonymousAuthDisableDate = Lens.lens (\AdvancedSecurityOptions' {anonymousAuthDisableDate} -> anonymousAuthDisableDate) (\s@AdvancedSecurityOptions' {} a -> s {anonymousAuthDisableDate = a} :: AdvancedSecurityOptions) Prelude.. Lens.mapping Data._Time

-- | True if a 30-day migration period is enabled, during which
-- administrators can create role mappings. Only necessary when
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/fgac.html#fgac-enabling-existing enabling fine-grained access control on an existing domain>.
advancedSecurityOptions_anonymousAuthEnabled :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe Prelude.Bool)
advancedSecurityOptions_anonymousAuthEnabled = Lens.lens (\AdvancedSecurityOptions' {anonymousAuthEnabled} -> anonymousAuthEnabled) (\s@AdvancedSecurityOptions' {} a -> s {anonymousAuthEnabled = a} :: AdvancedSecurityOptions)

-- | True if fine-grained access control is enabled.
advancedSecurityOptions_enabled :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe Prelude.Bool)
advancedSecurityOptions_enabled = Lens.lens (\AdvancedSecurityOptions' {enabled} -> enabled) (\s@AdvancedSecurityOptions' {} a -> s {enabled = a} :: AdvancedSecurityOptions)

-- | True if the internal user database is enabled.
advancedSecurityOptions_internalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe Prelude.Bool)
advancedSecurityOptions_internalUserDatabaseEnabled = Lens.lens (\AdvancedSecurityOptions' {internalUserDatabaseEnabled} -> internalUserDatabaseEnabled) (\s@AdvancedSecurityOptions' {} a -> s {internalUserDatabaseEnabled = a} :: AdvancedSecurityOptions)

-- | Container for information about the SAML configuration for OpenSearch
-- Dashboards.
advancedSecurityOptions_sAMLOptions :: Lens.Lens' AdvancedSecurityOptions (Prelude.Maybe SAMLOptionsOutput)
advancedSecurityOptions_sAMLOptions = Lens.lens (\AdvancedSecurityOptions' {sAMLOptions} -> sAMLOptions) (\s@AdvancedSecurityOptions' {} a -> s {sAMLOptions = a} :: AdvancedSecurityOptions)

instance Data.FromJSON AdvancedSecurityOptions where
  parseJSON =
    Data.withObject
      "AdvancedSecurityOptions"
      ( \x ->
          AdvancedSecurityOptions'
            Prelude.<$> (x Data..:? "AnonymousAuthDisableDate")
            Prelude.<*> (x Data..:? "AnonymousAuthEnabled")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "InternalUserDatabaseEnabled")
            Prelude.<*> (x Data..:? "SAMLOptions")
      )

instance Prelude.Hashable AdvancedSecurityOptions where
  hashWithSalt _salt AdvancedSecurityOptions' {..} =
    _salt
      `Prelude.hashWithSalt` anonymousAuthDisableDate
      `Prelude.hashWithSalt` anonymousAuthEnabled
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` internalUserDatabaseEnabled
      `Prelude.hashWithSalt` sAMLOptions

instance Prelude.NFData AdvancedSecurityOptions where
  rnf AdvancedSecurityOptions' {..} =
    Prelude.rnf anonymousAuthDisableDate `Prelude.seq`
      Prelude.rnf anonymousAuthEnabled `Prelude.seq`
        Prelude.rnf enabled `Prelude.seq`
          Prelude.rnf internalUserDatabaseEnabled `Prelude.seq`
            Prelude.rnf sAMLOptions
