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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainMasterUserOptionsDetails

-- | Provides information about domain access control options.
--
-- /See:/ 'newAwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails = AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails'
  { -- | Enables fine-grained access control.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Enables the internal user database.
    internalUserDatabaseEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies information about the master user of the domain.
    masterUserOptions :: Prelude.Maybe AwsOpenSearchServiceDomainMasterUserOptionsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_enabled' - Enables fine-grained access control.
--
-- 'internalUserDatabaseEnabled', 'awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_internalUserDatabaseEnabled' - Enables the internal user database.
--
-- 'masterUserOptions', 'awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_masterUserOptions' - Specifies information about the master user of the domain.
newAwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails ::
  AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
newAwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails =
  AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails'
    { enabled =
        Prelude.Nothing,
      internalUserDatabaseEnabled =
        Prelude.Nothing,
      masterUserOptions =
        Prelude.Nothing
    }

-- | Enables fine-grained access control.
awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_enabled :: Lens.Lens' AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_enabled = Lens.lens (\AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {enabled} -> enabled) (\s@AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {} a -> s {enabled = a} :: AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails)

-- | Enables the internal user database.
awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_internalUserDatabaseEnabled :: Lens.Lens' AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_internalUserDatabaseEnabled = Lens.lens (\AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {internalUserDatabaseEnabled} -> internalUserDatabaseEnabled) (\s@AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {} a -> s {internalUserDatabaseEnabled = a} :: AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails)

-- | Specifies information about the master user of the domain.
awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_masterUserOptions :: Lens.Lens' AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails (Prelude.Maybe AwsOpenSearchServiceDomainMasterUserOptionsDetails)
awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_masterUserOptions = Lens.lens (\AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {masterUserOptions} -> masterUserOptions) (\s@AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {} a -> s {masterUserOptions = a} :: AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "InternalUserDatabaseEnabled")
            Prelude.<*> (x Data..:? "MasterUserOptions")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` internalUserDatabaseEnabled
        `Prelude.hashWithSalt` masterUserOptions

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {..} =
      Prelude.rnf enabled
        `Prelude.seq` Prelude.rnf internalUserDatabaseEnabled
        `Prelude.seq` Prelude.rnf masterUserOptions

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Enabled" Data..=) Prelude.<$> enabled,
              ("InternalUserDatabaseEnabled" Data..=)
                Prelude.<$> internalUserDatabaseEnabled,
              ("MasterUserOptions" Data..=)
                Prelude.<$> masterUserOptions
            ]
        )
