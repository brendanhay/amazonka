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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.Types.LinuxSubscriptionsDiscoverySettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerLinuxSubscriptions.Types.LinuxSubscriptionsDiscoverySettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerLinuxSubscriptions.Types.OrganizationIntegration
import qualified Amazonka.Prelude as Prelude

-- | Lists the settings defined for discovering Linux subscriptions.
--
-- /See:/ 'newLinuxSubscriptionsDiscoverySettings' smart constructor.
data LinuxSubscriptionsDiscoverySettings = LinuxSubscriptionsDiscoverySettings'
  { -- | Details if you have enabled resource discovery across your accounts in
    -- Organizations.
    organizationIntegration :: OrganizationIntegration,
    -- | The Regions in which to discover data for Linux subscriptions.
    sourceRegions :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LinuxSubscriptionsDiscoverySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationIntegration', 'linuxSubscriptionsDiscoverySettings_organizationIntegration' - Details if you have enabled resource discovery across your accounts in
-- Organizations.
--
-- 'sourceRegions', 'linuxSubscriptionsDiscoverySettings_sourceRegions' - The Regions in which to discover data for Linux subscriptions.
newLinuxSubscriptionsDiscoverySettings ::
  -- | 'organizationIntegration'
  OrganizationIntegration ->
  -- | 'sourceRegions'
  Prelude.NonEmpty Prelude.Text ->
  LinuxSubscriptionsDiscoverySettings
newLinuxSubscriptionsDiscoverySettings
  pOrganizationIntegration_
  pSourceRegions_ =
    LinuxSubscriptionsDiscoverySettings'
      { organizationIntegration =
          pOrganizationIntegration_,
        sourceRegions =
          Lens.coerced Lens.# pSourceRegions_
      }

-- | Details if you have enabled resource discovery across your accounts in
-- Organizations.
linuxSubscriptionsDiscoverySettings_organizationIntegration :: Lens.Lens' LinuxSubscriptionsDiscoverySettings OrganizationIntegration
linuxSubscriptionsDiscoverySettings_organizationIntegration = Lens.lens (\LinuxSubscriptionsDiscoverySettings' {organizationIntegration} -> organizationIntegration) (\s@LinuxSubscriptionsDiscoverySettings' {} a -> s {organizationIntegration = a} :: LinuxSubscriptionsDiscoverySettings)

-- | The Regions in which to discover data for Linux subscriptions.
linuxSubscriptionsDiscoverySettings_sourceRegions :: Lens.Lens' LinuxSubscriptionsDiscoverySettings (Prelude.NonEmpty Prelude.Text)
linuxSubscriptionsDiscoverySettings_sourceRegions = Lens.lens (\LinuxSubscriptionsDiscoverySettings' {sourceRegions} -> sourceRegions) (\s@LinuxSubscriptionsDiscoverySettings' {} a -> s {sourceRegions = a} :: LinuxSubscriptionsDiscoverySettings) Prelude.. Lens.coerced

instance
  Data.FromJSON
    LinuxSubscriptionsDiscoverySettings
  where
  parseJSON =
    Data.withObject
      "LinuxSubscriptionsDiscoverySettings"
      ( \x ->
          LinuxSubscriptionsDiscoverySettings'
            Prelude.<$> (x Data..: "OrganizationIntegration")
            Prelude.<*> (x Data..: "SourceRegions")
      )

instance
  Prelude.Hashable
    LinuxSubscriptionsDiscoverySettings
  where
  hashWithSalt
    _salt
    LinuxSubscriptionsDiscoverySettings' {..} =
      _salt
        `Prelude.hashWithSalt` organizationIntegration
        `Prelude.hashWithSalt` sourceRegions

instance
  Prelude.NFData
    LinuxSubscriptionsDiscoverySettings
  where
  rnf LinuxSubscriptionsDiscoverySettings' {..} =
    Prelude.rnf organizationIntegration
      `Prelude.seq` Prelude.rnf sourceRegions

instance
  Data.ToJSON
    LinuxSubscriptionsDiscoverySettings
  where
  toJSON LinuxSubscriptionsDiscoverySettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationIntegration"
                  Data..= organizationIntegration
              ),
            Prelude.Just
              ("SourceRegions" Data..= sourceRegions)
          ]
      )
