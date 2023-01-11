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
-- Module      : Amazonka.LicenseManager.Types.LicenseConfigurationUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseConfigurationUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Details about the usage of a resource associated with a license
-- configuration.
--
-- /See:/ 'newLicenseConfigurationUsage' smart constructor.
data LicenseConfigurationUsage = LicenseConfigurationUsage'
  { -- | Time when the license configuration was initially associated with the
    -- resource.
    associationTime :: Prelude.Maybe Data.POSIX,
    -- | Number of licenses consumed by the resource.
    consumedLicenses :: Prelude.Maybe Prelude.Integer,
    -- | Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | ID of the account that owns the resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Status of the resource.
    resourceStatus :: Prelude.Maybe Prelude.Text,
    -- | Type of resource.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseConfigurationUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationTime', 'licenseConfigurationUsage_associationTime' - Time when the license configuration was initially associated with the
-- resource.
--
-- 'consumedLicenses', 'licenseConfigurationUsage_consumedLicenses' - Number of licenses consumed by the resource.
--
-- 'resourceArn', 'licenseConfigurationUsage_resourceArn' - Amazon Resource Name (ARN) of the resource.
--
-- 'resourceOwnerId', 'licenseConfigurationUsage_resourceOwnerId' - ID of the account that owns the resource.
--
-- 'resourceStatus', 'licenseConfigurationUsage_resourceStatus' - Status of the resource.
--
-- 'resourceType', 'licenseConfigurationUsage_resourceType' - Type of resource.
newLicenseConfigurationUsage ::
  LicenseConfigurationUsage
newLicenseConfigurationUsage =
  LicenseConfigurationUsage'
    { associationTime =
        Prelude.Nothing,
      consumedLicenses = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceStatus = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Time when the license configuration was initially associated with the
-- resource.
licenseConfigurationUsage_associationTime :: Lens.Lens' LicenseConfigurationUsage (Prelude.Maybe Prelude.UTCTime)
licenseConfigurationUsage_associationTime = Lens.lens (\LicenseConfigurationUsage' {associationTime} -> associationTime) (\s@LicenseConfigurationUsage' {} a -> s {associationTime = a} :: LicenseConfigurationUsage) Prelude.. Lens.mapping Data._Time

-- | Number of licenses consumed by the resource.
licenseConfigurationUsage_consumedLicenses :: Lens.Lens' LicenseConfigurationUsage (Prelude.Maybe Prelude.Integer)
licenseConfigurationUsage_consumedLicenses = Lens.lens (\LicenseConfigurationUsage' {consumedLicenses} -> consumedLicenses) (\s@LicenseConfigurationUsage' {} a -> s {consumedLicenses = a} :: LicenseConfigurationUsage)

-- | Amazon Resource Name (ARN) of the resource.
licenseConfigurationUsage_resourceArn :: Lens.Lens' LicenseConfigurationUsage (Prelude.Maybe Prelude.Text)
licenseConfigurationUsage_resourceArn = Lens.lens (\LicenseConfigurationUsage' {resourceArn} -> resourceArn) (\s@LicenseConfigurationUsage' {} a -> s {resourceArn = a} :: LicenseConfigurationUsage)

-- | ID of the account that owns the resource.
licenseConfigurationUsage_resourceOwnerId :: Lens.Lens' LicenseConfigurationUsage (Prelude.Maybe Prelude.Text)
licenseConfigurationUsage_resourceOwnerId = Lens.lens (\LicenseConfigurationUsage' {resourceOwnerId} -> resourceOwnerId) (\s@LicenseConfigurationUsage' {} a -> s {resourceOwnerId = a} :: LicenseConfigurationUsage)

-- | Status of the resource.
licenseConfigurationUsage_resourceStatus :: Lens.Lens' LicenseConfigurationUsage (Prelude.Maybe Prelude.Text)
licenseConfigurationUsage_resourceStatus = Lens.lens (\LicenseConfigurationUsage' {resourceStatus} -> resourceStatus) (\s@LicenseConfigurationUsage' {} a -> s {resourceStatus = a} :: LicenseConfigurationUsage)

-- | Type of resource.
licenseConfigurationUsage_resourceType :: Lens.Lens' LicenseConfigurationUsage (Prelude.Maybe ResourceType)
licenseConfigurationUsage_resourceType = Lens.lens (\LicenseConfigurationUsage' {resourceType} -> resourceType) (\s@LicenseConfigurationUsage' {} a -> s {resourceType = a} :: LicenseConfigurationUsage)

instance Data.FromJSON LicenseConfigurationUsage where
  parseJSON =
    Data.withObject
      "LicenseConfigurationUsage"
      ( \x ->
          LicenseConfigurationUsage'
            Prelude.<$> (x Data..:? "AssociationTime")
            Prelude.<*> (x Data..:? "ConsumedLicenses")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceOwnerId")
            Prelude.<*> (x Data..:? "ResourceStatus")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance Prelude.Hashable LicenseConfigurationUsage where
  hashWithSalt _salt LicenseConfigurationUsage' {..} =
    _salt `Prelude.hashWithSalt` associationTime
      `Prelude.hashWithSalt` consumedLicenses
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceOwnerId
      `Prelude.hashWithSalt` resourceStatus
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData LicenseConfigurationUsage where
  rnf LicenseConfigurationUsage' {..} =
    Prelude.rnf associationTime
      `Prelude.seq` Prelude.rnf consumedLicenses
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceStatus
      `Prelude.seq` Prelude.rnf resourceType
