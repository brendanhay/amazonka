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
-- Module      : Amazonka.LicenseManager.Types.LicenseConfigurationAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseConfigurationAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes an association with a license configuration.
--
-- /See:/ 'newLicenseConfigurationAssociation' smart constructor.
data LicenseConfigurationAssociation = LicenseConfigurationAssociation'
  { -- | Scope of AMI associations. The possible value is @cross-account@.
    amiAssociationScope :: Prelude.Maybe Prelude.Text,
    -- | Time when the license configuration was associated with the resource.
    associationTime :: Prelude.Maybe Data.POSIX,
    -- | Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | ID of the Amazon Web Services account that owns the resource consuming
    -- licenses.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Type of server resource.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseConfigurationAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiAssociationScope', 'licenseConfigurationAssociation_amiAssociationScope' - Scope of AMI associations. The possible value is @cross-account@.
--
-- 'associationTime', 'licenseConfigurationAssociation_associationTime' - Time when the license configuration was associated with the resource.
--
-- 'resourceArn', 'licenseConfigurationAssociation_resourceArn' - Amazon Resource Name (ARN) of the resource.
--
-- 'resourceOwnerId', 'licenseConfigurationAssociation_resourceOwnerId' - ID of the Amazon Web Services account that owns the resource consuming
-- licenses.
--
-- 'resourceType', 'licenseConfigurationAssociation_resourceType' - Type of server resource.
newLicenseConfigurationAssociation ::
  LicenseConfigurationAssociation
newLicenseConfigurationAssociation =
  LicenseConfigurationAssociation'
    { amiAssociationScope =
        Prelude.Nothing,
      associationTime = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Scope of AMI associations. The possible value is @cross-account@.
licenseConfigurationAssociation_amiAssociationScope :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.Text)
licenseConfigurationAssociation_amiAssociationScope = Lens.lens (\LicenseConfigurationAssociation' {amiAssociationScope} -> amiAssociationScope) (\s@LicenseConfigurationAssociation' {} a -> s {amiAssociationScope = a} :: LicenseConfigurationAssociation)

-- | Time when the license configuration was associated with the resource.
licenseConfigurationAssociation_associationTime :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.UTCTime)
licenseConfigurationAssociation_associationTime = Lens.lens (\LicenseConfigurationAssociation' {associationTime} -> associationTime) (\s@LicenseConfigurationAssociation' {} a -> s {associationTime = a} :: LicenseConfigurationAssociation) Prelude.. Lens.mapping Data._Time

-- | Amazon Resource Name (ARN) of the resource.
licenseConfigurationAssociation_resourceArn :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.Text)
licenseConfigurationAssociation_resourceArn = Lens.lens (\LicenseConfigurationAssociation' {resourceArn} -> resourceArn) (\s@LicenseConfigurationAssociation' {} a -> s {resourceArn = a} :: LicenseConfigurationAssociation)

-- | ID of the Amazon Web Services account that owns the resource consuming
-- licenses.
licenseConfigurationAssociation_resourceOwnerId :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.Text)
licenseConfigurationAssociation_resourceOwnerId = Lens.lens (\LicenseConfigurationAssociation' {resourceOwnerId} -> resourceOwnerId) (\s@LicenseConfigurationAssociation' {} a -> s {resourceOwnerId = a} :: LicenseConfigurationAssociation)

-- | Type of server resource.
licenseConfigurationAssociation_resourceType :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe ResourceType)
licenseConfigurationAssociation_resourceType = Lens.lens (\LicenseConfigurationAssociation' {resourceType} -> resourceType) (\s@LicenseConfigurationAssociation' {} a -> s {resourceType = a} :: LicenseConfigurationAssociation)

instance
  Data.FromJSON
    LicenseConfigurationAssociation
  where
  parseJSON =
    Data.withObject
      "LicenseConfigurationAssociation"
      ( \x ->
          LicenseConfigurationAssociation'
            Prelude.<$> (x Data..:? "AmiAssociationScope")
            Prelude.<*> (x Data..:? "AssociationTime")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceOwnerId")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance
  Prelude.Hashable
    LicenseConfigurationAssociation
  where
  hashWithSalt
    _salt
    LicenseConfigurationAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` amiAssociationScope
        `Prelude.hashWithSalt` associationTime
        `Prelude.hashWithSalt` resourceArn
        `Prelude.hashWithSalt` resourceOwnerId
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    LicenseConfigurationAssociation
  where
  rnf LicenseConfigurationAssociation' {..} =
    Prelude.rnf amiAssociationScope
      `Prelude.seq` Prelude.rnf associationTime
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceType
