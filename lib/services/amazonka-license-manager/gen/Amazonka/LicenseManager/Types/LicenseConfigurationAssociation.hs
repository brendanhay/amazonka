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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseConfigurationAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes an association with a license configuration.
--
-- /See:/ 'newLicenseConfigurationAssociation' smart constructor.
data LicenseConfigurationAssociation = LicenseConfigurationAssociation'
  { -- | Type of server resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | ID of the Amazon Web Services account that owns the resource consuming
    -- licenses.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Time when the license configuration was associated with the resource.
    associationTime :: Prelude.Maybe Core.POSIX,
    -- | Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Scope of AMI associations. The possible value is @cross-account@.
    amiAssociationScope :: Prelude.Maybe Prelude.Text
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
-- 'resourceType', 'licenseConfigurationAssociation_resourceType' - Type of server resource.
--
-- 'resourceOwnerId', 'licenseConfigurationAssociation_resourceOwnerId' - ID of the Amazon Web Services account that owns the resource consuming
-- licenses.
--
-- 'associationTime', 'licenseConfigurationAssociation_associationTime' - Time when the license configuration was associated with the resource.
--
-- 'resourceArn', 'licenseConfigurationAssociation_resourceArn' - Amazon Resource Name (ARN) of the resource.
--
-- 'amiAssociationScope', 'licenseConfigurationAssociation_amiAssociationScope' - Scope of AMI associations. The possible value is @cross-account@.
newLicenseConfigurationAssociation ::
  LicenseConfigurationAssociation
newLicenseConfigurationAssociation =
  LicenseConfigurationAssociation'
    { resourceType =
        Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      associationTime = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      amiAssociationScope = Prelude.Nothing
    }

-- | Type of server resource.
licenseConfigurationAssociation_resourceType :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe ResourceType)
licenseConfigurationAssociation_resourceType = Lens.lens (\LicenseConfigurationAssociation' {resourceType} -> resourceType) (\s@LicenseConfigurationAssociation' {} a -> s {resourceType = a} :: LicenseConfigurationAssociation)

-- | ID of the Amazon Web Services account that owns the resource consuming
-- licenses.
licenseConfigurationAssociation_resourceOwnerId :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.Text)
licenseConfigurationAssociation_resourceOwnerId = Lens.lens (\LicenseConfigurationAssociation' {resourceOwnerId} -> resourceOwnerId) (\s@LicenseConfigurationAssociation' {} a -> s {resourceOwnerId = a} :: LicenseConfigurationAssociation)

-- | Time when the license configuration was associated with the resource.
licenseConfigurationAssociation_associationTime :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.UTCTime)
licenseConfigurationAssociation_associationTime = Lens.lens (\LicenseConfigurationAssociation' {associationTime} -> associationTime) (\s@LicenseConfigurationAssociation' {} a -> s {associationTime = a} :: LicenseConfigurationAssociation) Prelude.. Lens.mapping Core._Time

-- | Amazon Resource Name (ARN) of the resource.
licenseConfigurationAssociation_resourceArn :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.Text)
licenseConfigurationAssociation_resourceArn = Lens.lens (\LicenseConfigurationAssociation' {resourceArn} -> resourceArn) (\s@LicenseConfigurationAssociation' {} a -> s {resourceArn = a} :: LicenseConfigurationAssociation)

-- | Scope of AMI associations. The possible value is @cross-account@.
licenseConfigurationAssociation_amiAssociationScope :: Lens.Lens' LicenseConfigurationAssociation (Prelude.Maybe Prelude.Text)
licenseConfigurationAssociation_amiAssociationScope = Lens.lens (\LicenseConfigurationAssociation' {amiAssociationScope} -> amiAssociationScope) (\s@LicenseConfigurationAssociation' {} a -> s {amiAssociationScope = a} :: LicenseConfigurationAssociation)

instance
  Core.FromJSON
    LicenseConfigurationAssociation
  where
  parseJSON =
    Core.withObject
      "LicenseConfigurationAssociation"
      ( \x ->
          LicenseConfigurationAssociation'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "ResourceOwnerId")
            Prelude.<*> (x Core..:? "AssociationTime")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "AmiAssociationScope")
      )

instance
  Prelude.Hashable
    LicenseConfigurationAssociation
  where
  hashWithSalt
    _salt
    LicenseConfigurationAssociation' {..} =
      _salt `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` resourceOwnerId
        `Prelude.hashWithSalt` associationTime
        `Prelude.hashWithSalt` resourceArn
        `Prelude.hashWithSalt` amiAssociationScope

instance
  Prelude.NFData
    LicenseConfigurationAssociation
  where
  rnf LicenseConfigurationAssociation' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf associationTime
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf amiAssociationScope
