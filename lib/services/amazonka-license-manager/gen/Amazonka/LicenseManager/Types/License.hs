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
-- Module      : Amazonka.LicenseManager.Types.License
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.License where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types.ConsumptionConfiguration
import Amazonka.LicenseManager.Types.DatetimeRange
import Amazonka.LicenseManager.Types.Entitlement
import Amazonka.LicenseManager.Types.IssuerDetails
import Amazonka.LicenseManager.Types.LicenseStatus
import Amazonka.LicenseManager.Types.Metadata
import qualified Amazonka.Prelude as Prelude

-- | Software license that is managed in License Manager.
--
-- /See:/ 'newLicense' smart constructor.
data License = License'
  { -- | License status.
    status :: Prelude.Maybe LicenseStatus,
    -- | Product name.
    productName :: Prelude.Maybe Prelude.Text,
    -- | License name.
    licenseName :: Prelude.Maybe Prelude.Text,
    -- | Product SKU.
    productSKU :: Prelude.Maybe Prelude.Text,
    -- | Home Region of the license.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | License version.
    version :: Prelude.Maybe Prelude.Text,
    -- | License entitlements.
    entitlements :: Prelude.Maybe [Entitlement],
    -- | Date and time range during which the license is valid, in ISO8601-UTC
    -- format.
    validity :: Prelude.Maybe DatetimeRange,
    -- | Configuration for consumption of the license.
    consumptionConfiguration :: Prelude.Maybe ConsumptionConfiguration,
    -- | License beneficiary.
    beneficiary :: Prelude.Maybe Prelude.Text,
    -- | License issuer.
    issuer :: Prelude.Maybe IssuerDetails,
    -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Maybe Prelude.Text,
    -- | License metadata.
    licenseMetadata :: Prelude.Maybe [Metadata],
    -- | License creation time.
    createTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'License' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'license_status' - License status.
--
-- 'productName', 'license_productName' - Product name.
--
-- 'licenseName', 'license_licenseName' - License name.
--
-- 'productSKU', 'license_productSKU' - Product SKU.
--
-- 'homeRegion', 'license_homeRegion' - Home Region of the license.
--
-- 'version', 'license_version' - License version.
--
-- 'entitlements', 'license_entitlements' - License entitlements.
--
-- 'validity', 'license_validity' - Date and time range during which the license is valid, in ISO8601-UTC
-- format.
--
-- 'consumptionConfiguration', 'license_consumptionConfiguration' - Configuration for consumption of the license.
--
-- 'beneficiary', 'license_beneficiary' - License beneficiary.
--
-- 'issuer', 'license_issuer' - License issuer.
--
-- 'licenseArn', 'license_licenseArn' - Amazon Resource Name (ARN) of the license.
--
-- 'licenseMetadata', 'license_licenseMetadata' - License metadata.
--
-- 'createTime', 'license_createTime' - License creation time.
newLicense ::
  License
newLicense =
  License'
    { status = Prelude.Nothing,
      productName = Prelude.Nothing,
      licenseName = Prelude.Nothing,
      productSKU = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      version = Prelude.Nothing,
      entitlements = Prelude.Nothing,
      validity = Prelude.Nothing,
      consumptionConfiguration = Prelude.Nothing,
      beneficiary = Prelude.Nothing,
      issuer = Prelude.Nothing,
      licenseArn = Prelude.Nothing,
      licenseMetadata = Prelude.Nothing,
      createTime = Prelude.Nothing
    }

-- | License status.
license_status :: Lens.Lens' License (Prelude.Maybe LicenseStatus)
license_status = Lens.lens (\License' {status} -> status) (\s@License' {} a -> s {status = a} :: License)

-- | Product name.
license_productName :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_productName = Lens.lens (\License' {productName} -> productName) (\s@License' {} a -> s {productName = a} :: License)

-- | License name.
license_licenseName :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_licenseName = Lens.lens (\License' {licenseName} -> licenseName) (\s@License' {} a -> s {licenseName = a} :: License)

-- | Product SKU.
license_productSKU :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_productSKU = Lens.lens (\License' {productSKU} -> productSKU) (\s@License' {} a -> s {productSKU = a} :: License)

-- | Home Region of the license.
license_homeRegion :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_homeRegion = Lens.lens (\License' {homeRegion} -> homeRegion) (\s@License' {} a -> s {homeRegion = a} :: License)

-- | License version.
license_version :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_version = Lens.lens (\License' {version} -> version) (\s@License' {} a -> s {version = a} :: License)

-- | License entitlements.
license_entitlements :: Lens.Lens' License (Prelude.Maybe [Entitlement])
license_entitlements = Lens.lens (\License' {entitlements} -> entitlements) (\s@License' {} a -> s {entitlements = a} :: License) Prelude.. Lens.mapping Lens.coerced

-- | Date and time range during which the license is valid, in ISO8601-UTC
-- format.
license_validity :: Lens.Lens' License (Prelude.Maybe DatetimeRange)
license_validity = Lens.lens (\License' {validity} -> validity) (\s@License' {} a -> s {validity = a} :: License)

-- | Configuration for consumption of the license.
license_consumptionConfiguration :: Lens.Lens' License (Prelude.Maybe ConsumptionConfiguration)
license_consumptionConfiguration = Lens.lens (\License' {consumptionConfiguration} -> consumptionConfiguration) (\s@License' {} a -> s {consumptionConfiguration = a} :: License)

-- | License beneficiary.
license_beneficiary :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_beneficiary = Lens.lens (\License' {beneficiary} -> beneficiary) (\s@License' {} a -> s {beneficiary = a} :: License)

-- | License issuer.
license_issuer :: Lens.Lens' License (Prelude.Maybe IssuerDetails)
license_issuer = Lens.lens (\License' {issuer} -> issuer) (\s@License' {} a -> s {issuer = a} :: License)

-- | Amazon Resource Name (ARN) of the license.
license_licenseArn :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_licenseArn = Lens.lens (\License' {licenseArn} -> licenseArn) (\s@License' {} a -> s {licenseArn = a} :: License)

-- | License metadata.
license_licenseMetadata :: Lens.Lens' License (Prelude.Maybe [Metadata])
license_licenseMetadata = Lens.lens (\License' {licenseMetadata} -> licenseMetadata) (\s@License' {} a -> s {licenseMetadata = a} :: License) Prelude.. Lens.mapping Lens.coerced

-- | License creation time.
license_createTime :: Lens.Lens' License (Prelude.Maybe Prelude.Text)
license_createTime = Lens.lens (\License' {createTime} -> createTime) (\s@License' {} a -> s {createTime = a} :: License)

instance Core.FromJSON License where
  parseJSON =
    Core.withObject
      "License"
      ( \x ->
          License'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ProductName")
            Prelude.<*> (x Core..:? "LicenseName")
            Prelude.<*> (x Core..:? "ProductSKU")
            Prelude.<*> (x Core..:? "HomeRegion")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Entitlements" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Validity")
            Prelude.<*> (x Core..:? "ConsumptionConfiguration")
            Prelude.<*> (x Core..:? "Beneficiary")
            Prelude.<*> (x Core..:? "Issuer")
            Prelude.<*> (x Core..:? "LicenseArn")
            Prelude.<*> ( x Core..:? "LicenseMetadata"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreateTime")
      )

instance Prelude.Hashable License where
  hashWithSalt _salt License' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` licenseName
      `Prelude.hashWithSalt` productSKU
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` entitlements
      `Prelude.hashWithSalt` validity
      `Prelude.hashWithSalt` consumptionConfiguration
      `Prelude.hashWithSalt` beneficiary
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` licenseArn
      `Prelude.hashWithSalt` licenseMetadata
      `Prelude.hashWithSalt` createTime

instance Prelude.NFData License where
  rnf License' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf licenseName
      `Prelude.seq` Prelude.rnf productSKU
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf validity
      `Prelude.seq` Prelude.rnf consumptionConfiguration
      `Prelude.seq` Prelude.rnf beneficiary
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf licenseMetadata
      `Prelude.seq` Prelude.rnf createTime
