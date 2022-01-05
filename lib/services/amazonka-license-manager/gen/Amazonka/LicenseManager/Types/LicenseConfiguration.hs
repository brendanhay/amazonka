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
-- Module      : Amazonka.LicenseManager.Types.LicenseConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types.AutomatedDiscoveryInformation
import Amazonka.LicenseManager.Types.ConsumedLicenseSummary
import Amazonka.LicenseManager.Types.LicenseCountingType
import Amazonka.LicenseManager.Types.ManagedResourceSummary
import Amazonka.LicenseManager.Types.ProductInformation
import qualified Amazonka.Prelude as Prelude

-- | A license configuration is an abstraction of a customer license
-- agreement that can be consumed and enforced by License Manager.
-- Components include specifications for the license type (licensing by
-- instance, socket, CPU, or vCPU), allowed tenancy (shared tenancy,
-- Dedicated Instance, Dedicated Host, or all of these), host affinity (how
-- long a VM must be associated with a host), and the number of licenses
-- purchased and used.
--
-- /See:/ 'newLicenseConfiguration' smart constructor.
data LicenseConfiguration = LicenseConfiguration'
  { -- | Status of the license configuration.
    status :: Prelude.Maybe Prelude.Text,
    -- | Account ID of the license configuration\'s owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | Summaries for licenses consumed by various resources.
    consumedLicenseSummaryList :: Prelude.Maybe [ConsumedLicenseSummary],
    -- | Number of licenses managed by the license configuration.
    licenseCount :: Prelude.Maybe Prelude.Integer,
    -- | Summaries for managed resources.
    managedResourceSummaryList :: Prelude.Maybe [ManagedResourceSummary],
    -- | Name of the license configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | Number of available licenses as a hard limit.
    licenseCountHardLimit :: Prelude.Maybe Prelude.Bool,
    -- | When true, disassociates a resource when software is uninstalled.
    disassociateWhenNotFound :: Prelude.Maybe Prelude.Bool,
    -- | Product information.
    productInformationList :: Prelude.Maybe [ProductInformation],
    -- | Dimension to use to track the license inventory.
    licenseCountingType :: Prelude.Maybe LicenseCountingType,
    -- | Automated discovery information.
    automatedDiscoveryInformation :: Prelude.Maybe AutomatedDiscoveryInformation,
    -- | Number of licenses consumed.
    consumedLicenses :: Prelude.Maybe Prelude.Integer,
    -- | License rules.
    licenseRules :: Prelude.Maybe [Prelude.Text],
    -- | Unique ID of the license configuration.
    licenseConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | Description of the license configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'licenseConfiguration_status' - Status of the license configuration.
--
-- 'ownerAccountId', 'licenseConfiguration_ownerAccountId' - Account ID of the license configuration\'s owner.
--
-- 'consumedLicenseSummaryList', 'licenseConfiguration_consumedLicenseSummaryList' - Summaries for licenses consumed by various resources.
--
-- 'licenseCount', 'licenseConfiguration_licenseCount' - Number of licenses managed by the license configuration.
--
-- 'managedResourceSummaryList', 'licenseConfiguration_managedResourceSummaryList' - Summaries for managed resources.
--
-- 'name', 'licenseConfiguration_name' - Name of the license configuration.
--
-- 'licenseCountHardLimit', 'licenseConfiguration_licenseCountHardLimit' - Number of available licenses as a hard limit.
--
-- 'disassociateWhenNotFound', 'licenseConfiguration_disassociateWhenNotFound' - When true, disassociates a resource when software is uninstalled.
--
-- 'productInformationList', 'licenseConfiguration_productInformationList' - Product information.
--
-- 'licenseCountingType', 'licenseConfiguration_licenseCountingType' - Dimension to use to track the license inventory.
--
-- 'automatedDiscoveryInformation', 'licenseConfiguration_automatedDiscoveryInformation' - Automated discovery information.
--
-- 'consumedLicenses', 'licenseConfiguration_consumedLicenses' - Number of licenses consumed.
--
-- 'licenseRules', 'licenseConfiguration_licenseRules' - License rules.
--
-- 'licenseConfigurationId', 'licenseConfiguration_licenseConfigurationId' - Unique ID of the license configuration.
--
-- 'description', 'licenseConfiguration_description' - Description of the license configuration.
--
-- 'licenseConfigurationArn', 'licenseConfiguration_licenseConfigurationArn' - Amazon Resource Name (ARN) of the license configuration.
newLicenseConfiguration ::
  LicenseConfiguration
newLicenseConfiguration =
  LicenseConfiguration'
    { status = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      consumedLicenseSummaryList = Prelude.Nothing,
      licenseCount = Prelude.Nothing,
      managedResourceSummaryList = Prelude.Nothing,
      name = Prelude.Nothing,
      licenseCountHardLimit = Prelude.Nothing,
      disassociateWhenNotFound = Prelude.Nothing,
      productInformationList = Prelude.Nothing,
      licenseCountingType = Prelude.Nothing,
      automatedDiscoveryInformation = Prelude.Nothing,
      consumedLicenses = Prelude.Nothing,
      licenseRules = Prelude.Nothing,
      licenseConfigurationId = Prelude.Nothing,
      description = Prelude.Nothing,
      licenseConfigurationArn = Prelude.Nothing
    }

-- | Status of the license configuration.
licenseConfiguration_status :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Text)
licenseConfiguration_status = Lens.lens (\LicenseConfiguration' {status} -> status) (\s@LicenseConfiguration' {} a -> s {status = a} :: LicenseConfiguration)

-- | Account ID of the license configuration\'s owner.
licenseConfiguration_ownerAccountId :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Text)
licenseConfiguration_ownerAccountId = Lens.lens (\LicenseConfiguration' {ownerAccountId} -> ownerAccountId) (\s@LicenseConfiguration' {} a -> s {ownerAccountId = a} :: LicenseConfiguration)

-- | Summaries for licenses consumed by various resources.
licenseConfiguration_consumedLicenseSummaryList :: Lens.Lens' LicenseConfiguration (Prelude.Maybe [ConsumedLicenseSummary])
licenseConfiguration_consumedLicenseSummaryList = Lens.lens (\LicenseConfiguration' {consumedLicenseSummaryList} -> consumedLicenseSummaryList) (\s@LicenseConfiguration' {} a -> s {consumedLicenseSummaryList = a} :: LicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Number of licenses managed by the license configuration.
licenseConfiguration_licenseCount :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Integer)
licenseConfiguration_licenseCount = Lens.lens (\LicenseConfiguration' {licenseCount} -> licenseCount) (\s@LicenseConfiguration' {} a -> s {licenseCount = a} :: LicenseConfiguration)

-- | Summaries for managed resources.
licenseConfiguration_managedResourceSummaryList :: Lens.Lens' LicenseConfiguration (Prelude.Maybe [ManagedResourceSummary])
licenseConfiguration_managedResourceSummaryList = Lens.lens (\LicenseConfiguration' {managedResourceSummaryList} -> managedResourceSummaryList) (\s@LicenseConfiguration' {} a -> s {managedResourceSummaryList = a} :: LicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Name of the license configuration.
licenseConfiguration_name :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Text)
licenseConfiguration_name = Lens.lens (\LicenseConfiguration' {name} -> name) (\s@LicenseConfiguration' {} a -> s {name = a} :: LicenseConfiguration)

-- | Number of available licenses as a hard limit.
licenseConfiguration_licenseCountHardLimit :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Bool)
licenseConfiguration_licenseCountHardLimit = Lens.lens (\LicenseConfiguration' {licenseCountHardLimit} -> licenseCountHardLimit) (\s@LicenseConfiguration' {} a -> s {licenseCountHardLimit = a} :: LicenseConfiguration)

-- | When true, disassociates a resource when software is uninstalled.
licenseConfiguration_disassociateWhenNotFound :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Bool)
licenseConfiguration_disassociateWhenNotFound = Lens.lens (\LicenseConfiguration' {disassociateWhenNotFound} -> disassociateWhenNotFound) (\s@LicenseConfiguration' {} a -> s {disassociateWhenNotFound = a} :: LicenseConfiguration)

-- | Product information.
licenseConfiguration_productInformationList :: Lens.Lens' LicenseConfiguration (Prelude.Maybe [ProductInformation])
licenseConfiguration_productInformationList = Lens.lens (\LicenseConfiguration' {productInformationList} -> productInformationList) (\s@LicenseConfiguration' {} a -> s {productInformationList = a} :: LicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Dimension to use to track the license inventory.
licenseConfiguration_licenseCountingType :: Lens.Lens' LicenseConfiguration (Prelude.Maybe LicenseCountingType)
licenseConfiguration_licenseCountingType = Lens.lens (\LicenseConfiguration' {licenseCountingType} -> licenseCountingType) (\s@LicenseConfiguration' {} a -> s {licenseCountingType = a} :: LicenseConfiguration)

-- | Automated discovery information.
licenseConfiguration_automatedDiscoveryInformation :: Lens.Lens' LicenseConfiguration (Prelude.Maybe AutomatedDiscoveryInformation)
licenseConfiguration_automatedDiscoveryInformation = Lens.lens (\LicenseConfiguration' {automatedDiscoveryInformation} -> automatedDiscoveryInformation) (\s@LicenseConfiguration' {} a -> s {automatedDiscoveryInformation = a} :: LicenseConfiguration)

-- | Number of licenses consumed.
licenseConfiguration_consumedLicenses :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Integer)
licenseConfiguration_consumedLicenses = Lens.lens (\LicenseConfiguration' {consumedLicenses} -> consumedLicenses) (\s@LicenseConfiguration' {} a -> s {consumedLicenses = a} :: LicenseConfiguration)

-- | License rules.
licenseConfiguration_licenseRules :: Lens.Lens' LicenseConfiguration (Prelude.Maybe [Prelude.Text])
licenseConfiguration_licenseRules = Lens.lens (\LicenseConfiguration' {licenseRules} -> licenseRules) (\s@LicenseConfiguration' {} a -> s {licenseRules = a} :: LicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Unique ID of the license configuration.
licenseConfiguration_licenseConfigurationId :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Text)
licenseConfiguration_licenseConfigurationId = Lens.lens (\LicenseConfiguration' {licenseConfigurationId} -> licenseConfigurationId) (\s@LicenseConfiguration' {} a -> s {licenseConfigurationId = a} :: LicenseConfiguration)

-- | Description of the license configuration.
licenseConfiguration_description :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Text)
licenseConfiguration_description = Lens.lens (\LicenseConfiguration' {description} -> description) (\s@LicenseConfiguration' {} a -> s {description = a} :: LicenseConfiguration)

-- | Amazon Resource Name (ARN) of the license configuration.
licenseConfiguration_licenseConfigurationArn :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Text)
licenseConfiguration_licenseConfigurationArn = Lens.lens (\LicenseConfiguration' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@LicenseConfiguration' {} a -> s {licenseConfigurationArn = a} :: LicenseConfiguration)

instance Core.FromJSON LicenseConfiguration where
  parseJSON =
    Core.withObject
      "LicenseConfiguration"
      ( \x ->
          LicenseConfiguration'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "OwnerAccountId")
            Prelude.<*> ( x Core..:? "ConsumedLicenseSummaryList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LicenseCount")
            Prelude.<*> ( x Core..:? "ManagedResourceSummaryList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LicenseCountHardLimit")
            Prelude.<*> (x Core..:? "DisassociateWhenNotFound")
            Prelude.<*> ( x Core..:? "ProductInformationList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LicenseCountingType")
            Prelude.<*> (x Core..:? "AutomatedDiscoveryInformation")
            Prelude.<*> (x Core..:? "ConsumedLicenses")
            Prelude.<*> (x Core..:? "LicenseRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LicenseConfigurationId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LicenseConfigurationArn")
      )

instance Prelude.Hashable LicenseConfiguration where
  hashWithSalt _salt LicenseConfiguration' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` consumedLicenseSummaryList
      `Prelude.hashWithSalt` licenseCount
      `Prelude.hashWithSalt` managedResourceSummaryList
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` licenseCountHardLimit
      `Prelude.hashWithSalt` disassociateWhenNotFound
      `Prelude.hashWithSalt` productInformationList
      `Prelude.hashWithSalt` licenseCountingType
      `Prelude.hashWithSalt` automatedDiscoveryInformation
      `Prelude.hashWithSalt` consumedLicenses
      `Prelude.hashWithSalt` licenseRules
      `Prelude.hashWithSalt` licenseConfigurationId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` licenseConfigurationArn

instance Prelude.NFData LicenseConfiguration where
  rnf LicenseConfiguration' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf consumedLicenseSummaryList
      `Prelude.seq` Prelude.rnf licenseCount
      `Prelude.seq` Prelude.rnf managedResourceSummaryList
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf licenseCountHardLimit
      `Prelude.seq` Prelude.rnf disassociateWhenNotFound
      `Prelude.seq` Prelude.rnf productInformationList
      `Prelude.seq` Prelude.rnf licenseCountingType
      `Prelude.seq` Prelude.rnf automatedDiscoveryInformation
      `Prelude.seq` Prelude.rnf consumedLicenses
      `Prelude.seq` Prelude.rnf licenseRules
      `Prelude.seq` Prelude.rnf licenseConfigurationId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf licenseConfigurationArn
