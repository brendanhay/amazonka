{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManager.GetLicenseConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information about the specified license configuration.
module Amazonka.LicenseManager.GetLicenseConfiguration
  ( -- * Creating a Request
    GetLicenseConfiguration (..),
    newGetLicenseConfiguration,

    -- * Request Lenses
    getLicenseConfiguration_licenseConfigurationArn,

    -- * Destructuring the Response
    GetLicenseConfigurationResponse (..),
    newGetLicenseConfigurationResponse,

    -- * Response Lenses
    getLicenseConfigurationResponse_status,
    getLicenseConfigurationResponse_ownerAccountId,
    getLicenseConfigurationResponse_consumedLicenseSummaryList,
    getLicenseConfigurationResponse_licenseCount,
    getLicenseConfigurationResponse_managedResourceSummaryList,
    getLicenseConfigurationResponse_name,
    getLicenseConfigurationResponse_licenseCountHardLimit,
    getLicenseConfigurationResponse_disassociateWhenNotFound,
    getLicenseConfigurationResponse_productInformationList,
    getLicenseConfigurationResponse_licenseCountingType,
    getLicenseConfigurationResponse_automatedDiscoveryInformation,
    getLicenseConfigurationResponse_consumedLicenses,
    getLicenseConfigurationResponse_licenseRules,
    getLicenseConfigurationResponse_licenseConfigurationId,
    getLicenseConfigurationResponse_description,
    getLicenseConfigurationResponse_licenseConfigurationArn,
    getLicenseConfigurationResponse_tags,
    getLicenseConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLicenseConfiguration' smart constructor.
data GetLicenseConfiguration = GetLicenseConfiguration'
  { -- | Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'getLicenseConfiguration_licenseConfigurationArn' - Amazon Resource Name (ARN) of the license configuration.
newGetLicenseConfiguration ::
  -- | 'licenseConfigurationArn'
  Prelude.Text ->
  GetLicenseConfiguration
newGetLicenseConfiguration pLicenseConfigurationArn_ =
  GetLicenseConfiguration'
    { licenseConfigurationArn =
        pLicenseConfigurationArn_
    }

-- | Amazon Resource Name (ARN) of the license configuration.
getLicenseConfiguration_licenseConfigurationArn :: Lens.Lens' GetLicenseConfiguration Prelude.Text
getLicenseConfiguration_licenseConfigurationArn = Lens.lens (\GetLicenseConfiguration' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@GetLicenseConfiguration' {} a -> s {licenseConfigurationArn = a} :: GetLicenseConfiguration)

instance Core.AWSRequest GetLicenseConfiguration where
  type
    AWSResponse GetLicenseConfiguration =
      GetLicenseConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLicenseConfigurationResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "OwnerAccountId")
            Prelude.<*> ( x Core..?> "ConsumedLicenseSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "LicenseCount")
            Prelude.<*> ( x Core..?> "ManagedResourceSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LicenseCountHardLimit")
            Prelude.<*> (x Core..?> "DisassociateWhenNotFound")
            Prelude.<*> ( x Core..?> "ProductInformationList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "LicenseCountingType")
            Prelude.<*> (x Core..?> "AutomatedDiscoveryInformation")
            Prelude.<*> (x Core..?> "ConsumedLicenses")
            Prelude.<*> (x Core..?> "LicenseRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LicenseConfigurationId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "LicenseConfigurationArn")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLicenseConfiguration where
  hashWithSalt salt' GetLicenseConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` licenseConfigurationArn

instance Prelude.NFData GetLicenseConfiguration where
  rnf GetLicenseConfiguration' {..} =
    Prelude.rnf licenseConfigurationArn

instance Core.ToHeaders GetLicenseConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.GetLicenseConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLicenseConfiguration where
  toJSON GetLicenseConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LicenseConfigurationArn"
                  Core..= licenseConfigurationArn
              )
          ]
      )

instance Core.ToPath GetLicenseConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLicenseConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLicenseConfigurationResponse' smart constructor.
data GetLicenseConfigurationResponse = GetLicenseConfigurationResponse'
  { -- | License configuration status.
    status :: Prelude.Maybe Prelude.Text,
    -- | Account ID of the owner of the license configuration.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | Summaries of the licenses consumed by resources.
    consumedLicenseSummaryList :: Prelude.Maybe [ConsumedLicenseSummary],
    -- | Number of available licenses.
    licenseCount :: Prelude.Maybe Prelude.Integer,
    -- | Summaries of the managed resources.
    managedResourceSummaryList :: Prelude.Maybe [ManagedResourceSummary],
    -- | Name of the license configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | Sets the number of available licenses as a hard limit.
    licenseCountHardLimit :: Prelude.Maybe Prelude.Bool,
    -- | When true, disassociates a resource when software is uninstalled.
    disassociateWhenNotFound :: Prelude.Maybe Prelude.Bool,
    -- | Product information.
    productInformationList :: Prelude.Maybe [ProductInformation],
    -- | Dimension for which the licenses are counted.
    licenseCountingType :: Prelude.Maybe LicenseCountingType,
    -- | Automated discovery information.
    automatedDiscoveryInformation :: Prelude.Maybe AutomatedDiscoveryInformation,
    -- | Number of licenses assigned to resources.
    consumedLicenses :: Prelude.Maybe Prelude.Integer,
    -- | License rules.
    licenseRules :: Prelude.Maybe [Prelude.Text],
    -- | Unique ID for the license configuration.
    licenseConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | Description of the license configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Tags for the license configuration.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getLicenseConfigurationResponse_status' - License configuration status.
--
-- 'ownerAccountId', 'getLicenseConfigurationResponse_ownerAccountId' - Account ID of the owner of the license configuration.
--
-- 'consumedLicenseSummaryList', 'getLicenseConfigurationResponse_consumedLicenseSummaryList' - Summaries of the licenses consumed by resources.
--
-- 'licenseCount', 'getLicenseConfigurationResponse_licenseCount' - Number of available licenses.
--
-- 'managedResourceSummaryList', 'getLicenseConfigurationResponse_managedResourceSummaryList' - Summaries of the managed resources.
--
-- 'name', 'getLicenseConfigurationResponse_name' - Name of the license configuration.
--
-- 'licenseCountHardLimit', 'getLicenseConfigurationResponse_licenseCountHardLimit' - Sets the number of available licenses as a hard limit.
--
-- 'disassociateWhenNotFound', 'getLicenseConfigurationResponse_disassociateWhenNotFound' - When true, disassociates a resource when software is uninstalled.
--
-- 'productInformationList', 'getLicenseConfigurationResponse_productInformationList' - Product information.
--
-- 'licenseCountingType', 'getLicenseConfigurationResponse_licenseCountingType' - Dimension for which the licenses are counted.
--
-- 'automatedDiscoveryInformation', 'getLicenseConfigurationResponse_automatedDiscoveryInformation' - Automated discovery information.
--
-- 'consumedLicenses', 'getLicenseConfigurationResponse_consumedLicenses' - Number of licenses assigned to resources.
--
-- 'licenseRules', 'getLicenseConfigurationResponse_licenseRules' - License rules.
--
-- 'licenseConfigurationId', 'getLicenseConfigurationResponse_licenseConfigurationId' - Unique ID for the license configuration.
--
-- 'description', 'getLicenseConfigurationResponse_description' - Description of the license configuration.
--
-- 'licenseConfigurationArn', 'getLicenseConfigurationResponse_licenseConfigurationArn' - Amazon Resource Name (ARN) of the license configuration.
--
-- 'tags', 'getLicenseConfigurationResponse_tags' - Tags for the license configuration.
--
-- 'httpStatus', 'getLicenseConfigurationResponse_httpStatus' - The response's http status code.
newGetLicenseConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLicenseConfigurationResponse
newGetLicenseConfigurationResponse pHttpStatus_ =
  GetLicenseConfigurationResponse'
    { status =
        Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      consumedLicenseSummaryList =
        Prelude.Nothing,
      licenseCount = Prelude.Nothing,
      managedResourceSummaryList =
        Prelude.Nothing,
      name = Prelude.Nothing,
      licenseCountHardLimit = Prelude.Nothing,
      disassociateWhenNotFound = Prelude.Nothing,
      productInformationList = Prelude.Nothing,
      licenseCountingType = Prelude.Nothing,
      automatedDiscoveryInformation =
        Prelude.Nothing,
      consumedLicenses = Prelude.Nothing,
      licenseRules = Prelude.Nothing,
      licenseConfigurationId = Prelude.Nothing,
      description = Prelude.Nothing,
      licenseConfigurationArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | License configuration status.
getLicenseConfigurationResponse_status :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_status = Lens.lens (\GetLicenseConfigurationResponse' {status} -> status) (\s@GetLicenseConfigurationResponse' {} a -> s {status = a} :: GetLicenseConfigurationResponse)

-- | Account ID of the owner of the license configuration.
getLicenseConfigurationResponse_ownerAccountId :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_ownerAccountId = Lens.lens (\GetLicenseConfigurationResponse' {ownerAccountId} -> ownerAccountId) (\s@GetLicenseConfigurationResponse' {} a -> s {ownerAccountId = a} :: GetLicenseConfigurationResponse)

-- | Summaries of the licenses consumed by resources.
getLicenseConfigurationResponse_consumedLicenseSummaryList :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [ConsumedLicenseSummary])
getLicenseConfigurationResponse_consumedLicenseSummaryList = Lens.lens (\GetLicenseConfigurationResponse' {consumedLicenseSummaryList} -> consumedLicenseSummaryList) (\s@GetLicenseConfigurationResponse' {} a -> s {consumedLicenseSummaryList = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Number of available licenses.
getLicenseConfigurationResponse_licenseCount :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Integer)
getLicenseConfigurationResponse_licenseCount = Lens.lens (\GetLicenseConfigurationResponse' {licenseCount} -> licenseCount) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseCount = a} :: GetLicenseConfigurationResponse)

-- | Summaries of the managed resources.
getLicenseConfigurationResponse_managedResourceSummaryList :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [ManagedResourceSummary])
getLicenseConfigurationResponse_managedResourceSummaryList = Lens.lens (\GetLicenseConfigurationResponse' {managedResourceSummaryList} -> managedResourceSummaryList) (\s@GetLicenseConfigurationResponse' {} a -> s {managedResourceSummaryList = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Name of the license configuration.
getLicenseConfigurationResponse_name :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_name = Lens.lens (\GetLicenseConfigurationResponse' {name} -> name) (\s@GetLicenseConfigurationResponse' {} a -> s {name = a} :: GetLicenseConfigurationResponse)

-- | Sets the number of available licenses as a hard limit.
getLicenseConfigurationResponse_licenseCountHardLimit :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Bool)
getLicenseConfigurationResponse_licenseCountHardLimit = Lens.lens (\GetLicenseConfigurationResponse' {licenseCountHardLimit} -> licenseCountHardLimit) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseCountHardLimit = a} :: GetLicenseConfigurationResponse)

-- | When true, disassociates a resource when software is uninstalled.
getLicenseConfigurationResponse_disassociateWhenNotFound :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Bool)
getLicenseConfigurationResponse_disassociateWhenNotFound = Lens.lens (\GetLicenseConfigurationResponse' {disassociateWhenNotFound} -> disassociateWhenNotFound) (\s@GetLicenseConfigurationResponse' {} a -> s {disassociateWhenNotFound = a} :: GetLicenseConfigurationResponse)

-- | Product information.
getLicenseConfigurationResponse_productInformationList :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [ProductInformation])
getLicenseConfigurationResponse_productInformationList = Lens.lens (\GetLicenseConfigurationResponse' {productInformationList} -> productInformationList) (\s@GetLicenseConfigurationResponse' {} a -> s {productInformationList = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Dimension for which the licenses are counted.
getLicenseConfigurationResponse_licenseCountingType :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe LicenseCountingType)
getLicenseConfigurationResponse_licenseCountingType = Lens.lens (\GetLicenseConfigurationResponse' {licenseCountingType} -> licenseCountingType) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseCountingType = a} :: GetLicenseConfigurationResponse)

-- | Automated discovery information.
getLicenseConfigurationResponse_automatedDiscoveryInformation :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe AutomatedDiscoveryInformation)
getLicenseConfigurationResponse_automatedDiscoveryInformation = Lens.lens (\GetLicenseConfigurationResponse' {automatedDiscoveryInformation} -> automatedDiscoveryInformation) (\s@GetLicenseConfigurationResponse' {} a -> s {automatedDiscoveryInformation = a} :: GetLicenseConfigurationResponse)

-- | Number of licenses assigned to resources.
getLicenseConfigurationResponse_consumedLicenses :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Integer)
getLicenseConfigurationResponse_consumedLicenses = Lens.lens (\GetLicenseConfigurationResponse' {consumedLicenses} -> consumedLicenses) (\s@GetLicenseConfigurationResponse' {} a -> s {consumedLicenses = a} :: GetLicenseConfigurationResponse)

-- | License rules.
getLicenseConfigurationResponse_licenseRules :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [Prelude.Text])
getLicenseConfigurationResponse_licenseRules = Lens.lens (\GetLicenseConfigurationResponse' {licenseRules} -> licenseRules) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseRules = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Unique ID for the license configuration.
getLicenseConfigurationResponse_licenseConfigurationId :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_licenseConfigurationId = Lens.lens (\GetLicenseConfigurationResponse' {licenseConfigurationId} -> licenseConfigurationId) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseConfigurationId = a} :: GetLicenseConfigurationResponse)

-- | Description of the license configuration.
getLicenseConfigurationResponse_description :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_description = Lens.lens (\GetLicenseConfigurationResponse' {description} -> description) (\s@GetLicenseConfigurationResponse' {} a -> s {description = a} :: GetLicenseConfigurationResponse)

-- | Amazon Resource Name (ARN) of the license configuration.
getLicenseConfigurationResponse_licenseConfigurationArn :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_licenseConfigurationArn = Lens.lens (\GetLicenseConfigurationResponse' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseConfigurationArn = a} :: GetLicenseConfigurationResponse)

-- | Tags for the license configuration.
getLicenseConfigurationResponse_tags :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [Tag])
getLicenseConfigurationResponse_tags = Lens.lens (\GetLicenseConfigurationResponse' {tags} -> tags) (\s@GetLicenseConfigurationResponse' {} a -> s {tags = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLicenseConfigurationResponse_httpStatus :: Lens.Lens' GetLicenseConfigurationResponse Prelude.Int
getLicenseConfigurationResponse_httpStatus = Lens.lens (\GetLicenseConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetLicenseConfigurationResponse' {} a -> s {httpStatus = a} :: GetLicenseConfigurationResponse)

instance
  Prelude.NFData
    GetLicenseConfigurationResponse
  where
  rnf GetLicenseConfigurationResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf licenseConfigurationArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf licenseConfigurationId
      `Prelude.seq` Prelude.rnf licenseRules
      `Prelude.seq` Prelude.rnf consumedLicenses
      `Prelude.seq` Prelude.rnf automatedDiscoveryInformation
      `Prelude.seq` Prelude.rnf licenseCountingType
      `Prelude.seq` Prelude.rnf productInformationList
      `Prelude.seq` Prelude.rnf disassociateWhenNotFound
      `Prelude.seq` Prelude.rnf licenseCountHardLimit
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf managedResourceSummaryList
      `Prelude.seq` Prelude.rnf licenseCount
      `Prelude.seq` Prelude.rnf consumedLicenseSummaryList
      `Prelude.seq` Prelude.rnf ownerAccountId
