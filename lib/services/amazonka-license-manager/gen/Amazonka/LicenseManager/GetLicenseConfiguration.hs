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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getLicenseConfigurationResponse_automatedDiscoveryInformation,
    getLicenseConfigurationResponse_consumedLicenseSummaryList,
    getLicenseConfigurationResponse_consumedLicenses,
    getLicenseConfigurationResponse_description,
    getLicenseConfigurationResponse_disassociateWhenNotFound,
    getLicenseConfigurationResponse_licenseConfigurationArn,
    getLicenseConfigurationResponse_licenseConfigurationId,
    getLicenseConfigurationResponse_licenseCount,
    getLicenseConfigurationResponse_licenseCountHardLimit,
    getLicenseConfigurationResponse_licenseCountingType,
    getLicenseConfigurationResponse_licenseRules,
    getLicenseConfigurationResponse_managedResourceSummaryList,
    getLicenseConfigurationResponse_name,
    getLicenseConfigurationResponse_ownerAccountId,
    getLicenseConfigurationResponse_productInformationList,
    getLicenseConfigurationResponse_status,
    getLicenseConfigurationResponse_tags,
    getLicenseConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLicenseConfigurationResponse'
            Prelude.<$> (x Data..?> "AutomatedDiscoveryInformation")
            Prelude.<*> ( x Data..?> "ConsumedLicenseSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ConsumedLicenses")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "DisassociateWhenNotFound")
            Prelude.<*> (x Data..?> "LicenseConfigurationArn")
            Prelude.<*> (x Data..?> "LicenseConfigurationId")
            Prelude.<*> (x Data..?> "LicenseCount")
            Prelude.<*> (x Data..?> "LicenseCountHardLimit")
            Prelude.<*> (x Data..?> "LicenseCountingType")
            Prelude.<*> (x Data..?> "LicenseRules" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "ManagedResourceSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OwnerAccountId")
            Prelude.<*> ( x Data..?> "ProductInformationList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLicenseConfiguration where
  hashWithSalt _salt GetLicenseConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` licenseConfigurationArn

instance Prelude.NFData GetLicenseConfiguration where
  rnf GetLicenseConfiguration' {..} =
    Prelude.rnf licenseConfigurationArn

instance Data.ToHeaders GetLicenseConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.GetLicenseConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLicenseConfiguration where
  toJSON GetLicenseConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LicenseConfigurationArn"
                  Data..= licenseConfigurationArn
              )
          ]
      )

instance Data.ToPath GetLicenseConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLicenseConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLicenseConfigurationResponse' smart constructor.
data GetLicenseConfigurationResponse = GetLicenseConfigurationResponse'
  { -- | Automated discovery information.
    automatedDiscoveryInformation :: Prelude.Maybe AutomatedDiscoveryInformation,
    -- | Summaries of the licenses consumed by resources.
    consumedLicenseSummaryList :: Prelude.Maybe [ConsumedLicenseSummary],
    -- | Number of licenses assigned to resources.
    consumedLicenses :: Prelude.Maybe Prelude.Integer,
    -- | Description of the license configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | When true, disassociates a resource when software is uninstalled.
    disassociateWhenNotFound :: Prelude.Maybe Prelude.Bool,
    -- | Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Unique ID for the license configuration.
    licenseConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | Number of available licenses.
    licenseCount :: Prelude.Maybe Prelude.Integer,
    -- | Sets the number of available licenses as a hard limit.
    licenseCountHardLimit :: Prelude.Maybe Prelude.Bool,
    -- | Dimension for which the licenses are counted.
    licenseCountingType :: Prelude.Maybe LicenseCountingType,
    -- | License rules.
    licenseRules :: Prelude.Maybe [Prelude.Text],
    -- | Summaries of the managed resources.
    managedResourceSummaryList :: Prelude.Maybe [ManagedResourceSummary],
    -- | Name of the license configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | Account ID of the owner of the license configuration.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | Product information.
    productInformationList :: Prelude.Maybe [ProductInformation],
    -- | License configuration status.
    status :: Prelude.Maybe Prelude.Text,
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
-- 'automatedDiscoveryInformation', 'getLicenseConfigurationResponse_automatedDiscoveryInformation' - Automated discovery information.
--
-- 'consumedLicenseSummaryList', 'getLicenseConfigurationResponse_consumedLicenseSummaryList' - Summaries of the licenses consumed by resources.
--
-- 'consumedLicenses', 'getLicenseConfigurationResponse_consumedLicenses' - Number of licenses assigned to resources.
--
-- 'description', 'getLicenseConfigurationResponse_description' - Description of the license configuration.
--
-- 'disassociateWhenNotFound', 'getLicenseConfigurationResponse_disassociateWhenNotFound' - When true, disassociates a resource when software is uninstalled.
--
-- 'licenseConfigurationArn', 'getLicenseConfigurationResponse_licenseConfigurationArn' - Amazon Resource Name (ARN) of the license configuration.
--
-- 'licenseConfigurationId', 'getLicenseConfigurationResponse_licenseConfigurationId' - Unique ID for the license configuration.
--
-- 'licenseCount', 'getLicenseConfigurationResponse_licenseCount' - Number of available licenses.
--
-- 'licenseCountHardLimit', 'getLicenseConfigurationResponse_licenseCountHardLimit' - Sets the number of available licenses as a hard limit.
--
-- 'licenseCountingType', 'getLicenseConfigurationResponse_licenseCountingType' - Dimension for which the licenses are counted.
--
-- 'licenseRules', 'getLicenseConfigurationResponse_licenseRules' - License rules.
--
-- 'managedResourceSummaryList', 'getLicenseConfigurationResponse_managedResourceSummaryList' - Summaries of the managed resources.
--
-- 'name', 'getLicenseConfigurationResponse_name' - Name of the license configuration.
--
-- 'ownerAccountId', 'getLicenseConfigurationResponse_ownerAccountId' - Account ID of the owner of the license configuration.
--
-- 'productInformationList', 'getLicenseConfigurationResponse_productInformationList' - Product information.
--
-- 'status', 'getLicenseConfigurationResponse_status' - License configuration status.
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
    { automatedDiscoveryInformation =
        Prelude.Nothing,
      consumedLicenseSummaryList =
        Prelude.Nothing,
      consumedLicenses = Prelude.Nothing,
      description = Prelude.Nothing,
      disassociateWhenNotFound = Prelude.Nothing,
      licenseConfigurationArn = Prelude.Nothing,
      licenseConfigurationId = Prelude.Nothing,
      licenseCount = Prelude.Nothing,
      licenseCountHardLimit = Prelude.Nothing,
      licenseCountingType = Prelude.Nothing,
      licenseRules = Prelude.Nothing,
      managedResourceSummaryList =
        Prelude.Nothing,
      name = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      productInformationList = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Automated discovery information.
getLicenseConfigurationResponse_automatedDiscoveryInformation :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe AutomatedDiscoveryInformation)
getLicenseConfigurationResponse_automatedDiscoveryInformation = Lens.lens (\GetLicenseConfigurationResponse' {automatedDiscoveryInformation} -> automatedDiscoveryInformation) (\s@GetLicenseConfigurationResponse' {} a -> s {automatedDiscoveryInformation = a} :: GetLicenseConfigurationResponse)

-- | Summaries of the licenses consumed by resources.
getLicenseConfigurationResponse_consumedLicenseSummaryList :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [ConsumedLicenseSummary])
getLicenseConfigurationResponse_consumedLicenseSummaryList = Lens.lens (\GetLicenseConfigurationResponse' {consumedLicenseSummaryList} -> consumedLicenseSummaryList) (\s@GetLicenseConfigurationResponse' {} a -> s {consumedLicenseSummaryList = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Number of licenses assigned to resources.
getLicenseConfigurationResponse_consumedLicenses :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Integer)
getLicenseConfigurationResponse_consumedLicenses = Lens.lens (\GetLicenseConfigurationResponse' {consumedLicenses} -> consumedLicenses) (\s@GetLicenseConfigurationResponse' {} a -> s {consumedLicenses = a} :: GetLicenseConfigurationResponse)

-- | Description of the license configuration.
getLicenseConfigurationResponse_description :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_description = Lens.lens (\GetLicenseConfigurationResponse' {description} -> description) (\s@GetLicenseConfigurationResponse' {} a -> s {description = a} :: GetLicenseConfigurationResponse)

-- | When true, disassociates a resource when software is uninstalled.
getLicenseConfigurationResponse_disassociateWhenNotFound :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Bool)
getLicenseConfigurationResponse_disassociateWhenNotFound = Lens.lens (\GetLicenseConfigurationResponse' {disassociateWhenNotFound} -> disassociateWhenNotFound) (\s@GetLicenseConfigurationResponse' {} a -> s {disassociateWhenNotFound = a} :: GetLicenseConfigurationResponse)

-- | Amazon Resource Name (ARN) of the license configuration.
getLicenseConfigurationResponse_licenseConfigurationArn :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_licenseConfigurationArn = Lens.lens (\GetLicenseConfigurationResponse' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseConfigurationArn = a} :: GetLicenseConfigurationResponse)

-- | Unique ID for the license configuration.
getLicenseConfigurationResponse_licenseConfigurationId :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_licenseConfigurationId = Lens.lens (\GetLicenseConfigurationResponse' {licenseConfigurationId} -> licenseConfigurationId) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseConfigurationId = a} :: GetLicenseConfigurationResponse)

-- | Number of available licenses.
getLicenseConfigurationResponse_licenseCount :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Integer)
getLicenseConfigurationResponse_licenseCount = Lens.lens (\GetLicenseConfigurationResponse' {licenseCount} -> licenseCount) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseCount = a} :: GetLicenseConfigurationResponse)

-- | Sets the number of available licenses as a hard limit.
getLicenseConfigurationResponse_licenseCountHardLimit :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Bool)
getLicenseConfigurationResponse_licenseCountHardLimit = Lens.lens (\GetLicenseConfigurationResponse' {licenseCountHardLimit} -> licenseCountHardLimit) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseCountHardLimit = a} :: GetLicenseConfigurationResponse)

-- | Dimension for which the licenses are counted.
getLicenseConfigurationResponse_licenseCountingType :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe LicenseCountingType)
getLicenseConfigurationResponse_licenseCountingType = Lens.lens (\GetLicenseConfigurationResponse' {licenseCountingType} -> licenseCountingType) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseCountingType = a} :: GetLicenseConfigurationResponse)

-- | License rules.
getLicenseConfigurationResponse_licenseRules :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [Prelude.Text])
getLicenseConfigurationResponse_licenseRules = Lens.lens (\GetLicenseConfigurationResponse' {licenseRules} -> licenseRules) (\s@GetLicenseConfigurationResponse' {} a -> s {licenseRules = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Summaries of the managed resources.
getLicenseConfigurationResponse_managedResourceSummaryList :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [ManagedResourceSummary])
getLicenseConfigurationResponse_managedResourceSummaryList = Lens.lens (\GetLicenseConfigurationResponse' {managedResourceSummaryList} -> managedResourceSummaryList) (\s@GetLicenseConfigurationResponse' {} a -> s {managedResourceSummaryList = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Name of the license configuration.
getLicenseConfigurationResponse_name :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_name = Lens.lens (\GetLicenseConfigurationResponse' {name} -> name) (\s@GetLicenseConfigurationResponse' {} a -> s {name = a} :: GetLicenseConfigurationResponse)

-- | Account ID of the owner of the license configuration.
getLicenseConfigurationResponse_ownerAccountId :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_ownerAccountId = Lens.lens (\GetLicenseConfigurationResponse' {ownerAccountId} -> ownerAccountId) (\s@GetLicenseConfigurationResponse' {} a -> s {ownerAccountId = a} :: GetLicenseConfigurationResponse)

-- | Product information.
getLicenseConfigurationResponse_productInformationList :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe [ProductInformation])
getLicenseConfigurationResponse_productInformationList = Lens.lens (\GetLicenseConfigurationResponse' {productInformationList} -> productInformationList) (\s@GetLicenseConfigurationResponse' {} a -> s {productInformationList = a} :: GetLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | License configuration status.
getLicenseConfigurationResponse_status :: Lens.Lens' GetLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
getLicenseConfigurationResponse_status = Lens.lens (\GetLicenseConfigurationResponse' {status} -> status) (\s@GetLicenseConfigurationResponse' {} a -> s {status = a} :: GetLicenseConfigurationResponse)

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
    Prelude.rnf automatedDiscoveryInformation
      `Prelude.seq` Prelude.rnf consumedLicenseSummaryList
      `Prelude.seq` Prelude.rnf consumedLicenses
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disassociateWhenNotFound
      `Prelude.seq` Prelude.rnf licenseConfigurationArn
      `Prelude.seq` Prelude.rnf licenseConfigurationId
      `Prelude.seq` Prelude.rnf licenseCount
      `Prelude.seq` Prelude.rnf licenseCountHardLimit
      `Prelude.seq` Prelude.rnf licenseCountingType
      `Prelude.seq` Prelude.rnf licenseRules
      `Prelude.seq` Prelude.rnf managedResourceSummaryList
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf productInformationList
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
