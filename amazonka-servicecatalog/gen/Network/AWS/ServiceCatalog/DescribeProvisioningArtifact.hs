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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioning artifact (also known
-- as a version) for the specified product.
module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
  ( -- * Creating a Request
    DescribeProvisioningArtifact (..),
    newDescribeProvisioningArtifact,

    -- * Request Lenses
    describeProvisioningArtifact_provisioningArtifactName,
    describeProvisioningArtifact_provisioningArtifactId,
    describeProvisioningArtifact_productName,
    describeProvisioningArtifact_productId,
    describeProvisioningArtifact_verbose,
    describeProvisioningArtifact_acceptLanguage,

    -- * Destructuring the Response
    DescribeProvisioningArtifactResponse (..),
    newDescribeProvisioningArtifactResponse,

    -- * Response Lenses
    describeProvisioningArtifactResponse_status,
    describeProvisioningArtifactResponse_info,
    describeProvisioningArtifactResponse_provisioningArtifactDetail,
    describeProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeProvisioningArtifact' smart constructor.
data DescribeProvisioningArtifact = DescribeProvisioningArtifact'
  { -- | The provisioning artifact name.
    provisioningArtifactName :: Core.Maybe Core.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The product name.
    productName :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Maybe Core.Text,
    -- | Indicates whether a verbose level of detail is enabled.
    verbose :: Core.Maybe Core.Bool,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifactName', 'describeProvisioningArtifact_provisioningArtifactName' - The provisioning artifact name.
--
-- 'provisioningArtifactId', 'describeProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'productName', 'describeProvisioningArtifact_productName' - The product name.
--
-- 'productId', 'describeProvisioningArtifact_productId' - The product identifier.
--
-- 'verbose', 'describeProvisioningArtifact_verbose' - Indicates whether a verbose level of detail is enabled.
--
-- 'acceptLanguage', 'describeProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newDescribeProvisioningArtifact ::
  DescribeProvisioningArtifact
newDescribeProvisioningArtifact =
  DescribeProvisioningArtifact'
    { provisioningArtifactName =
        Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      productName = Core.Nothing,
      productId = Core.Nothing,
      verbose = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The provisioning artifact name.
describeProvisioningArtifact_provisioningArtifactName :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Core.Text)
describeProvisioningArtifact_provisioningArtifactName = Lens.lens (\DescribeProvisioningArtifact' {provisioningArtifactName} -> provisioningArtifactName) (\s@DescribeProvisioningArtifact' {} a -> s {provisioningArtifactName = a} :: DescribeProvisioningArtifact)

-- | The identifier of the provisioning artifact.
describeProvisioningArtifact_provisioningArtifactId :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Core.Text)
describeProvisioningArtifact_provisioningArtifactId = Lens.lens (\DescribeProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@DescribeProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: DescribeProvisioningArtifact)

-- | The product name.
describeProvisioningArtifact_productName :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Core.Text)
describeProvisioningArtifact_productName = Lens.lens (\DescribeProvisioningArtifact' {productName} -> productName) (\s@DescribeProvisioningArtifact' {} a -> s {productName = a} :: DescribeProvisioningArtifact)

-- | The product identifier.
describeProvisioningArtifact_productId :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Core.Text)
describeProvisioningArtifact_productId = Lens.lens (\DescribeProvisioningArtifact' {productId} -> productId) (\s@DescribeProvisioningArtifact' {} a -> s {productId = a} :: DescribeProvisioningArtifact)

-- | Indicates whether a verbose level of detail is enabled.
describeProvisioningArtifact_verbose :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Core.Bool)
describeProvisioningArtifact_verbose = Lens.lens (\DescribeProvisioningArtifact' {verbose} -> verbose) (\s@DescribeProvisioningArtifact' {} a -> s {verbose = a} :: DescribeProvisioningArtifact)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisioningArtifact_acceptLanguage :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Core.Text)
describeProvisioningArtifact_acceptLanguage = Lens.lens (\DescribeProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisioningArtifact' {} a -> s {acceptLanguage = a} :: DescribeProvisioningArtifact)

instance Core.AWSRequest DescribeProvisioningArtifact where
  type
    AWSResponse DescribeProvisioningArtifact =
      DescribeProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningArtifactResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "Info" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ProvisioningArtifactDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProvisioningArtifact

instance Core.NFData DescribeProvisioningArtifact

instance Core.ToHeaders DescribeProvisioningArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProvisioningArtifact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProvisioningArtifact where
  toJSON DescribeProvisioningArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProvisioningArtifactName" Core..=)
              Core.<$> provisioningArtifactName,
            ("ProvisioningArtifactId" Core..=)
              Core.<$> provisioningArtifactId,
            ("ProductName" Core..=) Core.<$> productName,
            ("ProductId" Core..=) Core.<$> productId,
            ("Verbose" Core..=) Core.<$> verbose,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath DescribeProvisioningArtifact where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProvisioningArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProvisioningArtifactResponse' smart constructor.
data DescribeProvisioningArtifactResponse = DescribeProvisioningArtifactResponse'
  { -- | The status of the current request.
    status :: Core.Maybe RequestStatus,
    -- | The URL of the CloudFormation template in Amazon S3.
    info :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Core.Maybe ProvisioningArtifactDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeProvisioningArtifactResponse_status' - The status of the current request.
--
-- 'info', 'describeProvisioningArtifactResponse_info' - The URL of the CloudFormation template in Amazon S3.
--
-- 'provisioningArtifactDetail', 'describeProvisioningArtifactResponse_provisioningArtifactDetail' - Information about the provisioning artifact.
--
-- 'httpStatus', 'describeProvisioningArtifactResponse_httpStatus' - The response's http status code.
newDescribeProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProvisioningArtifactResponse
newDescribeProvisioningArtifactResponse pHttpStatus_ =
  DescribeProvisioningArtifactResponse'
    { status =
        Core.Nothing,
      info = Core.Nothing,
      provisioningArtifactDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the current request.
describeProvisioningArtifactResponse_status :: Lens.Lens' DescribeProvisioningArtifactResponse (Core.Maybe RequestStatus)
describeProvisioningArtifactResponse_status = Lens.lens (\DescribeProvisioningArtifactResponse' {status} -> status) (\s@DescribeProvisioningArtifactResponse' {} a -> s {status = a} :: DescribeProvisioningArtifactResponse)

-- | The URL of the CloudFormation template in Amazon S3.
describeProvisioningArtifactResponse_info :: Lens.Lens' DescribeProvisioningArtifactResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeProvisioningArtifactResponse_info = Lens.lens (\DescribeProvisioningArtifactResponse' {info} -> info) (\s@DescribeProvisioningArtifactResponse' {} a -> s {info = a} :: DescribeProvisioningArtifactResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the provisioning artifact.
describeProvisioningArtifactResponse_provisioningArtifactDetail :: Lens.Lens' DescribeProvisioningArtifactResponse (Core.Maybe ProvisioningArtifactDetail)
describeProvisioningArtifactResponse_provisioningArtifactDetail = Lens.lens (\DescribeProvisioningArtifactResponse' {provisioningArtifactDetail} -> provisioningArtifactDetail) (\s@DescribeProvisioningArtifactResponse' {} a -> s {provisioningArtifactDetail = a} :: DescribeProvisioningArtifactResponse)

-- | The response's http status code.
describeProvisioningArtifactResponse_httpStatus :: Lens.Lens' DescribeProvisioningArtifactResponse Core.Int
describeProvisioningArtifactResponse_httpStatus = Lens.lens (\DescribeProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningArtifactResponse)

instance
  Core.NFData
    DescribeProvisioningArtifactResponse
