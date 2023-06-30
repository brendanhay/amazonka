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
-- Module      : Amazonka.ServiceCatalog.DescribeProvisioningArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioning artifact (also known
-- as a version) for the specified product.
module Amazonka.ServiceCatalog.DescribeProvisioningArtifact
  ( -- * Creating a Request
    DescribeProvisioningArtifact (..),
    newDescribeProvisioningArtifact,

    -- * Request Lenses
    describeProvisioningArtifact_acceptLanguage,
    describeProvisioningArtifact_productId,
    describeProvisioningArtifact_productName,
    describeProvisioningArtifact_provisioningArtifactId,
    describeProvisioningArtifact_provisioningArtifactName,
    describeProvisioningArtifact_verbose,

    -- * Destructuring the Response
    DescribeProvisioningArtifactResponse (..),
    newDescribeProvisioningArtifactResponse,

    -- * Response Lenses
    describeProvisioningArtifactResponse_info,
    describeProvisioningArtifactResponse_provisioningArtifactDetail,
    describeProvisioningArtifactResponse_status,
    describeProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeProvisioningArtifact' smart constructor.
data DescribeProvisioningArtifact = DescribeProvisioningArtifact'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The product name.
    productName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The provisioning artifact name.
    provisioningArtifactName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a verbose level of detail is enabled.
    verbose :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'describeProvisioningArtifact_productId' - The product identifier.
--
-- 'productName', 'describeProvisioningArtifact_productName' - The product name.
--
-- 'provisioningArtifactId', 'describeProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'provisioningArtifactName', 'describeProvisioningArtifact_provisioningArtifactName' - The provisioning artifact name.
--
-- 'verbose', 'describeProvisioningArtifact_verbose' - Indicates whether a verbose level of detail is enabled.
newDescribeProvisioningArtifact ::
  DescribeProvisioningArtifact
newDescribeProvisioningArtifact =
  DescribeProvisioningArtifact'
    { acceptLanguage =
        Prelude.Nothing,
      productId = Prelude.Nothing,
      productName = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      provisioningArtifactName = Prelude.Nothing,
      verbose = Prelude.Nothing
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisioningArtifact_acceptLanguage :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_acceptLanguage = Lens.lens (\DescribeProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisioningArtifact' {} a -> s {acceptLanguage = a} :: DescribeProvisioningArtifact)

-- | The product identifier.
describeProvisioningArtifact_productId :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_productId = Lens.lens (\DescribeProvisioningArtifact' {productId} -> productId) (\s@DescribeProvisioningArtifact' {} a -> s {productId = a} :: DescribeProvisioningArtifact)

-- | The product name.
describeProvisioningArtifact_productName :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_productName = Lens.lens (\DescribeProvisioningArtifact' {productName} -> productName) (\s@DescribeProvisioningArtifact' {} a -> s {productName = a} :: DescribeProvisioningArtifact)

-- | The identifier of the provisioning artifact.
describeProvisioningArtifact_provisioningArtifactId :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_provisioningArtifactId = Lens.lens (\DescribeProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@DescribeProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: DescribeProvisioningArtifact)

-- | The provisioning artifact name.
describeProvisioningArtifact_provisioningArtifactName :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_provisioningArtifactName = Lens.lens (\DescribeProvisioningArtifact' {provisioningArtifactName} -> provisioningArtifactName) (\s@DescribeProvisioningArtifact' {} a -> s {provisioningArtifactName = a} :: DescribeProvisioningArtifact)

-- | Indicates whether a verbose level of detail is enabled.
describeProvisioningArtifact_verbose :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Bool)
describeProvisioningArtifact_verbose = Lens.lens (\DescribeProvisioningArtifact' {verbose} -> verbose) (\s@DescribeProvisioningArtifact' {} a -> s {verbose = a} :: DescribeProvisioningArtifact)

instance Core.AWSRequest DescribeProvisioningArtifact where
  type
    AWSResponse DescribeProvisioningArtifact =
      DescribeProvisioningArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningArtifactResponse'
            Prelude.<$> (x Data..?> "Info" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ProvisioningArtifactDetail")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisioningArtifact
  where
  hashWithSalt _salt DescribeProvisioningArtifact' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` provisioningArtifactName
      `Prelude.hashWithSalt` verbose

instance Prelude.NFData DescribeProvisioningArtifact where
  rnf DescribeProvisioningArtifact' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf provisioningArtifactName
      `Prelude.seq` Prelude.rnf verbose

instance Data.ToHeaders DescribeProvisioningArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeProvisioningArtifact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProvisioningArtifact where
  toJSON DescribeProvisioningArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("ProductId" Data..=) Prelude.<$> productId,
            ("ProductName" Data..=) Prelude.<$> productName,
            ("ProvisioningArtifactId" Data..=)
              Prelude.<$> provisioningArtifactId,
            ("ProvisioningArtifactName" Data..=)
              Prelude.<$> provisioningArtifactName,
            ("Verbose" Data..=) Prelude.<$> verbose
          ]
      )

instance Data.ToPath DescribeProvisioningArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProvisioningArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisioningArtifactResponse' smart constructor.
data DescribeProvisioningArtifactResponse = DescribeProvisioningArtifactResponse'
  { -- | The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
    -- format.
    info :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Prelude.Maybe ProvisioningArtifactDetail,
    -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'info', 'describeProvisioningArtifactResponse_info' - The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
-- format.
--
-- 'provisioningArtifactDetail', 'describeProvisioningArtifactResponse_provisioningArtifactDetail' - Information about the provisioning artifact.
--
-- 'status', 'describeProvisioningArtifactResponse_status' - The status of the current request.
--
-- 'httpStatus', 'describeProvisioningArtifactResponse_httpStatus' - The response's http status code.
newDescribeProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisioningArtifactResponse
newDescribeProvisioningArtifactResponse pHttpStatus_ =
  DescribeProvisioningArtifactResponse'
    { info =
        Prelude.Nothing,
      provisioningArtifactDetail =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
-- format.
describeProvisioningArtifactResponse_info :: Lens.Lens' DescribeProvisioningArtifactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeProvisioningArtifactResponse_info = Lens.lens (\DescribeProvisioningArtifactResponse' {info} -> info) (\s@DescribeProvisioningArtifactResponse' {} a -> s {info = a} :: DescribeProvisioningArtifactResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the provisioning artifact.
describeProvisioningArtifactResponse_provisioningArtifactDetail :: Lens.Lens' DescribeProvisioningArtifactResponse (Prelude.Maybe ProvisioningArtifactDetail)
describeProvisioningArtifactResponse_provisioningArtifactDetail = Lens.lens (\DescribeProvisioningArtifactResponse' {provisioningArtifactDetail} -> provisioningArtifactDetail) (\s@DescribeProvisioningArtifactResponse' {} a -> s {provisioningArtifactDetail = a} :: DescribeProvisioningArtifactResponse)

-- | The status of the current request.
describeProvisioningArtifactResponse_status :: Lens.Lens' DescribeProvisioningArtifactResponse (Prelude.Maybe RequestStatus)
describeProvisioningArtifactResponse_status = Lens.lens (\DescribeProvisioningArtifactResponse' {status} -> status) (\s@DescribeProvisioningArtifactResponse' {} a -> s {status = a} :: DescribeProvisioningArtifactResponse)

-- | The response's http status code.
describeProvisioningArtifactResponse_httpStatus :: Lens.Lens' DescribeProvisioningArtifactResponse Prelude.Int
describeProvisioningArtifactResponse_httpStatus = Lens.lens (\DescribeProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningArtifactResponse)

instance
  Prelude.NFData
    DescribeProvisioningArtifactResponse
  where
  rnf DescribeProvisioningArtifactResponse' {..} =
    Prelude.rnf info
      `Prelude.seq` Prelude.rnf provisioningArtifactDetail
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
