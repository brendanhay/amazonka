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
-- Module      : Amazonka.LicenseManager.CreateLicense
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a license.
module Amazonka.LicenseManager.CreateLicense
  ( -- * Creating a Request
    CreateLicense (..),
    newCreateLicense,

    -- * Request Lenses
    createLicense_licenseMetadata,
    createLicense_licenseName,
    createLicense_productName,
    createLicense_productSKU,
    createLicense_issuer,
    createLicense_homeRegion,
    createLicense_validity,
    createLicense_entitlements,
    createLicense_beneficiary,
    createLicense_consumptionConfiguration,
    createLicense_clientToken,

    -- * Destructuring the Response
    CreateLicenseResponse (..),
    newCreateLicenseResponse,

    -- * Response Lenses
    createLicenseResponse_licenseArn,
    createLicenseResponse_status,
    createLicenseResponse_version,
    createLicenseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLicense' smart constructor.
data CreateLicense = CreateLicense'
  { -- | Information about the license.
    licenseMetadata :: Prelude.Maybe [Metadata],
    -- | License name.
    licenseName :: Prelude.Text,
    -- | Product name.
    productName :: Prelude.Text,
    -- | Product SKU.
    productSKU :: Prelude.Text,
    -- | License issuer.
    issuer :: Issuer,
    -- | Home Region for the license.
    homeRegion :: Prelude.Text,
    -- | Date and time range during which the license is valid, in ISO8601-UTC
    -- format.
    validity :: DatetimeRange,
    -- | License entitlements.
    entitlements :: [Entitlement],
    -- | License beneficiary.
    beneficiary :: Prelude.Text,
    -- | Configuration for consumption of the license. Choose a provisional
    -- configuration for workloads running with continuous connectivity. Choose
    -- a borrow configuration for workloads with offline usage.
    consumptionConfiguration :: ConsumptionConfiguration,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseMetadata', 'createLicense_licenseMetadata' - Information about the license.
--
-- 'licenseName', 'createLicense_licenseName' - License name.
--
-- 'productName', 'createLicense_productName' - Product name.
--
-- 'productSKU', 'createLicense_productSKU' - Product SKU.
--
-- 'issuer', 'createLicense_issuer' - License issuer.
--
-- 'homeRegion', 'createLicense_homeRegion' - Home Region for the license.
--
-- 'validity', 'createLicense_validity' - Date and time range during which the license is valid, in ISO8601-UTC
-- format.
--
-- 'entitlements', 'createLicense_entitlements' - License entitlements.
--
-- 'beneficiary', 'createLicense_beneficiary' - License beneficiary.
--
-- 'consumptionConfiguration', 'createLicense_consumptionConfiguration' - Configuration for consumption of the license. Choose a provisional
-- configuration for workloads running with continuous connectivity. Choose
-- a borrow configuration for workloads with offline usage.
--
-- 'clientToken', 'createLicense_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
newCreateLicense ::
  -- | 'licenseName'
  Prelude.Text ->
  -- | 'productName'
  Prelude.Text ->
  -- | 'productSKU'
  Prelude.Text ->
  -- | 'issuer'
  Issuer ->
  -- | 'homeRegion'
  Prelude.Text ->
  -- | 'validity'
  DatetimeRange ->
  -- | 'beneficiary'
  Prelude.Text ->
  -- | 'consumptionConfiguration'
  ConsumptionConfiguration ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateLicense
newCreateLicense
  pLicenseName_
  pProductName_
  pProductSKU_
  pIssuer_
  pHomeRegion_
  pValidity_
  pBeneficiary_
  pConsumptionConfiguration_
  pClientToken_ =
    CreateLicense'
      { licenseMetadata = Prelude.Nothing,
        licenseName = pLicenseName_,
        productName = pProductName_,
        productSKU = pProductSKU_,
        issuer = pIssuer_,
        homeRegion = pHomeRegion_,
        validity = pValidity_,
        entitlements = Prelude.mempty,
        beneficiary = pBeneficiary_,
        consumptionConfiguration =
          pConsumptionConfiguration_,
        clientToken = pClientToken_
      }

-- | Information about the license.
createLicense_licenseMetadata :: Lens.Lens' CreateLicense (Prelude.Maybe [Metadata])
createLicense_licenseMetadata = Lens.lens (\CreateLicense' {licenseMetadata} -> licenseMetadata) (\s@CreateLicense' {} a -> s {licenseMetadata = a} :: CreateLicense) Prelude.. Lens.mapping Lens.coerced

-- | License name.
createLicense_licenseName :: Lens.Lens' CreateLicense Prelude.Text
createLicense_licenseName = Lens.lens (\CreateLicense' {licenseName} -> licenseName) (\s@CreateLicense' {} a -> s {licenseName = a} :: CreateLicense)

-- | Product name.
createLicense_productName :: Lens.Lens' CreateLicense Prelude.Text
createLicense_productName = Lens.lens (\CreateLicense' {productName} -> productName) (\s@CreateLicense' {} a -> s {productName = a} :: CreateLicense)

-- | Product SKU.
createLicense_productSKU :: Lens.Lens' CreateLicense Prelude.Text
createLicense_productSKU = Lens.lens (\CreateLicense' {productSKU} -> productSKU) (\s@CreateLicense' {} a -> s {productSKU = a} :: CreateLicense)

-- | License issuer.
createLicense_issuer :: Lens.Lens' CreateLicense Issuer
createLicense_issuer = Lens.lens (\CreateLicense' {issuer} -> issuer) (\s@CreateLicense' {} a -> s {issuer = a} :: CreateLicense)

-- | Home Region for the license.
createLicense_homeRegion :: Lens.Lens' CreateLicense Prelude.Text
createLicense_homeRegion = Lens.lens (\CreateLicense' {homeRegion} -> homeRegion) (\s@CreateLicense' {} a -> s {homeRegion = a} :: CreateLicense)

-- | Date and time range during which the license is valid, in ISO8601-UTC
-- format.
createLicense_validity :: Lens.Lens' CreateLicense DatetimeRange
createLicense_validity = Lens.lens (\CreateLicense' {validity} -> validity) (\s@CreateLicense' {} a -> s {validity = a} :: CreateLicense)

-- | License entitlements.
createLicense_entitlements :: Lens.Lens' CreateLicense [Entitlement]
createLicense_entitlements = Lens.lens (\CreateLicense' {entitlements} -> entitlements) (\s@CreateLicense' {} a -> s {entitlements = a} :: CreateLicense) Prelude.. Lens.coerced

-- | License beneficiary.
createLicense_beneficiary :: Lens.Lens' CreateLicense Prelude.Text
createLicense_beneficiary = Lens.lens (\CreateLicense' {beneficiary} -> beneficiary) (\s@CreateLicense' {} a -> s {beneficiary = a} :: CreateLicense)

-- | Configuration for consumption of the license. Choose a provisional
-- configuration for workloads running with continuous connectivity. Choose
-- a borrow configuration for workloads with offline usage.
createLicense_consumptionConfiguration :: Lens.Lens' CreateLicense ConsumptionConfiguration
createLicense_consumptionConfiguration = Lens.lens (\CreateLicense' {consumptionConfiguration} -> consumptionConfiguration) (\s@CreateLicense' {} a -> s {consumptionConfiguration = a} :: CreateLicense)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createLicense_clientToken :: Lens.Lens' CreateLicense Prelude.Text
createLicense_clientToken = Lens.lens (\CreateLicense' {clientToken} -> clientToken) (\s@CreateLicense' {} a -> s {clientToken = a} :: CreateLicense)

instance Core.AWSRequest CreateLicense where
  type
    AWSResponse CreateLicense =
      CreateLicenseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLicenseResponse'
            Prelude.<$> (x Data..?> "LicenseArn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLicense where
  hashWithSalt _salt CreateLicense' {..} =
    _salt
      `Prelude.hashWithSalt` licenseMetadata
      `Prelude.hashWithSalt` licenseName
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` productSKU
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` validity
      `Prelude.hashWithSalt` entitlements
      `Prelude.hashWithSalt` beneficiary
      `Prelude.hashWithSalt` consumptionConfiguration
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateLicense where
  rnf CreateLicense' {..} =
    Prelude.rnf licenseMetadata `Prelude.seq`
      Prelude.rnf licenseName `Prelude.seq`
        Prelude.rnf productName `Prelude.seq`
          Prelude.rnf productSKU `Prelude.seq`
            Prelude.rnf issuer `Prelude.seq`
              Prelude.rnf homeRegion `Prelude.seq`
                Prelude.rnf validity `Prelude.seq`
                  Prelude.rnf entitlements `Prelude.seq`
                    Prelude.rnf beneficiary `Prelude.seq`
                      Prelude.rnf consumptionConfiguration `Prelude.seq`
                        Prelude.rnf clientToken

instance Data.ToHeaders CreateLicense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CreateLicense" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLicense where
  toJSON CreateLicense' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LicenseMetadata" Data..=)
              Prelude.<$> licenseMetadata,
            Prelude.Just ("LicenseName" Data..= licenseName),
            Prelude.Just ("ProductName" Data..= productName),
            Prelude.Just ("ProductSKU" Data..= productSKU),
            Prelude.Just ("Issuer" Data..= issuer),
            Prelude.Just ("HomeRegion" Data..= homeRegion),
            Prelude.Just ("Validity" Data..= validity),
            Prelude.Just ("Entitlements" Data..= entitlements),
            Prelude.Just ("Beneficiary" Data..= beneficiary),
            Prelude.Just
              ( "ConsumptionConfiguration"
                  Data..= consumptionConfiguration
              ),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateLicense where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLicense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLicenseResponse' smart constructor.
data CreateLicenseResponse = CreateLicenseResponse'
  { -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Maybe Prelude.Text,
    -- | License status.
    status :: Prelude.Maybe LicenseStatus,
    -- | License version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseArn', 'createLicenseResponse_licenseArn' - Amazon Resource Name (ARN) of the license.
--
-- 'status', 'createLicenseResponse_status' - License status.
--
-- 'version', 'createLicenseResponse_version' - License version.
--
-- 'httpStatus', 'createLicenseResponse_httpStatus' - The response's http status code.
newCreateLicenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLicenseResponse
newCreateLicenseResponse pHttpStatus_ =
  CreateLicenseResponse'
    { licenseArn =
        Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amazon Resource Name (ARN) of the license.
createLicenseResponse_licenseArn :: Lens.Lens' CreateLicenseResponse (Prelude.Maybe Prelude.Text)
createLicenseResponse_licenseArn = Lens.lens (\CreateLicenseResponse' {licenseArn} -> licenseArn) (\s@CreateLicenseResponse' {} a -> s {licenseArn = a} :: CreateLicenseResponse)

-- | License status.
createLicenseResponse_status :: Lens.Lens' CreateLicenseResponse (Prelude.Maybe LicenseStatus)
createLicenseResponse_status = Lens.lens (\CreateLicenseResponse' {status} -> status) (\s@CreateLicenseResponse' {} a -> s {status = a} :: CreateLicenseResponse)

-- | License version.
createLicenseResponse_version :: Lens.Lens' CreateLicenseResponse (Prelude.Maybe Prelude.Text)
createLicenseResponse_version = Lens.lens (\CreateLicenseResponse' {version} -> version) (\s@CreateLicenseResponse' {} a -> s {version = a} :: CreateLicenseResponse)

-- | The response's http status code.
createLicenseResponse_httpStatus :: Lens.Lens' CreateLicenseResponse Prelude.Int
createLicenseResponse_httpStatus = Lens.lens (\CreateLicenseResponse' {httpStatus} -> httpStatus) (\s@CreateLicenseResponse' {} a -> s {httpStatus = a} :: CreateLicenseResponse)

instance Prelude.NFData CreateLicenseResponse where
  rnf CreateLicenseResponse' {..} =
    Prelude.rnf licenseArn `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf version `Prelude.seq`
          Prelude.rnf httpStatus
