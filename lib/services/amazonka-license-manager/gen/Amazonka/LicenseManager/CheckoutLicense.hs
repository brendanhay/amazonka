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
-- Module      : Amazonka.LicenseManager.CheckoutLicense
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks out the specified license.
--
-- If the account that created the license is the same that is performing
-- the check out, you must specify the account as the beneficiary.
module Amazonka.LicenseManager.CheckoutLicense
  ( -- * Creating a Request
    CheckoutLicense (..),
    newCheckoutLicense,

    -- * Request Lenses
    checkoutLicense_beneficiary,
    checkoutLicense_nodeId,
    checkoutLicense_productSKU,
    checkoutLicense_checkoutType,
    checkoutLicense_keyFingerprint,
    checkoutLicense_entitlements,
    checkoutLicense_clientToken,

    -- * Destructuring the Response
    CheckoutLicenseResponse (..),
    newCheckoutLicenseResponse,

    -- * Response Lenses
    checkoutLicenseResponse_checkoutType,
    checkoutLicenseResponse_entitlementsAllowed,
    checkoutLicenseResponse_expiration,
    checkoutLicenseResponse_issuedAt,
    checkoutLicenseResponse_licenseArn,
    checkoutLicenseResponse_licenseConsumptionToken,
    checkoutLicenseResponse_nodeId,
    checkoutLicenseResponse_signedToken,
    checkoutLicenseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCheckoutLicense' smart constructor.
data CheckoutLicense = CheckoutLicense'
  { -- | License beneficiary.
    beneficiary :: Prelude.Maybe Prelude.Text,
    -- | Node ID.
    nodeId :: Prelude.Maybe Prelude.Text,
    -- | Product SKU.
    productSKU :: Prelude.Text,
    -- | Checkout type.
    checkoutType :: CheckoutType,
    -- | Key fingerprint identifying the license.
    keyFingerprint :: Prelude.Text,
    -- | License entitlements.
    entitlements :: [EntitlementData],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckoutLicense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beneficiary', 'checkoutLicense_beneficiary' - License beneficiary.
--
-- 'nodeId', 'checkoutLicense_nodeId' - Node ID.
--
-- 'productSKU', 'checkoutLicense_productSKU' - Product SKU.
--
-- 'checkoutType', 'checkoutLicense_checkoutType' - Checkout type.
--
-- 'keyFingerprint', 'checkoutLicense_keyFingerprint' - Key fingerprint identifying the license.
--
-- 'entitlements', 'checkoutLicense_entitlements' - License entitlements.
--
-- 'clientToken', 'checkoutLicense_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
newCheckoutLicense ::
  -- | 'productSKU'
  Prelude.Text ->
  -- | 'checkoutType'
  CheckoutType ->
  -- | 'keyFingerprint'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CheckoutLicense
newCheckoutLicense
  pProductSKU_
  pCheckoutType_
  pKeyFingerprint_
  pClientToken_ =
    CheckoutLicense'
      { beneficiary = Prelude.Nothing,
        nodeId = Prelude.Nothing,
        productSKU = pProductSKU_,
        checkoutType = pCheckoutType_,
        keyFingerprint = pKeyFingerprint_,
        entitlements = Prelude.mempty,
        clientToken = pClientToken_
      }

-- | License beneficiary.
checkoutLicense_beneficiary :: Lens.Lens' CheckoutLicense (Prelude.Maybe Prelude.Text)
checkoutLicense_beneficiary = Lens.lens (\CheckoutLicense' {beneficiary} -> beneficiary) (\s@CheckoutLicense' {} a -> s {beneficiary = a} :: CheckoutLicense)

-- | Node ID.
checkoutLicense_nodeId :: Lens.Lens' CheckoutLicense (Prelude.Maybe Prelude.Text)
checkoutLicense_nodeId = Lens.lens (\CheckoutLicense' {nodeId} -> nodeId) (\s@CheckoutLicense' {} a -> s {nodeId = a} :: CheckoutLicense)

-- | Product SKU.
checkoutLicense_productSKU :: Lens.Lens' CheckoutLicense Prelude.Text
checkoutLicense_productSKU = Lens.lens (\CheckoutLicense' {productSKU} -> productSKU) (\s@CheckoutLicense' {} a -> s {productSKU = a} :: CheckoutLicense)

-- | Checkout type.
checkoutLicense_checkoutType :: Lens.Lens' CheckoutLicense CheckoutType
checkoutLicense_checkoutType = Lens.lens (\CheckoutLicense' {checkoutType} -> checkoutType) (\s@CheckoutLicense' {} a -> s {checkoutType = a} :: CheckoutLicense)

-- | Key fingerprint identifying the license.
checkoutLicense_keyFingerprint :: Lens.Lens' CheckoutLicense Prelude.Text
checkoutLicense_keyFingerprint = Lens.lens (\CheckoutLicense' {keyFingerprint} -> keyFingerprint) (\s@CheckoutLicense' {} a -> s {keyFingerprint = a} :: CheckoutLicense)

-- | License entitlements.
checkoutLicense_entitlements :: Lens.Lens' CheckoutLicense [EntitlementData]
checkoutLicense_entitlements = Lens.lens (\CheckoutLicense' {entitlements} -> entitlements) (\s@CheckoutLicense' {} a -> s {entitlements = a} :: CheckoutLicense) Prelude.. Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
checkoutLicense_clientToken :: Lens.Lens' CheckoutLicense Prelude.Text
checkoutLicense_clientToken = Lens.lens (\CheckoutLicense' {clientToken} -> clientToken) (\s@CheckoutLicense' {} a -> s {clientToken = a} :: CheckoutLicense)

instance Core.AWSRequest CheckoutLicense where
  type
    AWSResponse CheckoutLicense =
      CheckoutLicenseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckoutLicenseResponse'
            Prelude.<$> (x Data..?> "CheckoutType")
            Prelude.<*> ( x
                            Data..?> "EntitlementsAllowed"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Expiration")
            Prelude.<*> (x Data..?> "IssuedAt")
            Prelude.<*> (x Data..?> "LicenseArn")
            Prelude.<*> (x Data..?> "LicenseConsumptionToken")
            Prelude.<*> (x Data..?> "NodeId")
            Prelude.<*> (x Data..?> "SignedToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CheckoutLicense where
  hashWithSalt _salt CheckoutLicense' {..} =
    _salt
      `Prelude.hashWithSalt` beneficiary
      `Prelude.hashWithSalt` nodeId
      `Prelude.hashWithSalt` productSKU
      `Prelude.hashWithSalt` checkoutType
      `Prelude.hashWithSalt` keyFingerprint
      `Prelude.hashWithSalt` entitlements
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CheckoutLicense where
  rnf CheckoutLicense' {..} =
    Prelude.rnf beneficiary
      `Prelude.seq` Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf productSKU
      `Prelude.seq` Prelude.rnf checkoutType
      `Prelude.seq` Prelude.rnf keyFingerprint
      `Prelude.seq` Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CheckoutLicense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CheckoutLicense" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CheckoutLicense where
  toJSON CheckoutLicense' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Beneficiary" Data..=) Prelude.<$> beneficiary,
            ("NodeId" Data..=) Prelude.<$> nodeId,
            Prelude.Just ("ProductSKU" Data..= productSKU),
            Prelude.Just ("CheckoutType" Data..= checkoutType),
            Prelude.Just
              ("KeyFingerprint" Data..= keyFingerprint),
            Prelude.Just ("Entitlements" Data..= entitlements),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CheckoutLicense where
  toPath = Prelude.const "/"

instance Data.ToQuery CheckoutLicense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCheckoutLicenseResponse' smart constructor.
data CheckoutLicenseResponse = CheckoutLicenseResponse'
  { -- | Checkout type.
    checkoutType :: Prelude.Maybe CheckoutType,
    -- | Allowed license entitlements.
    entitlementsAllowed :: Prelude.Maybe [EntitlementData],
    -- | Date and time at which the license checkout expires.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | Date and time at which the license checkout is issued.
    issuedAt :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the checkout license.
    licenseArn :: Prelude.Maybe Prelude.Text,
    -- | License consumption token.
    licenseConsumptionToken :: Prelude.Maybe Prelude.Text,
    -- | Node ID.
    nodeId :: Prelude.Maybe Prelude.Text,
    -- | Signed token.
    signedToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckoutLicenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkoutType', 'checkoutLicenseResponse_checkoutType' - Checkout type.
--
-- 'entitlementsAllowed', 'checkoutLicenseResponse_entitlementsAllowed' - Allowed license entitlements.
--
-- 'expiration', 'checkoutLicenseResponse_expiration' - Date and time at which the license checkout expires.
--
-- 'issuedAt', 'checkoutLicenseResponse_issuedAt' - Date and time at which the license checkout is issued.
--
-- 'licenseArn', 'checkoutLicenseResponse_licenseArn' - Amazon Resource Name (ARN) of the checkout license.
--
-- 'licenseConsumptionToken', 'checkoutLicenseResponse_licenseConsumptionToken' - License consumption token.
--
-- 'nodeId', 'checkoutLicenseResponse_nodeId' - Node ID.
--
-- 'signedToken', 'checkoutLicenseResponse_signedToken' - Signed token.
--
-- 'httpStatus', 'checkoutLicenseResponse_httpStatus' - The response's http status code.
newCheckoutLicenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckoutLicenseResponse
newCheckoutLicenseResponse pHttpStatus_ =
  CheckoutLicenseResponse'
    { checkoutType =
        Prelude.Nothing,
      entitlementsAllowed = Prelude.Nothing,
      expiration = Prelude.Nothing,
      issuedAt = Prelude.Nothing,
      licenseArn = Prelude.Nothing,
      licenseConsumptionToken = Prelude.Nothing,
      nodeId = Prelude.Nothing,
      signedToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Checkout type.
checkoutLicenseResponse_checkoutType :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe CheckoutType)
checkoutLicenseResponse_checkoutType = Lens.lens (\CheckoutLicenseResponse' {checkoutType} -> checkoutType) (\s@CheckoutLicenseResponse' {} a -> s {checkoutType = a} :: CheckoutLicenseResponse)

-- | Allowed license entitlements.
checkoutLicenseResponse_entitlementsAllowed :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe [EntitlementData])
checkoutLicenseResponse_entitlementsAllowed = Lens.lens (\CheckoutLicenseResponse' {entitlementsAllowed} -> entitlementsAllowed) (\s@CheckoutLicenseResponse' {} a -> s {entitlementsAllowed = a} :: CheckoutLicenseResponse) Prelude.. Lens.mapping Lens.coerced

-- | Date and time at which the license checkout expires.
checkoutLicenseResponse_expiration :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutLicenseResponse_expiration = Lens.lens (\CheckoutLicenseResponse' {expiration} -> expiration) (\s@CheckoutLicenseResponse' {} a -> s {expiration = a} :: CheckoutLicenseResponse)

-- | Date and time at which the license checkout is issued.
checkoutLicenseResponse_issuedAt :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutLicenseResponse_issuedAt = Lens.lens (\CheckoutLicenseResponse' {issuedAt} -> issuedAt) (\s@CheckoutLicenseResponse' {} a -> s {issuedAt = a} :: CheckoutLicenseResponse)

-- | Amazon Resource Name (ARN) of the checkout license.
checkoutLicenseResponse_licenseArn :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutLicenseResponse_licenseArn = Lens.lens (\CheckoutLicenseResponse' {licenseArn} -> licenseArn) (\s@CheckoutLicenseResponse' {} a -> s {licenseArn = a} :: CheckoutLicenseResponse)

-- | License consumption token.
checkoutLicenseResponse_licenseConsumptionToken :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutLicenseResponse_licenseConsumptionToken = Lens.lens (\CheckoutLicenseResponse' {licenseConsumptionToken} -> licenseConsumptionToken) (\s@CheckoutLicenseResponse' {} a -> s {licenseConsumptionToken = a} :: CheckoutLicenseResponse)

-- | Node ID.
checkoutLicenseResponse_nodeId :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutLicenseResponse_nodeId = Lens.lens (\CheckoutLicenseResponse' {nodeId} -> nodeId) (\s@CheckoutLicenseResponse' {} a -> s {nodeId = a} :: CheckoutLicenseResponse)

-- | Signed token.
checkoutLicenseResponse_signedToken :: Lens.Lens' CheckoutLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutLicenseResponse_signedToken = Lens.lens (\CheckoutLicenseResponse' {signedToken} -> signedToken) (\s@CheckoutLicenseResponse' {} a -> s {signedToken = a} :: CheckoutLicenseResponse)

-- | The response's http status code.
checkoutLicenseResponse_httpStatus :: Lens.Lens' CheckoutLicenseResponse Prelude.Int
checkoutLicenseResponse_httpStatus = Lens.lens (\CheckoutLicenseResponse' {httpStatus} -> httpStatus) (\s@CheckoutLicenseResponse' {} a -> s {httpStatus = a} :: CheckoutLicenseResponse)

instance Prelude.NFData CheckoutLicenseResponse where
  rnf CheckoutLicenseResponse' {..} =
    Prelude.rnf checkoutType
      `Prelude.seq` Prelude.rnf entitlementsAllowed
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf issuedAt
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf licenseConsumptionToken
      `Prelude.seq` Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf signedToken
      `Prelude.seq` Prelude.rnf httpStatus
