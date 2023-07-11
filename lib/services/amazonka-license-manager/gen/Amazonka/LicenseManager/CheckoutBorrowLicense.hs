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
-- Module      : Amazonka.LicenseManager.CheckoutBorrowLicense
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks out the specified license for offline use.
module Amazonka.LicenseManager.CheckoutBorrowLicense
  ( -- * Creating a Request
    CheckoutBorrowLicense (..),
    newCheckoutBorrowLicense,

    -- * Request Lenses
    checkoutBorrowLicense_checkoutMetadata,
    checkoutBorrowLicense_nodeId,
    checkoutBorrowLicense_licenseArn,
    checkoutBorrowLicense_entitlements,
    checkoutBorrowLicense_digitalSignatureMethod,
    checkoutBorrowLicense_clientToken,

    -- * Destructuring the Response
    CheckoutBorrowLicenseResponse (..),
    newCheckoutBorrowLicenseResponse,

    -- * Response Lenses
    checkoutBorrowLicenseResponse_checkoutMetadata,
    checkoutBorrowLicenseResponse_entitlementsAllowed,
    checkoutBorrowLicenseResponse_expiration,
    checkoutBorrowLicenseResponse_issuedAt,
    checkoutBorrowLicenseResponse_licenseArn,
    checkoutBorrowLicenseResponse_licenseConsumptionToken,
    checkoutBorrowLicenseResponse_nodeId,
    checkoutBorrowLicenseResponse_signedToken,
    checkoutBorrowLicenseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCheckoutBorrowLicense' smart constructor.
data CheckoutBorrowLicense = CheckoutBorrowLicense'
  { -- | Information about constraints.
    checkoutMetadata :: Prelude.Maybe [Metadata],
    -- | Node ID.
    nodeId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license. The license must use the
    -- borrow consumption configuration.
    licenseArn :: Prelude.Text,
    -- | License entitlements. Partial checkouts are not supported.
    entitlements :: [EntitlementData],
    -- | Digital signature method. The possible value is JSON Web Signature (JWS)
    -- algorithm PS384. For more information, see
    -- <https://tools.ietf.org/html/rfc7518#section-3.5 RFC 7518 Digital Signature with RSASSA-PSS>.
    digitalSignatureMethod :: DigitalSignatureMethod,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckoutBorrowLicense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkoutMetadata', 'checkoutBorrowLicense_checkoutMetadata' - Information about constraints.
--
-- 'nodeId', 'checkoutBorrowLicense_nodeId' - Node ID.
--
-- 'licenseArn', 'checkoutBorrowLicense_licenseArn' - Amazon Resource Name (ARN) of the license. The license must use the
-- borrow consumption configuration.
--
-- 'entitlements', 'checkoutBorrowLicense_entitlements' - License entitlements. Partial checkouts are not supported.
--
-- 'digitalSignatureMethod', 'checkoutBorrowLicense_digitalSignatureMethod' - Digital signature method. The possible value is JSON Web Signature (JWS)
-- algorithm PS384. For more information, see
-- <https://tools.ietf.org/html/rfc7518#section-3.5 RFC 7518 Digital Signature with RSASSA-PSS>.
--
-- 'clientToken', 'checkoutBorrowLicense_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
newCheckoutBorrowLicense ::
  -- | 'licenseArn'
  Prelude.Text ->
  -- | 'digitalSignatureMethod'
  DigitalSignatureMethod ->
  -- | 'clientToken'
  Prelude.Text ->
  CheckoutBorrowLicense
newCheckoutBorrowLicense
  pLicenseArn_
  pDigitalSignatureMethod_
  pClientToken_ =
    CheckoutBorrowLicense'
      { checkoutMetadata =
          Prelude.Nothing,
        nodeId = Prelude.Nothing,
        licenseArn = pLicenseArn_,
        entitlements = Prelude.mempty,
        digitalSignatureMethod = pDigitalSignatureMethod_,
        clientToken = pClientToken_
      }

-- | Information about constraints.
checkoutBorrowLicense_checkoutMetadata :: Lens.Lens' CheckoutBorrowLicense (Prelude.Maybe [Metadata])
checkoutBorrowLicense_checkoutMetadata = Lens.lens (\CheckoutBorrowLicense' {checkoutMetadata} -> checkoutMetadata) (\s@CheckoutBorrowLicense' {} a -> s {checkoutMetadata = a} :: CheckoutBorrowLicense) Prelude.. Lens.mapping Lens.coerced

-- | Node ID.
checkoutBorrowLicense_nodeId :: Lens.Lens' CheckoutBorrowLicense (Prelude.Maybe Prelude.Text)
checkoutBorrowLicense_nodeId = Lens.lens (\CheckoutBorrowLicense' {nodeId} -> nodeId) (\s@CheckoutBorrowLicense' {} a -> s {nodeId = a} :: CheckoutBorrowLicense)

-- | Amazon Resource Name (ARN) of the license. The license must use the
-- borrow consumption configuration.
checkoutBorrowLicense_licenseArn :: Lens.Lens' CheckoutBorrowLicense Prelude.Text
checkoutBorrowLicense_licenseArn = Lens.lens (\CheckoutBorrowLicense' {licenseArn} -> licenseArn) (\s@CheckoutBorrowLicense' {} a -> s {licenseArn = a} :: CheckoutBorrowLicense)

-- | License entitlements. Partial checkouts are not supported.
checkoutBorrowLicense_entitlements :: Lens.Lens' CheckoutBorrowLicense [EntitlementData]
checkoutBorrowLicense_entitlements = Lens.lens (\CheckoutBorrowLicense' {entitlements} -> entitlements) (\s@CheckoutBorrowLicense' {} a -> s {entitlements = a} :: CheckoutBorrowLicense) Prelude.. Lens.coerced

-- | Digital signature method. The possible value is JSON Web Signature (JWS)
-- algorithm PS384. For more information, see
-- <https://tools.ietf.org/html/rfc7518#section-3.5 RFC 7518 Digital Signature with RSASSA-PSS>.
checkoutBorrowLicense_digitalSignatureMethod :: Lens.Lens' CheckoutBorrowLicense DigitalSignatureMethod
checkoutBorrowLicense_digitalSignatureMethod = Lens.lens (\CheckoutBorrowLicense' {digitalSignatureMethod} -> digitalSignatureMethod) (\s@CheckoutBorrowLicense' {} a -> s {digitalSignatureMethod = a} :: CheckoutBorrowLicense)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
checkoutBorrowLicense_clientToken :: Lens.Lens' CheckoutBorrowLicense Prelude.Text
checkoutBorrowLicense_clientToken = Lens.lens (\CheckoutBorrowLicense' {clientToken} -> clientToken) (\s@CheckoutBorrowLicense' {} a -> s {clientToken = a} :: CheckoutBorrowLicense)

instance Core.AWSRequest CheckoutBorrowLicense where
  type
    AWSResponse CheckoutBorrowLicense =
      CheckoutBorrowLicenseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckoutBorrowLicenseResponse'
            Prelude.<$> ( x
                            Data..?> "CheckoutMetadata"
                            Core..!@ Prelude.mempty
                        )
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

instance Prelude.Hashable CheckoutBorrowLicense where
  hashWithSalt _salt CheckoutBorrowLicense' {..} =
    _salt
      `Prelude.hashWithSalt` checkoutMetadata
      `Prelude.hashWithSalt` nodeId
      `Prelude.hashWithSalt` licenseArn
      `Prelude.hashWithSalt` entitlements
      `Prelude.hashWithSalt` digitalSignatureMethod
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CheckoutBorrowLicense where
  rnf CheckoutBorrowLicense' {..} =
    Prelude.rnf checkoutMetadata
      `Prelude.seq` Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf digitalSignatureMethod
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CheckoutBorrowLicense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CheckoutBorrowLicense" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CheckoutBorrowLicense where
  toJSON CheckoutBorrowLicense' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CheckoutMetadata" Data..=)
              Prelude.<$> checkoutMetadata,
            ("NodeId" Data..=) Prelude.<$> nodeId,
            Prelude.Just ("LicenseArn" Data..= licenseArn),
            Prelude.Just ("Entitlements" Data..= entitlements),
            Prelude.Just
              ( "DigitalSignatureMethod"
                  Data..= digitalSignatureMethod
              ),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CheckoutBorrowLicense where
  toPath = Prelude.const "/"

instance Data.ToQuery CheckoutBorrowLicense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCheckoutBorrowLicenseResponse' smart constructor.
data CheckoutBorrowLicenseResponse = CheckoutBorrowLicenseResponse'
  { -- | Information about constraints.
    checkoutMetadata :: Prelude.Maybe [Metadata],
    -- | Allowed license entitlements.
    entitlementsAllowed :: Prelude.Maybe [EntitlementData],
    -- | Date and time at which the license checkout expires.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | Date and time at which the license checkout is issued.
    issuedAt :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license.
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
-- Create a value of 'CheckoutBorrowLicenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkoutMetadata', 'checkoutBorrowLicenseResponse_checkoutMetadata' - Information about constraints.
--
-- 'entitlementsAllowed', 'checkoutBorrowLicenseResponse_entitlementsAllowed' - Allowed license entitlements.
--
-- 'expiration', 'checkoutBorrowLicenseResponse_expiration' - Date and time at which the license checkout expires.
--
-- 'issuedAt', 'checkoutBorrowLicenseResponse_issuedAt' - Date and time at which the license checkout is issued.
--
-- 'licenseArn', 'checkoutBorrowLicenseResponse_licenseArn' - Amazon Resource Name (ARN) of the license.
--
-- 'licenseConsumptionToken', 'checkoutBorrowLicenseResponse_licenseConsumptionToken' - License consumption token.
--
-- 'nodeId', 'checkoutBorrowLicenseResponse_nodeId' - Node ID.
--
-- 'signedToken', 'checkoutBorrowLicenseResponse_signedToken' - Signed token.
--
-- 'httpStatus', 'checkoutBorrowLicenseResponse_httpStatus' - The response's http status code.
newCheckoutBorrowLicenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckoutBorrowLicenseResponse
newCheckoutBorrowLicenseResponse pHttpStatus_ =
  CheckoutBorrowLicenseResponse'
    { checkoutMetadata =
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

-- | Information about constraints.
checkoutBorrowLicenseResponse_checkoutMetadata :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe [Metadata])
checkoutBorrowLicenseResponse_checkoutMetadata = Lens.lens (\CheckoutBorrowLicenseResponse' {checkoutMetadata} -> checkoutMetadata) (\s@CheckoutBorrowLicenseResponse' {} a -> s {checkoutMetadata = a} :: CheckoutBorrowLicenseResponse) Prelude.. Lens.mapping Lens.coerced

-- | Allowed license entitlements.
checkoutBorrowLicenseResponse_entitlementsAllowed :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe [EntitlementData])
checkoutBorrowLicenseResponse_entitlementsAllowed = Lens.lens (\CheckoutBorrowLicenseResponse' {entitlementsAllowed} -> entitlementsAllowed) (\s@CheckoutBorrowLicenseResponse' {} a -> s {entitlementsAllowed = a} :: CheckoutBorrowLicenseResponse) Prelude.. Lens.mapping Lens.coerced

-- | Date and time at which the license checkout expires.
checkoutBorrowLicenseResponse_expiration :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutBorrowLicenseResponse_expiration = Lens.lens (\CheckoutBorrowLicenseResponse' {expiration} -> expiration) (\s@CheckoutBorrowLicenseResponse' {} a -> s {expiration = a} :: CheckoutBorrowLicenseResponse)

-- | Date and time at which the license checkout is issued.
checkoutBorrowLicenseResponse_issuedAt :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutBorrowLicenseResponse_issuedAt = Lens.lens (\CheckoutBorrowLicenseResponse' {issuedAt} -> issuedAt) (\s@CheckoutBorrowLicenseResponse' {} a -> s {issuedAt = a} :: CheckoutBorrowLicenseResponse)

-- | Amazon Resource Name (ARN) of the license.
checkoutBorrowLicenseResponse_licenseArn :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutBorrowLicenseResponse_licenseArn = Lens.lens (\CheckoutBorrowLicenseResponse' {licenseArn} -> licenseArn) (\s@CheckoutBorrowLicenseResponse' {} a -> s {licenseArn = a} :: CheckoutBorrowLicenseResponse)

-- | License consumption token.
checkoutBorrowLicenseResponse_licenseConsumptionToken :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutBorrowLicenseResponse_licenseConsumptionToken = Lens.lens (\CheckoutBorrowLicenseResponse' {licenseConsumptionToken} -> licenseConsumptionToken) (\s@CheckoutBorrowLicenseResponse' {} a -> s {licenseConsumptionToken = a} :: CheckoutBorrowLicenseResponse)

-- | Node ID.
checkoutBorrowLicenseResponse_nodeId :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutBorrowLicenseResponse_nodeId = Lens.lens (\CheckoutBorrowLicenseResponse' {nodeId} -> nodeId) (\s@CheckoutBorrowLicenseResponse' {} a -> s {nodeId = a} :: CheckoutBorrowLicenseResponse)

-- | Signed token.
checkoutBorrowLicenseResponse_signedToken :: Lens.Lens' CheckoutBorrowLicenseResponse (Prelude.Maybe Prelude.Text)
checkoutBorrowLicenseResponse_signedToken = Lens.lens (\CheckoutBorrowLicenseResponse' {signedToken} -> signedToken) (\s@CheckoutBorrowLicenseResponse' {} a -> s {signedToken = a} :: CheckoutBorrowLicenseResponse)

-- | The response's http status code.
checkoutBorrowLicenseResponse_httpStatus :: Lens.Lens' CheckoutBorrowLicenseResponse Prelude.Int
checkoutBorrowLicenseResponse_httpStatus = Lens.lens (\CheckoutBorrowLicenseResponse' {httpStatus} -> httpStatus) (\s@CheckoutBorrowLicenseResponse' {} a -> s {httpStatus = a} :: CheckoutBorrowLicenseResponse)

instance Prelude.NFData CheckoutBorrowLicenseResponse where
  rnf CheckoutBorrowLicenseResponse' {..} =
    Prelude.rnf checkoutMetadata
      `Prelude.seq` Prelude.rnf entitlementsAllowed
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf issuedAt
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf licenseConsumptionToken
      `Prelude.seq` Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf signedToken
      `Prelude.seq` Prelude.rnf httpStatus
