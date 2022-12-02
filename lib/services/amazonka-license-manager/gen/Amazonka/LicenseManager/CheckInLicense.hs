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
-- Module      : Amazonka.LicenseManager.CheckInLicense
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks in the specified license. Check in a license when it is no longer
-- in use.
module Amazonka.LicenseManager.CheckInLicense
  ( -- * Creating a Request
    CheckInLicense (..),
    newCheckInLicense,

    -- * Request Lenses
    checkInLicense_beneficiary,
    checkInLicense_licenseConsumptionToken,

    -- * Destructuring the Response
    CheckInLicenseResponse (..),
    newCheckInLicenseResponse,

    -- * Response Lenses
    checkInLicenseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCheckInLicense' smart constructor.
data CheckInLicense = CheckInLicense'
  { -- | License beneficiary.
    beneficiary :: Prelude.Maybe Prelude.Text,
    -- | License consumption token.
    licenseConsumptionToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckInLicense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beneficiary', 'checkInLicense_beneficiary' - License beneficiary.
--
-- 'licenseConsumptionToken', 'checkInLicense_licenseConsumptionToken' - License consumption token.
newCheckInLicense ::
  -- | 'licenseConsumptionToken'
  Prelude.Text ->
  CheckInLicense
newCheckInLicense pLicenseConsumptionToken_ =
  CheckInLicense'
    { beneficiary = Prelude.Nothing,
      licenseConsumptionToken = pLicenseConsumptionToken_
    }

-- | License beneficiary.
checkInLicense_beneficiary :: Lens.Lens' CheckInLicense (Prelude.Maybe Prelude.Text)
checkInLicense_beneficiary = Lens.lens (\CheckInLicense' {beneficiary} -> beneficiary) (\s@CheckInLicense' {} a -> s {beneficiary = a} :: CheckInLicense)

-- | License consumption token.
checkInLicense_licenseConsumptionToken :: Lens.Lens' CheckInLicense Prelude.Text
checkInLicense_licenseConsumptionToken = Lens.lens (\CheckInLicense' {licenseConsumptionToken} -> licenseConsumptionToken) (\s@CheckInLicense' {} a -> s {licenseConsumptionToken = a} :: CheckInLicense)

instance Core.AWSRequest CheckInLicense where
  type
    AWSResponse CheckInLicense =
      CheckInLicenseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CheckInLicenseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CheckInLicense where
  hashWithSalt _salt CheckInLicense' {..} =
    _salt `Prelude.hashWithSalt` beneficiary
      `Prelude.hashWithSalt` licenseConsumptionToken

instance Prelude.NFData CheckInLicense where
  rnf CheckInLicense' {..} =
    Prelude.rnf beneficiary
      `Prelude.seq` Prelude.rnf licenseConsumptionToken

instance Data.ToHeaders CheckInLicense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CheckInLicense" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CheckInLicense where
  toJSON CheckInLicense' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Beneficiary" Data..=) Prelude.<$> beneficiary,
            Prelude.Just
              ( "LicenseConsumptionToken"
                  Data..= licenseConsumptionToken
              )
          ]
      )

instance Data.ToPath CheckInLicense where
  toPath = Prelude.const "/"

instance Data.ToQuery CheckInLicense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCheckInLicenseResponse' smart constructor.
data CheckInLicenseResponse = CheckInLicenseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckInLicenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'checkInLicenseResponse_httpStatus' - The response's http status code.
newCheckInLicenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckInLicenseResponse
newCheckInLicenseResponse pHttpStatus_ =
  CheckInLicenseResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
checkInLicenseResponse_httpStatus :: Lens.Lens' CheckInLicenseResponse Prelude.Int
checkInLicenseResponse_httpStatus = Lens.lens (\CheckInLicenseResponse' {httpStatus} -> httpStatus) (\s@CheckInLicenseResponse' {} a -> s {httpStatus = a} :: CheckInLicenseResponse)

instance Prelude.NFData CheckInLicenseResponse where
  rnf CheckInLicenseResponse' {..} =
    Prelude.rnf httpStatus
