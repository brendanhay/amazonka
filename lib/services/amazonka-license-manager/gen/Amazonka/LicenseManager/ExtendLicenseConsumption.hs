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
-- Module      : Amazonka.LicenseManager.ExtendLicenseConsumption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Extends the expiration date for license consumption.
module Amazonka.LicenseManager.ExtendLicenseConsumption
  ( -- * Creating a Request
    ExtendLicenseConsumption (..),
    newExtendLicenseConsumption,

    -- * Request Lenses
    extendLicenseConsumption_dryRun,
    extendLicenseConsumption_licenseConsumptionToken,

    -- * Destructuring the Response
    ExtendLicenseConsumptionResponse (..),
    newExtendLicenseConsumptionResponse,

    -- * Response Lenses
    extendLicenseConsumptionResponse_expiration,
    extendLicenseConsumptionResponse_licenseConsumptionToken,
    extendLicenseConsumptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExtendLicenseConsumption' smart constructor.
data ExtendLicenseConsumption = ExtendLicenseConsumption'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request. Provides an error response if you do not
    -- have the required permissions.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | License consumption token.
    licenseConsumptionToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendLicenseConsumption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'extendLicenseConsumption_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request. Provides an error response if you do not
-- have the required permissions.
--
-- 'licenseConsumptionToken', 'extendLicenseConsumption_licenseConsumptionToken' - License consumption token.
newExtendLicenseConsumption ::
  -- | 'licenseConsumptionToken'
  Prelude.Text ->
  ExtendLicenseConsumption
newExtendLicenseConsumption pLicenseConsumptionToken_ =
  ExtendLicenseConsumption'
    { dryRun = Prelude.Nothing,
      licenseConsumptionToken =
        pLicenseConsumptionToken_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request. Provides an error response if you do not
-- have the required permissions.
extendLicenseConsumption_dryRun :: Lens.Lens' ExtendLicenseConsumption (Prelude.Maybe Prelude.Bool)
extendLicenseConsumption_dryRun = Lens.lens (\ExtendLicenseConsumption' {dryRun} -> dryRun) (\s@ExtendLicenseConsumption' {} a -> s {dryRun = a} :: ExtendLicenseConsumption)

-- | License consumption token.
extendLicenseConsumption_licenseConsumptionToken :: Lens.Lens' ExtendLicenseConsumption Prelude.Text
extendLicenseConsumption_licenseConsumptionToken = Lens.lens (\ExtendLicenseConsumption' {licenseConsumptionToken} -> licenseConsumptionToken) (\s@ExtendLicenseConsumption' {} a -> s {licenseConsumptionToken = a} :: ExtendLicenseConsumption)

instance Core.AWSRequest ExtendLicenseConsumption where
  type
    AWSResponse ExtendLicenseConsumption =
      ExtendLicenseConsumptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExtendLicenseConsumptionResponse'
            Prelude.<$> (x Data..?> "Expiration")
            Prelude.<*> (x Data..?> "LicenseConsumptionToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExtendLicenseConsumption where
  hashWithSalt _salt ExtendLicenseConsumption' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` licenseConsumptionToken

instance Prelude.NFData ExtendLicenseConsumption where
  rnf ExtendLicenseConsumption' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf licenseConsumptionToken

instance Data.ToHeaders ExtendLicenseConsumption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ExtendLicenseConsumption" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExtendLicenseConsumption where
  toJSON ExtendLicenseConsumption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "LicenseConsumptionToken"
                  Data..= licenseConsumptionToken
              )
          ]
      )

instance Data.ToPath ExtendLicenseConsumption where
  toPath = Prelude.const "/"

instance Data.ToQuery ExtendLicenseConsumption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExtendLicenseConsumptionResponse' smart constructor.
data ExtendLicenseConsumptionResponse = ExtendLicenseConsumptionResponse'
  { -- | Date and time at which the license consumption expires.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | License consumption token.
    licenseConsumptionToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendLicenseConsumptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'extendLicenseConsumptionResponse_expiration' - Date and time at which the license consumption expires.
--
-- 'licenseConsumptionToken', 'extendLicenseConsumptionResponse_licenseConsumptionToken' - License consumption token.
--
-- 'httpStatus', 'extendLicenseConsumptionResponse_httpStatus' - The response's http status code.
newExtendLicenseConsumptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExtendLicenseConsumptionResponse
newExtendLicenseConsumptionResponse pHttpStatus_ =
  ExtendLicenseConsumptionResponse'
    { expiration =
        Prelude.Nothing,
      licenseConsumptionToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Date and time at which the license consumption expires.
extendLicenseConsumptionResponse_expiration :: Lens.Lens' ExtendLicenseConsumptionResponse (Prelude.Maybe Prelude.Text)
extendLicenseConsumptionResponse_expiration = Lens.lens (\ExtendLicenseConsumptionResponse' {expiration} -> expiration) (\s@ExtendLicenseConsumptionResponse' {} a -> s {expiration = a} :: ExtendLicenseConsumptionResponse)

-- | License consumption token.
extendLicenseConsumptionResponse_licenseConsumptionToken :: Lens.Lens' ExtendLicenseConsumptionResponse (Prelude.Maybe Prelude.Text)
extendLicenseConsumptionResponse_licenseConsumptionToken = Lens.lens (\ExtendLicenseConsumptionResponse' {licenseConsumptionToken} -> licenseConsumptionToken) (\s@ExtendLicenseConsumptionResponse' {} a -> s {licenseConsumptionToken = a} :: ExtendLicenseConsumptionResponse)

-- | The response's http status code.
extendLicenseConsumptionResponse_httpStatus :: Lens.Lens' ExtendLicenseConsumptionResponse Prelude.Int
extendLicenseConsumptionResponse_httpStatus = Lens.lens (\ExtendLicenseConsumptionResponse' {httpStatus} -> httpStatus) (\s@ExtendLicenseConsumptionResponse' {} a -> s {httpStatus = a} :: ExtendLicenseConsumptionResponse)

instance
  Prelude.NFData
    ExtendLicenseConsumptionResponse
  where
  rnf ExtendLicenseConsumptionResponse' {..} =
    Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf licenseConsumptionToken
      `Prelude.seq` Prelude.rnf httpStatus
