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
-- Module      : Amazonka.LicenseManager.GetLicenseUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information about the usage of the specified license.
module Amazonka.LicenseManager.GetLicenseUsage
  ( -- * Creating a Request
    GetLicenseUsage (..),
    newGetLicenseUsage,

    -- * Request Lenses
    getLicenseUsage_licenseArn,

    -- * Destructuring the Response
    GetLicenseUsageResponse (..),
    newGetLicenseUsageResponse,

    -- * Response Lenses
    getLicenseUsageResponse_licenseUsage,
    getLicenseUsageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLicenseUsage' smart constructor.
data GetLicenseUsage = GetLicenseUsage'
  { -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseArn', 'getLicenseUsage_licenseArn' - Amazon Resource Name (ARN) of the license.
newGetLicenseUsage ::
  -- | 'licenseArn'
  Prelude.Text ->
  GetLicenseUsage
newGetLicenseUsage pLicenseArn_ =
  GetLicenseUsage' {licenseArn = pLicenseArn_}

-- | Amazon Resource Name (ARN) of the license.
getLicenseUsage_licenseArn :: Lens.Lens' GetLicenseUsage Prelude.Text
getLicenseUsage_licenseArn = Lens.lens (\GetLicenseUsage' {licenseArn} -> licenseArn) (\s@GetLicenseUsage' {} a -> s {licenseArn = a} :: GetLicenseUsage)

instance Core.AWSRequest GetLicenseUsage where
  type
    AWSResponse GetLicenseUsage =
      GetLicenseUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLicenseUsageResponse'
            Prelude.<$> (x Core..?> "LicenseUsage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLicenseUsage where
  hashWithSalt _salt GetLicenseUsage' {..} =
    _salt `Prelude.hashWithSalt` licenseArn

instance Prelude.NFData GetLicenseUsage where
  rnf GetLicenseUsage' {..} = Prelude.rnf licenseArn

instance Core.ToHeaders GetLicenseUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.GetLicenseUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLicenseUsage where
  toJSON GetLicenseUsage' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LicenseArn" Core..= licenseArn)]
      )

instance Core.ToPath GetLicenseUsage where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLicenseUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLicenseUsageResponse' smart constructor.
data GetLicenseUsageResponse = GetLicenseUsageResponse'
  { -- | License usage details.
    licenseUsage :: Prelude.Maybe LicenseUsage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseUsage', 'getLicenseUsageResponse_licenseUsage' - License usage details.
--
-- 'httpStatus', 'getLicenseUsageResponse_httpStatus' - The response's http status code.
newGetLicenseUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLicenseUsageResponse
newGetLicenseUsageResponse pHttpStatus_ =
  GetLicenseUsageResponse'
    { licenseUsage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | License usage details.
getLicenseUsageResponse_licenseUsage :: Lens.Lens' GetLicenseUsageResponse (Prelude.Maybe LicenseUsage)
getLicenseUsageResponse_licenseUsage = Lens.lens (\GetLicenseUsageResponse' {licenseUsage} -> licenseUsage) (\s@GetLicenseUsageResponse' {} a -> s {licenseUsage = a} :: GetLicenseUsageResponse)

-- | The response's http status code.
getLicenseUsageResponse_httpStatus :: Lens.Lens' GetLicenseUsageResponse Prelude.Int
getLicenseUsageResponse_httpStatus = Lens.lens (\GetLicenseUsageResponse' {httpStatus} -> httpStatus) (\s@GetLicenseUsageResponse' {} a -> s {httpStatus = a} :: GetLicenseUsageResponse)

instance Prelude.NFData GetLicenseUsageResponse where
  rnf GetLicenseUsageResponse' {..} =
    Prelude.rnf licenseUsage
      `Prelude.seq` Prelude.rnf httpStatus
