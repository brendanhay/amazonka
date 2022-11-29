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
-- Module      : Amazonka.WAFV2.GetMobileSdkRelease
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information for the specified mobile SDK release, including
-- release notes and tags.
--
-- The mobile SDK is not generally available. Customers who have access to
-- the mobile SDK can use it to establish and manage WAF tokens for use in
-- HTTP(S) requests from a mobile device to WAF. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
module Amazonka.WAFV2.GetMobileSdkRelease
  ( -- * Creating a Request
    GetMobileSdkRelease (..),
    newGetMobileSdkRelease,

    -- * Request Lenses
    getMobileSdkRelease_platform,
    getMobileSdkRelease_releaseVersion,

    -- * Destructuring the Response
    GetMobileSdkReleaseResponse (..),
    newGetMobileSdkReleaseResponse,

    -- * Response Lenses
    getMobileSdkReleaseResponse_mobileSdkRelease,
    getMobileSdkReleaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetMobileSdkRelease' smart constructor.
data GetMobileSdkRelease = GetMobileSdkRelease'
  { -- | The device platform.
    platform :: Platform,
    -- | The release version. For the latest available version, specify @LATEST@.
    releaseVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMobileSdkRelease' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'getMobileSdkRelease_platform' - The device platform.
--
-- 'releaseVersion', 'getMobileSdkRelease_releaseVersion' - The release version. For the latest available version, specify @LATEST@.
newGetMobileSdkRelease ::
  -- | 'platform'
  Platform ->
  -- | 'releaseVersion'
  Prelude.Text ->
  GetMobileSdkRelease
newGetMobileSdkRelease pPlatform_ pReleaseVersion_ =
  GetMobileSdkRelease'
    { platform = pPlatform_,
      releaseVersion = pReleaseVersion_
    }

-- | The device platform.
getMobileSdkRelease_platform :: Lens.Lens' GetMobileSdkRelease Platform
getMobileSdkRelease_platform = Lens.lens (\GetMobileSdkRelease' {platform} -> platform) (\s@GetMobileSdkRelease' {} a -> s {platform = a} :: GetMobileSdkRelease)

-- | The release version. For the latest available version, specify @LATEST@.
getMobileSdkRelease_releaseVersion :: Lens.Lens' GetMobileSdkRelease Prelude.Text
getMobileSdkRelease_releaseVersion = Lens.lens (\GetMobileSdkRelease' {releaseVersion} -> releaseVersion) (\s@GetMobileSdkRelease' {} a -> s {releaseVersion = a} :: GetMobileSdkRelease)

instance Core.AWSRequest GetMobileSdkRelease where
  type
    AWSResponse GetMobileSdkRelease =
      GetMobileSdkReleaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMobileSdkReleaseResponse'
            Prelude.<$> (x Core..?> "MobileSdkRelease")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMobileSdkRelease where
  hashWithSalt _salt GetMobileSdkRelease' {..} =
    _salt `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` releaseVersion

instance Prelude.NFData GetMobileSdkRelease where
  rnf GetMobileSdkRelease' {..} =
    Prelude.rnf platform
      `Prelude.seq` Prelude.rnf releaseVersion

instance Core.ToHeaders GetMobileSdkRelease where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20190729.GetMobileSdkRelease" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMobileSdkRelease where
  toJSON GetMobileSdkRelease' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Platform" Core..= platform),
            Prelude.Just
              ("ReleaseVersion" Core..= releaseVersion)
          ]
      )

instance Core.ToPath GetMobileSdkRelease where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMobileSdkRelease where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMobileSdkReleaseResponse' smart constructor.
data GetMobileSdkReleaseResponse = GetMobileSdkReleaseResponse'
  { -- | Information for a specified SDK release, including release notes and
    -- tags.
    mobileSdkRelease :: Prelude.Maybe MobileSdkRelease,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMobileSdkReleaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mobileSdkRelease', 'getMobileSdkReleaseResponse_mobileSdkRelease' - Information for a specified SDK release, including release notes and
-- tags.
--
-- 'httpStatus', 'getMobileSdkReleaseResponse_httpStatus' - The response's http status code.
newGetMobileSdkReleaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMobileSdkReleaseResponse
newGetMobileSdkReleaseResponse pHttpStatus_ =
  GetMobileSdkReleaseResponse'
    { mobileSdkRelease =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information for a specified SDK release, including release notes and
-- tags.
getMobileSdkReleaseResponse_mobileSdkRelease :: Lens.Lens' GetMobileSdkReleaseResponse (Prelude.Maybe MobileSdkRelease)
getMobileSdkReleaseResponse_mobileSdkRelease = Lens.lens (\GetMobileSdkReleaseResponse' {mobileSdkRelease} -> mobileSdkRelease) (\s@GetMobileSdkReleaseResponse' {} a -> s {mobileSdkRelease = a} :: GetMobileSdkReleaseResponse)

-- | The response's http status code.
getMobileSdkReleaseResponse_httpStatus :: Lens.Lens' GetMobileSdkReleaseResponse Prelude.Int
getMobileSdkReleaseResponse_httpStatus = Lens.lens (\GetMobileSdkReleaseResponse' {httpStatus} -> httpStatus) (\s@GetMobileSdkReleaseResponse' {} a -> s {httpStatus = a} :: GetMobileSdkReleaseResponse)

instance Prelude.NFData GetMobileSdkReleaseResponse where
  rnf GetMobileSdkReleaseResponse' {..} =
    Prelude.rnf mobileSdkRelease
      `Prelude.seq` Prelude.rnf httpStatus
