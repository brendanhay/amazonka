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
-- Module      : Amazonka.WAFV2.GenerateMobileSdkReleaseUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a presigned download URL for the specified release of the
-- mobile SDK.
--
-- The mobile SDK is not generally available. Customers who have access to
-- the mobile SDK can use it to establish and manage WAF tokens for use in
-- HTTP(S) requests from a mobile device to WAF. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
module Amazonka.WAFV2.GenerateMobileSdkReleaseUrl
  ( -- * Creating a Request
    GenerateMobileSdkReleaseUrl (..),
    newGenerateMobileSdkReleaseUrl,

    -- * Request Lenses
    generateMobileSdkReleaseUrl_platform,
    generateMobileSdkReleaseUrl_releaseVersion,

    -- * Destructuring the Response
    GenerateMobileSdkReleaseUrlResponse (..),
    newGenerateMobileSdkReleaseUrlResponse,

    -- * Response Lenses
    generateMobileSdkReleaseUrlResponse_url,
    generateMobileSdkReleaseUrlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGenerateMobileSdkReleaseUrl' smart constructor.
data GenerateMobileSdkReleaseUrl = GenerateMobileSdkReleaseUrl'
  { -- | The device platform.
    platform :: Platform,
    -- | The release version. For the latest available version, specify @LATEST@.
    releaseVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateMobileSdkReleaseUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'generateMobileSdkReleaseUrl_platform' - The device platform.
--
-- 'releaseVersion', 'generateMobileSdkReleaseUrl_releaseVersion' - The release version. For the latest available version, specify @LATEST@.
newGenerateMobileSdkReleaseUrl ::
  -- | 'platform'
  Platform ->
  -- | 'releaseVersion'
  Prelude.Text ->
  GenerateMobileSdkReleaseUrl
newGenerateMobileSdkReleaseUrl
  pPlatform_
  pReleaseVersion_ =
    GenerateMobileSdkReleaseUrl'
      { platform = pPlatform_,
        releaseVersion = pReleaseVersion_
      }

-- | The device platform.
generateMobileSdkReleaseUrl_platform :: Lens.Lens' GenerateMobileSdkReleaseUrl Platform
generateMobileSdkReleaseUrl_platform = Lens.lens (\GenerateMobileSdkReleaseUrl' {platform} -> platform) (\s@GenerateMobileSdkReleaseUrl' {} a -> s {platform = a} :: GenerateMobileSdkReleaseUrl)

-- | The release version. For the latest available version, specify @LATEST@.
generateMobileSdkReleaseUrl_releaseVersion :: Lens.Lens' GenerateMobileSdkReleaseUrl Prelude.Text
generateMobileSdkReleaseUrl_releaseVersion = Lens.lens (\GenerateMobileSdkReleaseUrl' {releaseVersion} -> releaseVersion) (\s@GenerateMobileSdkReleaseUrl' {} a -> s {releaseVersion = a} :: GenerateMobileSdkReleaseUrl)

instance Core.AWSRequest GenerateMobileSdkReleaseUrl where
  type
    AWSResponse GenerateMobileSdkReleaseUrl =
      GenerateMobileSdkReleaseUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateMobileSdkReleaseUrlResponse'
            Prelude.<$> (x Data..?> "Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateMobileSdkReleaseUrl where
  hashWithSalt _salt GenerateMobileSdkReleaseUrl' {..} =
    _salt
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` releaseVersion

instance Prelude.NFData GenerateMobileSdkReleaseUrl where
  rnf GenerateMobileSdkReleaseUrl' {..} =
    Prelude.rnf platform
      `Prelude.seq` Prelude.rnf releaseVersion

instance Data.ToHeaders GenerateMobileSdkReleaseUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GenerateMobileSdkReleaseUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateMobileSdkReleaseUrl where
  toJSON GenerateMobileSdkReleaseUrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Platform" Data..= platform),
            Prelude.Just
              ("ReleaseVersion" Data..= releaseVersion)
          ]
      )

instance Data.ToPath GenerateMobileSdkReleaseUrl where
  toPath = Prelude.const "/"

instance Data.ToQuery GenerateMobileSdkReleaseUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateMobileSdkReleaseUrlResponse' smart constructor.
data GenerateMobileSdkReleaseUrlResponse = GenerateMobileSdkReleaseUrlResponse'
  { -- | The presigned download URL for the specified SDK release.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateMobileSdkReleaseUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'generateMobileSdkReleaseUrlResponse_url' - The presigned download URL for the specified SDK release.
--
-- 'httpStatus', 'generateMobileSdkReleaseUrlResponse_httpStatus' - The response's http status code.
newGenerateMobileSdkReleaseUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateMobileSdkReleaseUrlResponse
newGenerateMobileSdkReleaseUrlResponse pHttpStatus_ =
  GenerateMobileSdkReleaseUrlResponse'
    { url =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The presigned download URL for the specified SDK release.
generateMobileSdkReleaseUrlResponse_url :: Lens.Lens' GenerateMobileSdkReleaseUrlResponse (Prelude.Maybe Prelude.Text)
generateMobileSdkReleaseUrlResponse_url = Lens.lens (\GenerateMobileSdkReleaseUrlResponse' {url} -> url) (\s@GenerateMobileSdkReleaseUrlResponse' {} a -> s {url = a} :: GenerateMobileSdkReleaseUrlResponse)

-- | The response's http status code.
generateMobileSdkReleaseUrlResponse_httpStatus :: Lens.Lens' GenerateMobileSdkReleaseUrlResponse Prelude.Int
generateMobileSdkReleaseUrlResponse_httpStatus = Lens.lens (\GenerateMobileSdkReleaseUrlResponse' {httpStatus} -> httpStatus) (\s@GenerateMobileSdkReleaseUrlResponse' {} a -> s {httpStatus = a} :: GenerateMobileSdkReleaseUrlResponse)

instance
  Prelude.NFData
    GenerateMobileSdkReleaseUrlResponse
  where
  rnf GenerateMobileSdkReleaseUrlResponse' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf httpStatus
