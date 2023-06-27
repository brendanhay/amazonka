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
-- Module      : Amazonka.TNB.GetSolFunctionPackageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of a function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
module Amazonka.TNB.GetSolFunctionPackageContent
  ( -- * Creating a Request
    GetSolFunctionPackageContent (..),
    newGetSolFunctionPackageContent,

    -- * Request Lenses
    getSolFunctionPackageContent_accept,
    getSolFunctionPackageContent_vnfPkgId,

    -- * Destructuring the Response
    GetSolFunctionPackageContentResponse (..),
    newGetSolFunctionPackageContentResponse,

    -- * Response Lenses
    getSolFunctionPackageContentResponse_contentType,
    getSolFunctionPackageContentResponse_packageContent,
    getSolFunctionPackageContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolFunctionPackageContent' smart constructor.
data GetSolFunctionPackageContent = GetSolFunctionPackageContent'
  { -- | The format of the package that you want to download from the function
    -- packages.
    accept :: PackageContentType,
    -- | ID of the function package.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionPackageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'getSolFunctionPackageContent_accept' - The format of the package that you want to download from the function
-- packages.
--
-- 'vnfPkgId', 'getSolFunctionPackageContent_vnfPkgId' - ID of the function package.
newGetSolFunctionPackageContent ::
  -- | 'accept'
  PackageContentType ->
  -- | 'vnfPkgId'
  Prelude.Text ->
  GetSolFunctionPackageContent
newGetSolFunctionPackageContent pAccept_ pVnfPkgId_ =
  GetSolFunctionPackageContent'
    { accept = pAccept_,
      vnfPkgId = pVnfPkgId_
    }

-- | The format of the package that you want to download from the function
-- packages.
getSolFunctionPackageContent_accept :: Lens.Lens' GetSolFunctionPackageContent PackageContentType
getSolFunctionPackageContent_accept = Lens.lens (\GetSolFunctionPackageContent' {accept} -> accept) (\s@GetSolFunctionPackageContent' {} a -> s {accept = a} :: GetSolFunctionPackageContent)

-- | ID of the function package.
getSolFunctionPackageContent_vnfPkgId :: Lens.Lens' GetSolFunctionPackageContent Prelude.Text
getSolFunctionPackageContent_vnfPkgId = Lens.lens (\GetSolFunctionPackageContent' {vnfPkgId} -> vnfPkgId) (\s@GetSolFunctionPackageContent' {} a -> s {vnfPkgId = a} :: GetSolFunctionPackageContent)

instance Core.AWSRequest GetSolFunctionPackageContent where
  type
    AWSResponse GetSolFunctionPackageContent =
      GetSolFunctionPackageContentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetSolFunctionPackageContentResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSolFunctionPackageContent
  where
  hashWithSalt _salt GetSolFunctionPackageContent' {..} =
    _salt
      `Prelude.hashWithSalt` accept
      `Prelude.hashWithSalt` vnfPkgId

instance Prelude.NFData GetSolFunctionPackageContent where
  rnf GetSolFunctionPackageContent' {..} =
    Prelude.rnf accept
      `Prelude.seq` Prelude.rnf vnfPkgId

instance Data.ToHeaders GetSolFunctionPackageContent where
  toHeaders GetSolFunctionPackageContent' {..} =
    Prelude.mconcat
      [ "Accept" Data.=# accept,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetSolFunctionPackageContent where
  toPath GetSolFunctionPackageContent' {..} =
    Prelude.mconcat
      [ "/sol/vnfpkgm/v1/vnf_packages/",
        Data.toBS vnfPkgId,
        "/package_content"
      ]

instance Data.ToQuery GetSolFunctionPackageContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolFunctionPackageContentResponse' smart constructor.
data GetSolFunctionPackageContentResponse = GetSolFunctionPackageContentResponse'
  { -- | Indicates the media type of the resource.
    contentType :: Prelude.Maybe PackageContentType,
    -- | Contents of the function package.
    packageContent :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionPackageContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getSolFunctionPackageContentResponse_contentType' - Indicates the media type of the resource.
--
-- 'packageContent', 'getSolFunctionPackageContentResponse_packageContent' - Contents of the function package.
--
-- 'httpStatus', 'getSolFunctionPackageContentResponse_httpStatus' - The response's http status code.
newGetSolFunctionPackageContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSolFunctionPackageContentResponse
newGetSolFunctionPackageContentResponse pHttpStatus_ =
  GetSolFunctionPackageContentResponse'
    { contentType =
        Prelude.Nothing,
      packageContent = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the media type of the resource.
getSolFunctionPackageContentResponse_contentType :: Lens.Lens' GetSolFunctionPackageContentResponse (Prelude.Maybe PackageContentType)
getSolFunctionPackageContentResponse_contentType = Lens.lens (\GetSolFunctionPackageContentResponse' {contentType} -> contentType) (\s@GetSolFunctionPackageContentResponse' {} a -> s {contentType = a} :: GetSolFunctionPackageContentResponse)

-- | Contents of the function package.
getSolFunctionPackageContentResponse_packageContent :: Lens.Lens' GetSolFunctionPackageContentResponse (Prelude.Maybe Prelude.ByteString)
getSolFunctionPackageContentResponse_packageContent = Lens.lens (\GetSolFunctionPackageContentResponse' {packageContent} -> packageContent) (\s@GetSolFunctionPackageContentResponse' {} a -> s {packageContent = a} :: GetSolFunctionPackageContentResponse)

-- | The response's http status code.
getSolFunctionPackageContentResponse_httpStatus :: Lens.Lens' GetSolFunctionPackageContentResponse Prelude.Int
getSolFunctionPackageContentResponse_httpStatus = Lens.lens (\GetSolFunctionPackageContentResponse' {httpStatus} -> httpStatus) (\s@GetSolFunctionPackageContentResponse' {} a -> s {httpStatus = a} :: GetSolFunctionPackageContentResponse)

instance
  Prelude.NFData
    GetSolFunctionPackageContentResponse
  where
  rnf GetSolFunctionPackageContentResponse' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf packageContent
      `Prelude.seq` Prelude.rnf httpStatus
