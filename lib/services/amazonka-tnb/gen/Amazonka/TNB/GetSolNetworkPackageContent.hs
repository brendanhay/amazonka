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
-- Module      : Amazonka.TNB.GetSolNetworkPackageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
module Amazonka.TNB.GetSolNetworkPackageContent
  ( -- * Creating a Request
    GetSolNetworkPackageContent (..),
    newGetSolNetworkPackageContent,

    -- * Request Lenses
    getSolNetworkPackageContent_accept,
    getSolNetworkPackageContent_nsdInfoId,

    -- * Destructuring the Response
    GetSolNetworkPackageContentResponse (..),
    newGetSolNetworkPackageContentResponse,

    -- * Response Lenses
    getSolNetworkPackageContentResponse_contentType,
    getSolNetworkPackageContentResponse_nsdContent,
    getSolNetworkPackageContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolNetworkPackageContent' smart constructor.
data GetSolNetworkPackageContent = GetSolNetworkPackageContent'
  { -- | The format of the package you want to download from the network package.
    accept :: PackageContentType,
    -- | ID of the network service descriptor in the network package.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkPackageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'getSolNetworkPackageContent_accept' - The format of the package you want to download from the network package.
--
-- 'nsdInfoId', 'getSolNetworkPackageContent_nsdInfoId' - ID of the network service descriptor in the network package.
newGetSolNetworkPackageContent ::
  -- | 'accept'
  PackageContentType ->
  -- | 'nsdInfoId'
  Prelude.Text ->
  GetSolNetworkPackageContent
newGetSolNetworkPackageContent pAccept_ pNsdInfoId_ =
  GetSolNetworkPackageContent'
    { accept = pAccept_,
      nsdInfoId = pNsdInfoId_
    }

-- | The format of the package you want to download from the network package.
getSolNetworkPackageContent_accept :: Lens.Lens' GetSolNetworkPackageContent PackageContentType
getSolNetworkPackageContent_accept = Lens.lens (\GetSolNetworkPackageContent' {accept} -> accept) (\s@GetSolNetworkPackageContent' {} a -> s {accept = a} :: GetSolNetworkPackageContent)

-- | ID of the network service descriptor in the network package.
getSolNetworkPackageContent_nsdInfoId :: Lens.Lens' GetSolNetworkPackageContent Prelude.Text
getSolNetworkPackageContent_nsdInfoId = Lens.lens (\GetSolNetworkPackageContent' {nsdInfoId} -> nsdInfoId) (\s@GetSolNetworkPackageContent' {} a -> s {nsdInfoId = a} :: GetSolNetworkPackageContent)

instance Core.AWSRequest GetSolNetworkPackageContent where
  type
    AWSResponse GetSolNetworkPackageContent =
      GetSolNetworkPackageContentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetSolNetworkPackageContentResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSolNetworkPackageContent where
  hashWithSalt _salt GetSolNetworkPackageContent' {..} =
    _salt
      `Prelude.hashWithSalt` accept
      `Prelude.hashWithSalt` nsdInfoId

instance Prelude.NFData GetSolNetworkPackageContent where
  rnf GetSolNetworkPackageContent' {..} =
    Prelude.rnf accept
      `Prelude.seq` Prelude.rnf nsdInfoId

instance Data.ToHeaders GetSolNetworkPackageContent where
  toHeaders GetSolNetworkPackageContent' {..} =
    Prelude.mconcat
      [ "Accept" Data.=# accept,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetSolNetworkPackageContent where
  toPath GetSolNetworkPackageContent' {..} =
    Prelude.mconcat
      [ "/sol/nsd/v1/ns_descriptors/",
        Data.toBS nsdInfoId,
        "/nsd_content"
      ]

instance Data.ToQuery GetSolNetworkPackageContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolNetworkPackageContentResponse' smart constructor.
data GetSolNetworkPackageContentResponse = GetSolNetworkPackageContentResponse'
  { -- | Indicates the media type of the resource.
    contentType :: Prelude.Maybe PackageContentType,
    -- | Content of the network service descriptor in the network package.
    nsdContent :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkPackageContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getSolNetworkPackageContentResponse_contentType' - Indicates the media type of the resource.
--
-- 'nsdContent', 'getSolNetworkPackageContentResponse_nsdContent' - Content of the network service descriptor in the network package.
--
-- 'httpStatus', 'getSolNetworkPackageContentResponse_httpStatus' - The response's http status code.
newGetSolNetworkPackageContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSolNetworkPackageContentResponse
newGetSolNetworkPackageContentResponse pHttpStatus_ =
  GetSolNetworkPackageContentResponse'
    { contentType =
        Prelude.Nothing,
      nsdContent = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the media type of the resource.
getSolNetworkPackageContentResponse_contentType :: Lens.Lens' GetSolNetworkPackageContentResponse (Prelude.Maybe PackageContentType)
getSolNetworkPackageContentResponse_contentType = Lens.lens (\GetSolNetworkPackageContentResponse' {contentType} -> contentType) (\s@GetSolNetworkPackageContentResponse' {} a -> s {contentType = a} :: GetSolNetworkPackageContentResponse)

-- | Content of the network service descriptor in the network package.
getSolNetworkPackageContentResponse_nsdContent :: Lens.Lens' GetSolNetworkPackageContentResponse (Prelude.Maybe Prelude.ByteString)
getSolNetworkPackageContentResponse_nsdContent = Lens.lens (\GetSolNetworkPackageContentResponse' {nsdContent} -> nsdContent) (\s@GetSolNetworkPackageContentResponse' {} a -> s {nsdContent = a} :: GetSolNetworkPackageContentResponse)

-- | The response's http status code.
getSolNetworkPackageContentResponse_httpStatus :: Lens.Lens' GetSolNetworkPackageContentResponse Prelude.Int
getSolNetworkPackageContentResponse_httpStatus = Lens.lens (\GetSolNetworkPackageContentResponse' {httpStatus} -> httpStatus) (\s@GetSolNetworkPackageContentResponse' {} a -> s {httpStatus = a} :: GetSolNetworkPackageContentResponse)

instance
  Prelude.NFData
    GetSolNetworkPackageContentResponse
  where
  rnf GetSolNetworkPackageContentResponse' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf nsdContent
      `Prelude.seq` Prelude.rnf httpStatus
