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
-- Module      : Amazonka.TNB.PutSolNetworkPackageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the contents of a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
module Amazonka.TNB.PutSolNetworkPackageContent
  ( -- * Creating a Request
    PutSolNetworkPackageContent (..),
    newPutSolNetworkPackageContent,

    -- * Request Lenses
    putSolNetworkPackageContent_contentType,
    putSolNetworkPackageContent_file,
    putSolNetworkPackageContent_nsdInfoId,

    -- * Destructuring the Response
    PutSolNetworkPackageContentResponse (..),
    newPutSolNetworkPackageContentResponse,

    -- * Response Lenses
    putSolNetworkPackageContentResponse_httpStatus,
    putSolNetworkPackageContentResponse_arn,
    putSolNetworkPackageContentResponse_id,
    putSolNetworkPackageContentResponse_metadata,
    putSolNetworkPackageContentResponse_nsdId,
    putSolNetworkPackageContentResponse_nsdName,
    putSolNetworkPackageContentResponse_nsdVersion,
    putSolNetworkPackageContentResponse_vnfPkgIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newPutSolNetworkPackageContent' smart constructor.
data PutSolNetworkPackageContent = PutSolNetworkPackageContent'
  { -- | Network package content type.
    contentType :: Prelude.Maybe PackageContentType,
    -- | Network package file.
    file :: Prelude.ByteString,
    -- | Network service descriptor info ID.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSolNetworkPackageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'putSolNetworkPackageContent_contentType' - Network package content type.
--
-- 'file', 'putSolNetworkPackageContent_file' - Network package file.
--
-- 'nsdInfoId', 'putSolNetworkPackageContent_nsdInfoId' - Network service descriptor info ID.
newPutSolNetworkPackageContent ::
  -- | 'file'
  Prelude.ByteString ->
  -- | 'nsdInfoId'
  Prelude.Text ->
  PutSolNetworkPackageContent
newPutSolNetworkPackageContent pFile_ pNsdInfoId_ =
  PutSolNetworkPackageContent'
    { contentType =
        Prelude.Nothing,
      file = pFile_,
      nsdInfoId = pNsdInfoId_
    }

-- | Network package content type.
putSolNetworkPackageContent_contentType :: Lens.Lens' PutSolNetworkPackageContent (Prelude.Maybe PackageContentType)
putSolNetworkPackageContent_contentType = Lens.lens (\PutSolNetworkPackageContent' {contentType} -> contentType) (\s@PutSolNetworkPackageContent' {} a -> s {contentType = a} :: PutSolNetworkPackageContent)

-- | Network package file.
putSolNetworkPackageContent_file :: Lens.Lens' PutSolNetworkPackageContent Prelude.ByteString
putSolNetworkPackageContent_file = Lens.lens (\PutSolNetworkPackageContent' {file} -> file) (\s@PutSolNetworkPackageContent' {} a -> s {file = a} :: PutSolNetworkPackageContent)

-- | Network service descriptor info ID.
putSolNetworkPackageContent_nsdInfoId :: Lens.Lens' PutSolNetworkPackageContent Prelude.Text
putSolNetworkPackageContent_nsdInfoId = Lens.lens (\PutSolNetworkPackageContent' {nsdInfoId} -> nsdInfoId) (\s@PutSolNetworkPackageContent' {} a -> s {nsdInfoId = a} :: PutSolNetworkPackageContent)

instance Core.AWSRequest PutSolNetworkPackageContent where
  type
    AWSResponse PutSolNetworkPackageContent =
      PutSolNetworkPackageContentResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSolNetworkPackageContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "metadata")
            Prelude.<*> (x Data..:> "nsdId")
            Prelude.<*> (x Data..:> "nsdName")
            Prelude.<*> (x Data..:> "nsdVersion")
            Prelude.<*> (x Data..?> "vnfPkgIds" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable PutSolNetworkPackageContent where
  hashWithSalt _salt PutSolNetworkPackageContent' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` nsdInfoId

instance Prelude.NFData PutSolNetworkPackageContent where
  rnf PutSolNetworkPackageContent' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf nsdInfoId

instance Data.ToBody PutSolNetworkPackageContent where
  toBody PutSolNetworkPackageContent' {..} =
    Data.toBody file

instance Data.ToHeaders PutSolNetworkPackageContent where
  toHeaders PutSolNetworkPackageContent' {..} =
    Prelude.mconcat
      ["Content-Type" Data.=# contentType]

instance Data.ToPath PutSolNetworkPackageContent where
  toPath PutSolNetworkPackageContent' {..} =
    Prelude.mconcat
      [ "/sol/nsd/v1/ns_descriptors/",
        Data.toBS nsdInfoId,
        "/nsd_content"
      ]

instance Data.ToQuery PutSolNetworkPackageContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSolNetworkPackageContentResponse' smart constructor.
data PutSolNetworkPackageContentResponse = PutSolNetworkPackageContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network package ARN.
    arn :: Prelude.Text,
    -- | Network package ID.
    id :: Prelude.Text,
    -- | Network package metadata.
    metadata :: PutSolNetworkPackageContentMetadata,
    -- | Network service descriptor ID.
    nsdId :: Prelude.Text,
    -- | Network service descriptor name.
    nsdName :: Prelude.Text,
    -- | Network service descriptor version.
    nsdVersion :: Prelude.Text,
    -- | Function package IDs.
    vnfPkgIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSolNetworkPackageContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putSolNetworkPackageContentResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'putSolNetworkPackageContentResponse_arn' - Network package ARN.
--
-- 'id', 'putSolNetworkPackageContentResponse_id' - Network package ID.
--
-- 'metadata', 'putSolNetworkPackageContentResponse_metadata' - Network package metadata.
--
-- 'nsdId', 'putSolNetworkPackageContentResponse_nsdId' - Network service descriptor ID.
--
-- 'nsdName', 'putSolNetworkPackageContentResponse_nsdName' - Network service descriptor name.
--
-- 'nsdVersion', 'putSolNetworkPackageContentResponse_nsdVersion' - Network service descriptor version.
--
-- 'vnfPkgIds', 'putSolNetworkPackageContentResponse_vnfPkgIds' - Function package IDs.
newPutSolNetworkPackageContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  PutSolNetworkPackageContentMetadata ->
  -- | 'nsdId'
  Prelude.Text ->
  -- | 'nsdName'
  Prelude.Text ->
  -- | 'nsdVersion'
  Prelude.Text ->
  PutSolNetworkPackageContentResponse
newPutSolNetworkPackageContentResponse
  pHttpStatus_
  pArn_
  pId_
  pMetadata_
  pNsdId_
  pNsdName_
  pNsdVersion_ =
    PutSolNetworkPackageContentResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        id = pId_,
        metadata = pMetadata_,
        nsdId = pNsdId_,
        nsdName = pNsdName_,
        nsdVersion = pNsdVersion_,
        vnfPkgIds = Prelude.mempty
      }

-- | The response's http status code.
putSolNetworkPackageContentResponse_httpStatus :: Lens.Lens' PutSolNetworkPackageContentResponse Prelude.Int
putSolNetworkPackageContentResponse_httpStatus = Lens.lens (\PutSolNetworkPackageContentResponse' {httpStatus} -> httpStatus) (\s@PutSolNetworkPackageContentResponse' {} a -> s {httpStatus = a} :: PutSolNetworkPackageContentResponse)

-- | Network package ARN.
putSolNetworkPackageContentResponse_arn :: Lens.Lens' PutSolNetworkPackageContentResponse Prelude.Text
putSolNetworkPackageContentResponse_arn = Lens.lens (\PutSolNetworkPackageContentResponse' {arn} -> arn) (\s@PutSolNetworkPackageContentResponse' {} a -> s {arn = a} :: PutSolNetworkPackageContentResponse)

-- | Network package ID.
putSolNetworkPackageContentResponse_id :: Lens.Lens' PutSolNetworkPackageContentResponse Prelude.Text
putSolNetworkPackageContentResponse_id = Lens.lens (\PutSolNetworkPackageContentResponse' {id} -> id) (\s@PutSolNetworkPackageContentResponse' {} a -> s {id = a} :: PutSolNetworkPackageContentResponse)

-- | Network package metadata.
putSolNetworkPackageContentResponse_metadata :: Lens.Lens' PutSolNetworkPackageContentResponse PutSolNetworkPackageContentMetadata
putSolNetworkPackageContentResponse_metadata = Lens.lens (\PutSolNetworkPackageContentResponse' {metadata} -> metadata) (\s@PutSolNetworkPackageContentResponse' {} a -> s {metadata = a} :: PutSolNetworkPackageContentResponse)

-- | Network service descriptor ID.
putSolNetworkPackageContentResponse_nsdId :: Lens.Lens' PutSolNetworkPackageContentResponse Prelude.Text
putSolNetworkPackageContentResponse_nsdId = Lens.lens (\PutSolNetworkPackageContentResponse' {nsdId} -> nsdId) (\s@PutSolNetworkPackageContentResponse' {} a -> s {nsdId = a} :: PutSolNetworkPackageContentResponse)

-- | Network service descriptor name.
putSolNetworkPackageContentResponse_nsdName :: Lens.Lens' PutSolNetworkPackageContentResponse Prelude.Text
putSolNetworkPackageContentResponse_nsdName = Lens.lens (\PutSolNetworkPackageContentResponse' {nsdName} -> nsdName) (\s@PutSolNetworkPackageContentResponse' {} a -> s {nsdName = a} :: PutSolNetworkPackageContentResponse)

-- | Network service descriptor version.
putSolNetworkPackageContentResponse_nsdVersion :: Lens.Lens' PutSolNetworkPackageContentResponse Prelude.Text
putSolNetworkPackageContentResponse_nsdVersion = Lens.lens (\PutSolNetworkPackageContentResponse' {nsdVersion} -> nsdVersion) (\s@PutSolNetworkPackageContentResponse' {} a -> s {nsdVersion = a} :: PutSolNetworkPackageContentResponse)

-- | Function package IDs.
putSolNetworkPackageContentResponse_vnfPkgIds :: Lens.Lens' PutSolNetworkPackageContentResponse [Prelude.Text]
putSolNetworkPackageContentResponse_vnfPkgIds = Lens.lens (\PutSolNetworkPackageContentResponse' {vnfPkgIds} -> vnfPkgIds) (\s@PutSolNetworkPackageContentResponse' {} a -> s {vnfPkgIds = a} :: PutSolNetworkPackageContentResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    PutSolNetworkPackageContentResponse
  where
  rnf PutSolNetworkPackageContentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsdId
      `Prelude.seq` Prelude.rnf nsdName
      `Prelude.seq` Prelude.rnf nsdVersion
      `Prelude.seq` Prelude.rnf vnfPkgIds
