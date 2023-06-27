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
-- Module      : Amazonka.TNB.PutSolFunctionPackageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the contents of a function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
module Amazonka.TNB.PutSolFunctionPackageContent
  ( -- * Creating a Request
    PutSolFunctionPackageContent (..),
    newPutSolFunctionPackageContent,

    -- * Request Lenses
    putSolFunctionPackageContent_contentType,
    putSolFunctionPackageContent_file,
    putSolFunctionPackageContent_vnfPkgId,

    -- * Destructuring the Response
    PutSolFunctionPackageContentResponse (..),
    newPutSolFunctionPackageContentResponse,

    -- * Response Lenses
    putSolFunctionPackageContentResponse_httpStatus,
    putSolFunctionPackageContentResponse_id,
    putSolFunctionPackageContentResponse_metadata,
    putSolFunctionPackageContentResponse_vnfProductName,
    putSolFunctionPackageContentResponse_vnfProvider,
    putSolFunctionPackageContentResponse_vnfdId,
    putSolFunctionPackageContentResponse_vnfdVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newPutSolFunctionPackageContent' smart constructor.
data PutSolFunctionPackageContent = PutSolFunctionPackageContent'
  { -- | Function package content type.
    contentType :: Prelude.Maybe PackageContentType,
    -- | Function package file.
    file :: Prelude.ByteString,
    -- | Function package ID.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSolFunctionPackageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'putSolFunctionPackageContent_contentType' - Function package content type.
--
-- 'file', 'putSolFunctionPackageContent_file' - Function package file.
--
-- 'vnfPkgId', 'putSolFunctionPackageContent_vnfPkgId' - Function package ID.
newPutSolFunctionPackageContent ::
  -- | 'file'
  Prelude.ByteString ->
  -- | 'vnfPkgId'
  Prelude.Text ->
  PutSolFunctionPackageContent
newPutSolFunctionPackageContent pFile_ pVnfPkgId_ =
  PutSolFunctionPackageContent'
    { contentType =
        Prelude.Nothing,
      file = pFile_,
      vnfPkgId = pVnfPkgId_
    }

-- | Function package content type.
putSolFunctionPackageContent_contentType :: Lens.Lens' PutSolFunctionPackageContent (Prelude.Maybe PackageContentType)
putSolFunctionPackageContent_contentType = Lens.lens (\PutSolFunctionPackageContent' {contentType} -> contentType) (\s@PutSolFunctionPackageContent' {} a -> s {contentType = a} :: PutSolFunctionPackageContent)

-- | Function package file.
putSolFunctionPackageContent_file :: Lens.Lens' PutSolFunctionPackageContent Prelude.ByteString
putSolFunctionPackageContent_file = Lens.lens (\PutSolFunctionPackageContent' {file} -> file) (\s@PutSolFunctionPackageContent' {} a -> s {file = a} :: PutSolFunctionPackageContent)

-- | Function package ID.
putSolFunctionPackageContent_vnfPkgId :: Lens.Lens' PutSolFunctionPackageContent Prelude.Text
putSolFunctionPackageContent_vnfPkgId = Lens.lens (\PutSolFunctionPackageContent' {vnfPkgId} -> vnfPkgId) (\s@PutSolFunctionPackageContent' {} a -> s {vnfPkgId = a} :: PutSolFunctionPackageContent)

instance Core.AWSRequest PutSolFunctionPackageContent where
  type
    AWSResponse PutSolFunctionPackageContent =
      PutSolFunctionPackageContentResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSolFunctionPackageContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "metadata")
            Prelude.<*> (x Data..:> "vnfProductName")
            Prelude.<*> (x Data..:> "vnfProvider")
            Prelude.<*> (x Data..:> "vnfdId")
            Prelude.<*> (x Data..:> "vnfdVersion")
      )

instance
  Prelude.Hashable
    PutSolFunctionPackageContent
  where
  hashWithSalt _salt PutSolFunctionPackageContent' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` vnfPkgId

instance Prelude.NFData PutSolFunctionPackageContent where
  rnf PutSolFunctionPackageContent' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf vnfPkgId

instance Data.ToBody PutSolFunctionPackageContent where
  toBody PutSolFunctionPackageContent' {..} =
    Data.toBody file

instance Data.ToHeaders PutSolFunctionPackageContent where
  toHeaders PutSolFunctionPackageContent' {..} =
    Prelude.mconcat
      ["Content-Type" Data.=# contentType]

instance Data.ToPath PutSolFunctionPackageContent where
  toPath PutSolFunctionPackageContent' {..} =
    Prelude.mconcat
      [ "/sol/vnfpkgm/v1/vnf_packages/",
        Data.toBS vnfPkgId,
        "/package_content"
      ]

instance Data.ToQuery PutSolFunctionPackageContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSolFunctionPackageContentResponse' smart constructor.
data PutSolFunctionPackageContentResponse = PutSolFunctionPackageContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Function package ID.
    id :: Prelude.Text,
    -- | Function package metadata.
    metadata :: PutSolFunctionPackageContentMetadata,
    -- | Function product name.
    vnfProductName :: Prelude.Text,
    -- | Function provider.
    vnfProvider :: Prelude.Text,
    -- | Function package descriptor ID.
    vnfdId :: Prelude.Text,
    -- | Function package descriptor version.
    vnfdVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSolFunctionPackageContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putSolFunctionPackageContentResponse_httpStatus' - The response's http status code.
--
-- 'id', 'putSolFunctionPackageContentResponse_id' - Function package ID.
--
-- 'metadata', 'putSolFunctionPackageContentResponse_metadata' - Function package metadata.
--
-- 'vnfProductName', 'putSolFunctionPackageContentResponse_vnfProductName' - Function product name.
--
-- 'vnfProvider', 'putSolFunctionPackageContentResponse_vnfProvider' - Function provider.
--
-- 'vnfdId', 'putSolFunctionPackageContentResponse_vnfdId' - Function package descriptor ID.
--
-- 'vnfdVersion', 'putSolFunctionPackageContentResponse_vnfdVersion' - Function package descriptor version.
newPutSolFunctionPackageContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  PutSolFunctionPackageContentMetadata ->
  -- | 'vnfProductName'
  Prelude.Text ->
  -- | 'vnfProvider'
  Prelude.Text ->
  -- | 'vnfdId'
  Prelude.Text ->
  -- | 'vnfdVersion'
  Prelude.Text ->
  PutSolFunctionPackageContentResponse
newPutSolFunctionPackageContentResponse
  pHttpStatus_
  pId_
  pMetadata_
  pVnfProductName_
  pVnfProvider_
  pVnfdId_
  pVnfdVersion_ =
    PutSolFunctionPackageContentResponse'
      { httpStatus =
          pHttpStatus_,
        id = pId_,
        metadata = pMetadata_,
        vnfProductName = pVnfProductName_,
        vnfProvider = pVnfProvider_,
        vnfdId = pVnfdId_,
        vnfdVersion = pVnfdVersion_
      }

-- | The response's http status code.
putSolFunctionPackageContentResponse_httpStatus :: Lens.Lens' PutSolFunctionPackageContentResponse Prelude.Int
putSolFunctionPackageContentResponse_httpStatus = Lens.lens (\PutSolFunctionPackageContentResponse' {httpStatus} -> httpStatus) (\s@PutSolFunctionPackageContentResponse' {} a -> s {httpStatus = a} :: PutSolFunctionPackageContentResponse)

-- | Function package ID.
putSolFunctionPackageContentResponse_id :: Lens.Lens' PutSolFunctionPackageContentResponse Prelude.Text
putSolFunctionPackageContentResponse_id = Lens.lens (\PutSolFunctionPackageContentResponse' {id} -> id) (\s@PutSolFunctionPackageContentResponse' {} a -> s {id = a} :: PutSolFunctionPackageContentResponse)

-- | Function package metadata.
putSolFunctionPackageContentResponse_metadata :: Lens.Lens' PutSolFunctionPackageContentResponse PutSolFunctionPackageContentMetadata
putSolFunctionPackageContentResponse_metadata = Lens.lens (\PutSolFunctionPackageContentResponse' {metadata} -> metadata) (\s@PutSolFunctionPackageContentResponse' {} a -> s {metadata = a} :: PutSolFunctionPackageContentResponse)

-- | Function product name.
putSolFunctionPackageContentResponse_vnfProductName :: Lens.Lens' PutSolFunctionPackageContentResponse Prelude.Text
putSolFunctionPackageContentResponse_vnfProductName = Lens.lens (\PutSolFunctionPackageContentResponse' {vnfProductName} -> vnfProductName) (\s@PutSolFunctionPackageContentResponse' {} a -> s {vnfProductName = a} :: PutSolFunctionPackageContentResponse)

-- | Function provider.
putSolFunctionPackageContentResponse_vnfProvider :: Lens.Lens' PutSolFunctionPackageContentResponse Prelude.Text
putSolFunctionPackageContentResponse_vnfProvider = Lens.lens (\PutSolFunctionPackageContentResponse' {vnfProvider} -> vnfProvider) (\s@PutSolFunctionPackageContentResponse' {} a -> s {vnfProvider = a} :: PutSolFunctionPackageContentResponse)

-- | Function package descriptor ID.
putSolFunctionPackageContentResponse_vnfdId :: Lens.Lens' PutSolFunctionPackageContentResponse Prelude.Text
putSolFunctionPackageContentResponse_vnfdId = Lens.lens (\PutSolFunctionPackageContentResponse' {vnfdId} -> vnfdId) (\s@PutSolFunctionPackageContentResponse' {} a -> s {vnfdId = a} :: PutSolFunctionPackageContentResponse)

-- | Function package descriptor version.
putSolFunctionPackageContentResponse_vnfdVersion :: Lens.Lens' PutSolFunctionPackageContentResponse Prelude.Text
putSolFunctionPackageContentResponse_vnfdVersion = Lens.lens (\PutSolFunctionPackageContentResponse' {vnfdVersion} -> vnfdVersion) (\s@PutSolFunctionPackageContentResponse' {} a -> s {vnfdVersion = a} :: PutSolFunctionPackageContentResponse)

instance
  Prelude.NFData
    PutSolFunctionPackageContentResponse
  where
  rnf PutSolFunctionPackageContentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf vnfProductName
      `Prelude.seq` Prelude.rnf vnfProvider
      `Prelude.seq` Prelude.rnf vnfdId
      `Prelude.seq` Prelude.rnf vnfdVersion
