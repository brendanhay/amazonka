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
-- Module      : Amazonka.TNB.ValidateSolFunctionPackageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates function package content. This can be used as a dry run before
-- uploading function package content with
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_PutSolFunctionPackageContent.html PutSolFunctionPackageContent>.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
module Amazonka.TNB.ValidateSolFunctionPackageContent
  ( -- * Creating a Request
    ValidateSolFunctionPackageContent (..),
    newValidateSolFunctionPackageContent,

    -- * Request Lenses
    validateSolFunctionPackageContent_contentType,
    validateSolFunctionPackageContent_file,
    validateSolFunctionPackageContent_vnfPkgId,

    -- * Destructuring the Response
    ValidateSolFunctionPackageContentResponse (..),
    newValidateSolFunctionPackageContentResponse,

    -- * Response Lenses
    validateSolFunctionPackageContentResponse_httpStatus,
    validateSolFunctionPackageContentResponse_id,
    validateSolFunctionPackageContentResponse_metadata,
    validateSolFunctionPackageContentResponse_vnfProductName,
    validateSolFunctionPackageContentResponse_vnfProvider,
    validateSolFunctionPackageContentResponse_vnfdId,
    validateSolFunctionPackageContentResponse_vnfdVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newValidateSolFunctionPackageContent' smart constructor.
data ValidateSolFunctionPackageContent = ValidateSolFunctionPackageContent'
  { -- | Function package content type.
    contentType :: Prelude.Maybe PackageContentType,
    -- | Function package file.
    file :: Prelude.ByteString,
    -- | Function package ID.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateSolFunctionPackageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'validateSolFunctionPackageContent_contentType' - Function package content type.
--
-- 'file', 'validateSolFunctionPackageContent_file' - Function package file.
--
-- 'vnfPkgId', 'validateSolFunctionPackageContent_vnfPkgId' - Function package ID.
newValidateSolFunctionPackageContent ::
  -- | 'file'
  Prelude.ByteString ->
  -- | 'vnfPkgId'
  Prelude.Text ->
  ValidateSolFunctionPackageContent
newValidateSolFunctionPackageContent
  pFile_
  pVnfPkgId_ =
    ValidateSolFunctionPackageContent'
      { contentType =
          Prelude.Nothing,
        file = pFile_,
        vnfPkgId = pVnfPkgId_
      }

-- | Function package content type.
validateSolFunctionPackageContent_contentType :: Lens.Lens' ValidateSolFunctionPackageContent (Prelude.Maybe PackageContentType)
validateSolFunctionPackageContent_contentType = Lens.lens (\ValidateSolFunctionPackageContent' {contentType} -> contentType) (\s@ValidateSolFunctionPackageContent' {} a -> s {contentType = a} :: ValidateSolFunctionPackageContent)

-- | Function package file.
validateSolFunctionPackageContent_file :: Lens.Lens' ValidateSolFunctionPackageContent Prelude.ByteString
validateSolFunctionPackageContent_file = Lens.lens (\ValidateSolFunctionPackageContent' {file} -> file) (\s@ValidateSolFunctionPackageContent' {} a -> s {file = a} :: ValidateSolFunctionPackageContent)

-- | Function package ID.
validateSolFunctionPackageContent_vnfPkgId :: Lens.Lens' ValidateSolFunctionPackageContent Prelude.Text
validateSolFunctionPackageContent_vnfPkgId = Lens.lens (\ValidateSolFunctionPackageContent' {vnfPkgId} -> vnfPkgId) (\s@ValidateSolFunctionPackageContent' {} a -> s {vnfPkgId = a} :: ValidateSolFunctionPackageContent)

instance
  Core.AWSRequest
    ValidateSolFunctionPackageContent
  where
  type
    AWSResponse ValidateSolFunctionPackageContent =
      ValidateSolFunctionPackageContentResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateSolFunctionPackageContentResponse'
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
    ValidateSolFunctionPackageContent
  where
  hashWithSalt
    _salt
    ValidateSolFunctionPackageContent' {..} =
      _salt
        `Prelude.hashWithSalt` contentType
        `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` vnfPkgId

instance
  Prelude.NFData
    ValidateSolFunctionPackageContent
  where
  rnf ValidateSolFunctionPackageContent' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf vnfPkgId

instance
  Data.ToBody
    ValidateSolFunctionPackageContent
  where
  toBody ValidateSolFunctionPackageContent' {..} =
    Data.toBody file

instance
  Data.ToHeaders
    ValidateSolFunctionPackageContent
  where
  toHeaders ValidateSolFunctionPackageContent' {..} =
    Prelude.mconcat
      ["Content-Type" Data.=# contentType]

instance
  Data.ToPath
    ValidateSolFunctionPackageContent
  where
  toPath ValidateSolFunctionPackageContent' {..} =
    Prelude.mconcat
      [ "/sol/vnfpkgm/v1/vnf_packages/",
        Data.toBS vnfPkgId,
        "/package_content/validate"
      ]

instance
  Data.ToQuery
    ValidateSolFunctionPackageContent
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateSolFunctionPackageContentResponse' smart constructor.
data ValidateSolFunctionPackageContentResponse = ValidateSolFunctionPackageContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Function package ID.
    id :: Prelude.Text,
    -- | Function package metadata.
    metadata :: ValidateSolFunctionPackageContentMetadata,
    -- | Network function product name.
    vnfProductName :: Prelude.Text,
    -- | Network function provider.
    vnfProvider :: Prelude.Text,
    -- | Function package descriptor ID.
    vnfdId :: Prelude.Text,
    -- | Function package descriptor version.
    vnfdVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateSolFunctionPackageContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'validateSolFunctionPackageContentResponse_httpStatus' - The response's http status code.
--
-- 'id', 'validateSolFunctionPackageContentResponse_id' - Function package ID.
--
-- 'metadata', 'validateSolFunctionPackageContentResponse_metadata' - Function package metadata.
--
-- 'vnfProductName', 'validateSolFunctionPackageContentResponse_vnfProductName' - Network function product name.
--
-- 'vnfProvider', 'validateSolFunctionPackageContentResponse_vnfProvider' - Network function provider.
--
-- 'vnfdId', 'validateSolFunctionPackageContentResponse_vnfdId' - Function package descriptor ID.
--
-- 'vnfdVersion', 'validateSolFunctionPackageContentResponse_vnfdVersion' - Function package descriptor version.
newValidateSolFunctionPackageContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  ValidateSolFunctionPackageContentMetadata ->
  -- | 'vnfProductName'
  Prelude.Text ->
  -- | 'vnfProvider'
  Prelude.Text ->
  -- | 'vnfdId'
  Prelude.Text ->
  -- | 'vnfdVersion'
  Prelude.Text ->
  ValidateSolFunctionPackageContentResponse
newValidateSolFunctionPackageContentResponse
  pHttpStatus_
  pId_
  pMetadata_
  pVnfProductName_
  pVnfProvider_
  pVnfdId_
  pVnfdVersion_ =
    ValidateSolFunctionPackageContentResponse'
      { httpStatus =
          pHttpStatus_,
        id = pId_,
        metadata = pMetadata_,
        vnfProductName =
          pVnfProductName_,
        vnfProvider = pVnfProvider_,
        vnfdId = pVnfdId_,
        vnfdVersion = pVnfdVersion_
      }

-- | The response's http status code.
validateSolFunctionPackageContentResponse_httpStatus :: Lens.Lens' ValidateSolFunctionPackageContentResponse Prelude.Int
validateSolFunctionPackageContentResponse_httpStatus = Lens.lens (\ValidateSolFunctionPackageContentResponse' {httpStatus} -> httpStatus) (\s@ValidateSolFunctionPackageContentResponse' {} a -> s {httpStatus = a} :: ValidateSolFunctionPackageContentResponse)

-- | Function package ID.
validateSolFunctionPackageContentResponse_id :: Lens.Lens' ValidateSolFunctionPackageContentResponse Prelude.Text
validateSolFunctionPackageContentResponse_id = Lens.lens (\ValidateSolFunctionPackageContentResponse' {id} -> id) (\s@ValidateSolFunctionPackageContentResponse' {} a -> s {id = a} :: ValidateSolFunctionPackageContentResponse)

-- | Function package metadata.
validateSolFunctionPackageContentResponse_metadata :: Lens.Lens' ValidateSolFunctionPackageContentResponse ValidateSolFunctionPackageContentMetadata
validateSolFunctionPackageContentResponse_metadata = Lens.lens (\ValidateSolFunctionPackageContentResponse' {metadata} -> metadata) (\s@ValidateSolFunctionPackageContentResponse' {} a -> s {metadata = a} :: ValidateSolFunctionPackageContentResponse)

-- | Network function product name.
validateSolFunctionPackageContentResponse_vnfProductName :: Lens.Lens' ValidateSolFunctionPackageContentResponse Prelude.Text
validateSolFunctionPackageContentResponse_vnfProductName = Lens.lens (\ValidateSolFunctionPackageContentResponse' {vnfProductName} -> vnfProductName) (\s@ValidateSolFunctionPackageContentResponse' {} a -> s {vnfProductName = a} :: ValidateSolFunctionPackageContentResponse)

-- | Network function provider.
validateSolFunctionPackageContentResponse_vnfProvider :: Lens.Lens' ValidateSolFunctionPackageContentResponse Prelude.Text
validateSolFunctionPackageContentResponse_vnfProvider = Lens.lens (\ValidateSolFunctionPackageContentResponse' {vnfProvider} -> vnfProvider) (\s@ValidateSolFunctionPackageContentResponse' {} a -> s {vnfProvider = a} :: ValidateSolFunctionPackageContentResponse)

-- | Function package descriptor ID.
validateSolFunctionPackageContentResponse_vnfdId :: Lens.Lens' ValidateSolFunctionPackageContentResponse Prelude.Text
validateSolFunctionPackageContentResponse_vnfdId = Lens.lens (\ValidateSolFunctionPackageContentResponse' {vnfdId} -> vnfdId) (\s@ValidateSolFunctionPackageContentResponse' {} a -> s {vnfdId = a} :: ValidateSolFunctionPackageContentResponse)

-- | Function package descriptor version.
validateSolFunctionPackageContentResponse_vnfdVersion :: Lens.Lens' ValidateSolFunctionPackageContentResponse Prelude.Text
validateSolFunctionPackageContentResponse_vnfdVersion = Lens.lens (\ValidateSolFunctionPackageContentResponse' {vnfdVersion} -> vnfdVersion) (\s@ValidateSolFunctionPackageContentResponse' {} a -> s {vnfdVersion = a} :: ValidateSolFunctionPackageContentResponse)

instance
  Prelude.NFData
    ValidateSolFunctionPackageContentResponse
  where
  rnf ValidateSolFunctionPackageContentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf vnfProductName
      `Prelude.seq` Prelude.rnf vnfProvider
      `Prelude.seq` Prelude.rnf vnfdId
      `Prelude.seq` Prelude.rnf vnfdVersion
