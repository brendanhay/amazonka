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
-- Module      : Amazonka.TNB.ValidateSolNetworkPackageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates network package content. This can be used as a dry run before
-- uploading network package content with
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_PutSolNetworkPackageContent.html PutSolNetworkPackageContent>.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
module Amazonka.TNB.ValidateSolNetworkPackageContent
  ( -- * Creating a Request
    ValidateSolNetworkPackageContent (..),
    newValidateSolNetworkPackageContent,

    -- * Request Lenses
    validateSolNetworkPackageContent_contentType,
    validateSolNetworkPackageContent_file,
    validateSolNetworkPackageContent_nsdInfoId,

    -- * Destructuring the Response
    ValidateSolNetworkPackageContentResponse (..),
    newValidateSolNetworkPackageContentResponse,

    -- * Response Lenses
    validateSolNetworkPackageContentResponse_httpStatus,
    validateSolNetworkPackageContentResponse_arn,
    validateSolNetworkPackageContentResponse_id,
    validateSolNetworkPackageContentResponse_metadata,
    validateSolNetworkPackageContentResponse_nsdId,
    validateSolNetworkPackageContentResponse_nsdName,
    validateSolNetworkPackageContentResponse_nsdVersion,
    validateSolNetworkPackageContentResponse_vnfPkgIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newValidateSolNetworkPackageContent' smart constructor.
data ValidateSolNetworkPackageContent = ValidateSolNetworkPackageContent'
  { -- | Network package content type.
    contentType :: Prelude.Maybe PackageContentType,
    -- | Network package file.
    file :: Prelude.ByteString,
    -- | Network service descriptor file.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateSolNetworkPackageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'validateSolNetworkPackageContent_contentType' - Network package content type.
--
-- 'file', 'validateSolNetworkPackageContent_file' - Network package file.
--
-- 'nsdInfoId', 'validateSolNetworkPackageContent_nsdInfoId' - Network service descriptor file.
newValidateSolNetworkPackageContent ::
  -- | 'file'
  Prelude.ByteString ->
  -- | 'nsdInfoId'
  Prelude.Text ->
  ValidateSolNetworkPackageContent
newValidateSolNetworkPackageContent
  pFile_
  pNsdInfoId_ =
    ValidateSolNetworkPackageContent'
      { contentType =
          Prelude.Nothing,
        file = pFile_,
        nsdInfoId = pNsdInfoId_
      }

-- | Network package content type.
validateSolNetworkPackageContent_contentType :: Lens.Lens' ValidateSolNetworkPackageContent (Prelude.Maybe PackageContentType)
validateSolNetworkPackageContent_contentType = Lens.lens (\ValidateSolNetworkPackageContent' {contentType} -> contentType) (\s@ValidateSolNetworkPackageContent' {} a -> s {contentType = a} :: ValidateSolNetworkPackageContent)

-- | Network package file.
validateSolNetworkPackageContent_file :: Lens.Lens' ValidateSolNetworkPackageContent Prelude.ByteString
validateSolNetworkPackageContent_file = Lens.lens (\ValidateSolNetworkPackageContent' {file} -> file) (\s@ValidateSolNetworkPackageContent' {} a -> s {file = a} :: ValidateSolNetworkPackageContent)

-- | Network service descriptor file.
validateSolNetworkPackageContent_nsdInfoId :: Lens.Lens' ValidateSolNetworkPackageContent Prelude.Text
validateSolNetworkPackageContent_nsdInfoId = Lens.lens (\ValidateSolNetworkPackageContent' {nsdInfoId} -> nsdInfoId) (\s@ValidateSolNetworkPackageContent' {} a -> s {nsdInfoId = a} :: ValidateSolNetworkPackageContent)

instance
  Core.AWSRequest
    ValidateSolNetworkPackageContent
  where
  type
    AWSResponse ValidateSolNetworkPackageContent =
      ValidateSolNetworkPackageContentResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateSolNetworkPackageContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "metadata")
            Prelude.<*> (x Data..:> "nsdId")
            Prelude.<*> (x Data..:> "nsdName")
            Prelude.<*> (x Data..:> "nsdVersion")
            Prelude.<*> (x Data..?> "vnfPkgIds" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    ValidateSolNetworkPackageContent
  where
  hashWithSalt
    _salt
    ValidateSolNetworkPackageContent' {..} =
      _salt
        `Prelude.hashWithSalt` contentType
        `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` nsdInfoId

instance
  Prelude.NFData
    ValidateSolNetworkPackageContent
  where
  rnf ValidateSolNetworkPackageContent' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf nsdInfoId

instance Data.ToBody ValidateSolNetworkPackageContent where
  toBody ValidateSolNetworkPackageContent' {..} =
    Data.toBody file

instance
  Data.ToHeaders
    ValidateSolNetworkPackageContent
  where
  toHeaders ValidateSolNetworkPackageContent' {..} =
    Prelude.mconcat
      ["Content-Type" Data.=# contentType]

instance Data.ToPath ValidateSolNetworkPackageContent where
  toPath ValidateSolNetworkPackageContent' {..} =
    Prelude.mconcat
      [ "/sol/nsd/v1/ns_descriptors/",
        Data.toBS nsdInfoId,
        "/nsd_content/validate"
      ]

instance
  Data.ToQuery
    ValidateSolNetworkPackageContent
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateSolNetworkPackageContentResponse' smart constructor.
data ValidateSolNetworkPackageContentResponse = ValidateSolNetworkPackageContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network package ARN.
    arn :: Prelude.Text,
    -- | Network package ID.
    id :: Prelude.Text,
    -- | Network package metadata.
    metadata :: ValidateSolNetworkPackageContentMetadata,
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
-- Create a value of 'ValidateSolNetworkPackageContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'validateSolNetworkPackageContentResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'validateSolNetworkPackageContentResponse_arn' - Network package ARN.
--
-- 'id', 'validateSolNetworkPackageContentResponse_id' - Network package ID.
--
-- 'metadata', 'validateSolNetworkPackageContentResponse_metadata' - Network package metadata.
--
-- 'nsdId', 'validateSolNetworkPackageContentResponse_nsdId' - Network service descriptor ID.
--
-- 'nsdName', 'validateSolNetworkPackageContentResponse_nsdName' - Network service descriptor name.
--
-- 'nsdVersion', 'validateSolNetworkPackageContentResponse_nsdVersion' - Network service descriptor version.
--
-- 'vnfPkgIds', 'validateSolNetworkPackageContentResponse_vnfPkgIds' - Function package IDs.
newValidateSolNetworkPackageContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  ValidateSolNetworkPackageContentMetadata ->
  -- | 'nsdId'
  Prelude.Text ->
  -- | 'nsdName'
  Prelude.Text ->
  -- | 'nsdVersion'
  Prelude.Text ->
  ValidateSolNetworkPackageContentResponse
newValidateSolNetworkPackageContentResponse
  pHttpStatus_
  pArn_
  pId_
  pMetadata_
  pNsdId_
  pNsdName_
  pNsdVersion_ =
    ValidateSolNetworkPackageContentResponse'
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
validateSolNetworkPackageContentResponse_httpStatus :: Lens.Lens' ValidateSolNetworkPackageContentResponse Prelude.Int
validateSolNetworkPackageContentResponse_httpStatus = Lens.lens (\ValidateSolNetworkPackageContentResponse' {httpStatus} -> httpStatus) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {httpStatus = a} :: ValidateSolNetworkPackageContentResponse)

-- | Network package ARN.
validateSolNetworkPackageContentResponse_arn :: Lens.Lens' ValidateSolNetworkPackageContentResponse Prelude.Text
validateSolNetworkPackageContentResponse_arn = Lens.lens (\ValidateSolNetworkPackageContentResponse' {arn} -> arn) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {arn = a} :: ValidateSolNetworkPackageContentResponse)

-- | Network package ID.
validateSolNetworkPackageContentResponse_id :: Lens.Lens' ValidateSolNetworkPackageContentResponse Prelude.Text
validateSolNetworkPackageContentResponse_id = Lens.lens (\ValidateSolNetworkPackageContentResponse' {id} -> id) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {id = a} :: ValidateSolNetworkPackageContentResponse)

-- | Network package metadata.
validateSolNetworkPackageContentResponse_metadata :: Lens.Lens' ValidateSolNetworkPackageContentResponse ValidateSolNetworkPackageContentMetadata
validateSolNetworkPackageContentResponse_metadata = Lens.lens (\ValidateSolNetworkPackageContentResponse' {metadata} -> metadata) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {metadata = a} :: ValidateSolNetworkPackageContentResponse)

-- | Network service descriptor ID.
validateSolNetworkPackageContentResponse_nsdId :: Lens.Lens' ValidateSolNetworkPackageContentResponse Prelude.Text
validateSolNetworkPackageContentResponse_nsdId = Lens.lens (\ValidateSolNetworkPackageContentResponse' {nsdId} -> nsdId) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {nsdId = a} :: ValidateSolNetworkPackageContentResponse)

-- | Network service descriptor name.
validateSolNetworkPackageContentResponse_nsdName :: Lens.Lens' ValidateSolNetworkPackageContentResponse Prelude.Text
validateSolNetworkPackageContentResponse_nsdName = Lens.lens (\ValidateSolNetworkPackageContentResponse' {nsdName} -> nsdName) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {nsdName = a} :: ValidateSolNetworkPackageContentResponse)

-- | Network service descriptor version.
validateSolNetworkPackageContentResponse_nsdVersion :: Lens.Lens' ValidateSolNetworkPackageContentResponse Prelude.Text
validateSolNetworkPackageContentResponse_nsdVersion = Lens.lens (\ValidateSolNetworkPackageContentResponse' {nsdVersion} -> nsdVersion) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {nsdVersion = a} :: ValidateSolNetworkPackageContentResponse)

-- | Function package IDs.
validateSolNetworkPackageContentResponse_vnfPkgIds :: Lens.Lens' ValidateSolNetworkPackageContentResponse [Prelude.Text]
validateSolNetworkPackageContentResponse_vnfPkgIds = Lens.lens (\ValidateSolNetworkPackageContentResponse' {vnfPkgIds} -> vnfPkgIds) (\s@ValidateSolNetworkPackageContentResponse' {} a -> s {vnfPkgIds = a} :: ValidateSolNetworkPackageContentResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ValidateSolNetworkPackageContentResponse
  where
  rnf ValidateSolNetworkPackageContentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsdId
      `Prelude.seq` Prelude.rnf nsdName
      `Prelude.seq` Prelude.rnf nsdVersion
      `Prelude.seq` Prelude.rnf vnfPkgIds
