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
-- Module      : Amazonka.FSx.CreateFileCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon File Cache resource.
--
-- You can use this operation with a client request token in the request
-- that Amazon File Cache uses to ensure idempotent creation. If a cache
-- with the specified client request token exists and the parameters match,
-- @CreateFileCache@ returns the description of the existing cache. If a
-- cache with the specified client request token exists and the parameters
-- don\'t match, this call returns @IncompatibleParameterError@. If a file
-- cache with the specified client request token doesn\'t exist,
-- @CreateFileCache@ does the following:
--
-- -   Creates a new, empty Amazon File Cache resourcewith an assigned ID,
--     and an initial lifecycle state of @CREATING@.
--
-- -   Returns the description of the cache in JSON format.
--
-- The @CreateFileCache@ call returns while the cache\'s lifecycle state is
-- still @CREATING@. You can check the cache creation status by calling the
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DescribeFileCaches.html DescribeFileCaches>
-- operation, which returns the cache state along with other information.
module Amazonka.FSx.CreateFileCache
  ( -- * Creating a Request
    CreateFileCache (..),
    newCreateFileCache,

    -- * Request Lenses
    createFileCache_tags,
    createFileCache_clientRequestToken,
    createFileCache_securityGroupIds,
    createFileCache_copyTagsToDataRepositoryAssociations,
    createFileCache_kmsKeyId,
    createFileCache_dataRepositoryAssociations,
    createFileCache_lustreConfiguration,
    createFileCache_fileCacheType,
    createFileCache_fileCacheTypeVersion,
    createFileCache_storageCapacity,
    createFileCache_subnetIds,

    -- * Destructuring the Response
    CreateFileCacheResponse (..),
    newCreateFileCacheResponse,

    -- * Response Lenses
    createFileCacheResponse_fileCache,
    createFileCacheResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFileCache' smart constructor.
data CreateFileCache = CreateFileCache'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | An idempotency token for resource creation, in a string of up to 64
    -- ASCII characters. This token is automatically filled on your behalf when
    -- you use the Command Line Interface (CLI) or an Amazon Web Services SDK.
    --
    -- By using the idempotent operation, you can retry a @CreateFileCache@
    -- operation without the risk of creating an extra cache. This approach can
    -- be useful when an initial call fails in a way that makes it unclear
    -- whether a cache was created. Examples are if a transport level timeout
    -- occurred, or your connection was reset. If you use the same client
    -- request token and the initial call created a cache, the client receives
    -- success as long as the parameters are the same.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A list of IDs specifying the security groups to apply to all network
    -- interfaces created for Amazon File Cache access. This list isn\'t
    -- returned in later requests to describe the cache.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A boolean flag indicating whether tags for the cache should be copied to
    -- data repository associations. This value defaults to false.
    copyTagsToDataRepositoryAssociations :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the ID of the Key Management Service (KMS) key to use for
    -- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
    -- specified, the Amazon FSx-managed KMS key for your account is used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
    -- in the /Key Management Service API Reference/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 8 configurations for data repository associations (DRAs)
    -- to be created during the cache creation. The DRAs link the cache to
    -- either an Amazon S3 data repository or a Network File System (NFS) data
    -- repository that supports the NFSv3 protocol.
    --
    -- The DRA configurations must meet the following requirements:
    --
    -- -   All configurations on the list must be of the same data repository
    --     type, either all S3 or all NFS. A cache can\'t link to different
    --     data repository types at the same time.
    --
    -- -   An NFS DRA must link to an NFS file system that supports the NFSv3
    --     protocol.
    --
    -- DRA automatic import and automatic export is not supported.
    dataRepositoryAssociations :: Prelude.Maybe [FileCacheDataRepositoryAssociation],
    -- | The configuration for the Amazon File Cache resource being created.
    lustreConfiguration :: Prelude.Maybe CreateFileCacheLustreConfiguration,
    -- | The type of cache that you\'re creating, which must be @LUSTRE@.
    fileCacheType :: FileCacheType,
    -- | Sets the Lustre version for the cache that you\'re creating, which must
    -- be @2.12@.
    fileCacheTypeVersion :: Prelude.Text,
    -- | The storage capacity of the cache in gibibytes (GiB). Valid values are
    -- 1200 GiB, 2400 GiB, and increments of 2400 GiB.
    storageCapacity :: Prelude.Natural,
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFileCache_tags' - Undocumented member.
--
-- 'clientRequestToken', 'createFileCache_clientRequestToken' - An idempotency token for resource creation, in a string of up to 64
-- ASCII characters. This token is automatically filled on your behalf when
-- you use the Command Line Interface (CLI) or an Amazon Web Services SDK.
--
-- By using the idempotent operation, you can retry a @CreateFileCache@
-- operation without the risk of creating an extra cache. This approach can
-- be useful when an initial call fails in a way that makes it unclear
-- whether a cache was created. Examples are if a transport level timeout
-- occurred, or your connection was reset. If you use the same client
-- request token and the initial call created a cache, the client receives
-- success as long as the parameters are the same.
--
-- 'securityGroupIds', 'createFileCache_securityGroupIds' - A list of IDs specifying the security groups to apply to all network
-- interfaces created for Amazon File Cache access. This list isn\'t
-- returned in later requests to describe the cache.
--
-- 'copyTagsToDataRepositoryAssociations', 'createFileCache_copyTagsToDataRepositoryAssociations' - A boolean flag indicating whether tags for the cache should be copied to
-- data repository associations. This value defaults to false.
--
-- 'kmsKeyId', 'createFileCache_kmsKeyId' - Specifies the ID of the Key Management Service (KMS) key to use for
-- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
-- specified, the Amazon FSx-managed KMS key for your account is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
-- in the /Key Management Service API Reference/.
--
-- 'dataRepositoryAssociations', 'createFileCache_dataRepositoryAssociations' - A list of up to 8 configurations for data repository associations (DRAs)
-- to be created during the cache creation. The DRAs link the cache to
-- either an Amazon S3 data repository or a Network File System (NFS) data
-- repository that supports the NFSv3 protocol.
--
-- The DRA configurations must meet the following requirements:
--
-- -   All configurations on the list must be of the same data repository
--     type, either all S3 or all NFS. A cache can\'t link to different
--     data repository types at the same time.
--
-- -   An NFS DRA must link to an NFS file system that supports the NFSv3
--     protocol.
--
-- DRA automatic import and automatic export is not supported.
--
-- 'lustreConfiguration', 'createFileCache_lustreConfiguration' - The configuration for the Amazon File Cache resource being created.
--
-- 'fileCacheType', 'createFileCache_fileCacheType' - The type of cache that you\'re creating, which must be @LUSTRE@.
--
-- 'fileCacheTypeVersion', 'createFileCache_fileCacheTypeVersion' - Sets the Lustre version for the cache that you\'re creating, which must
-- be @2.12@.
--
-- 'storageCapacity', 'createFileCache_storageCapacity' - The storage capacity of the cache in gibibytes (GiB). Valid values are
-- 1200 GiB, 2400 GiB, and increments of 2400 GiB.
--
-- 'subnetIds', 'createFileCache_subnetIds' - Undocumented member.
newCreateFileCache ::
  -- | 'fileCacheType'
  FileCacheType ->
  -- | 'fileCacheTypeVersion'
  Prelude.Text ->
  -- | 'storageCapacity'
  Prelude.Natural ->
  CreateFileCache
newCreateFileCache
  pFileCacheType_
  pFileCacheTypeVersion_
  pStorageCapacity_ =
    CreateFileCache'
      { tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        copyTagsToDataRepositoryAssociations =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        dataRepositoryAssociations = Prelude.Nothing,
        lustreConfiguration = Prelude.Nothing,
        fileCacheType = pFileCacheType_,
        fileCacheTypeVersion = pFileCacheTypeVersion_,
        storageCapacity = pStorageCapacity_,
        subnetIds = Prelude.mempty
      }

-- | Undocumented member.
createFileCache_tags :: Lens.Lens' CreateFileCache (Prelude.Maybe (Prelude.NonEmpty Tag))
createFileCache_tags = Lens.lens (\CreateFileCache' {tags} -> tags) (\s@CreateFileCache' {} a -> s {tags = a} :: CreateFileCache) Prelude.. Lens.mapping Lens.coerced

-- | An idempotency token for resource creation, in a string of up to 64
-- ASCII characters. This token is automatically filled on your behalf when
-- you use the Command Line Interface (CLI) or an Amazon Web Services SDK.
--
-- By using the idempotent operation, you can retry a @CreateFileCache@
-- operation without the risk of creating an extra cache. This approach can
-- be useful when an initial call fails in a way that makes it unclear
-- whether a cache was created. Examples are if a transport level timeout
-- occurred, or your connection was reset. If you use the same client
-- request token and the initial call created a cache, the client receives
-- success as long as the parameters are the same.
createFileCache_clientRequestToken :: Lens.Lens' CreateFileCache (Prelude.Maybe Prelude.Text)
createFileCache_clientRequestToken = Lens.lens (\CreateFileCache' {clientRequestToken} -> clientRequestToken) (\s@CreateFileCache' {} a -> s {clientRequestToken = a} :: CreateFileCache)

-- | A list of IDs specifying the security groups to apply to all network
-- interfaces created for Amazon File Cache access. This list isn\'t
-- returned in later requests to describe the cache.
createFileCache_securityGroupIds :: Lens.Lens' CreateFileCache (Prelude.Maybe [Prelude.Text])
createFileCache_securityGroupIds = Lens.lens (\CreateFileCache' {securityGroupIds} -> securityGroupIds) (\s@CreateFileCache' {} a -> s {securityGroupIds = a} :: CreateFileCache) Prelude.. Lens.mapping Lens.coerced

-- | A boolean flag indicating whether tags for the cache should be copied to
-- data repository associations. This value defaults to false.
createFileCache_copyTagsToDataRepositoryAssociations :: Lens.Lens' CreateFileCache (Prelude.Maybe Prelude.Bool)
createFileCache_copyTagsToDataRepositoryAssociations = Lens.lens (\CreateFileCache' {copyTagsToDataRepositoryAssociations} -> copyTagsToDataRepositoryAssociations) (\s@CreateFileCache' {} a -> s {copyTagsToDataRepositoryAssociations = a} :: CreateFileCache)

-- | Specifies the ID of the Key Management Service (KMS) key to use for
-- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
-- specified, the Amazon FSx-managed KMS key for your account is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
-- in the /Key Management Service API Reference/.
createFileCache_kmsKeyId :: Lens.Lens' CreateFileCache (Prelude.Maybe Prelude.Text)
createFileCache_kmsKeyId = Lens.lens (\CreateFileCache' {kmsKeyId} -> kmsKeyId) (\s@CreateFileCache' {} a -> s {kmsKeyId = a} :: CreateFileCache)

-- | A list of up to 8 configurations for data repository associations (DRAs)
-- to be created during the cache creation. The DRAs link the cache to
-- either an Amazon S3 data repository or a Network File System (NFS) data
-- repository that supports the NFSv3 protocol.
--
-- The DRA configurations must meet the following requirements:
--
-- -   All configurations on the list must be of the same data repository
--     type, either all S3 or all NFS. A cache can\'t link to different
--     data repository types at the same time.
--
-- -   An NFS DRA must link to an NFS file system that supports the NFSv3
--     protocol.
--
-- DRA automatic import and automatic export is not supported.
createFileCache_dataRepositoryAssociations :: Lens.Lens' CreateFileCache (Prelude.Maybe [FileCacheDataRepositoryAssociation])
createFileCache_dataRepositoryAssociations = Lens.lens (\CreateFileCache' {dataRepositoryAssociations} -> dataRepositoryAssociations) (\s@CreateFileCache' {} a -> s {dataRepositoryAssociations = a} :: CreateFileCache) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for the Amazon File Cache resource being created.
createFileCache_lustreConfiguration :: Lens.Lens' CreateFileCache (Prelude.Maybe CreateFileCacheLustreConfiguration)
createFileCache_lustreConfiguration = Lens.lens (\CreateFileCache' {lustreConfiguration} -> lustreConfiguration) (\s@CreateFileCache' {} a -> s {lustreConfiguration = a} :: CreateFileCache)

-- | The type of cache that you\'re creating, which must be @LUSTRE@.
createFileCache_fileCacheType :: Lens.Lens' CreateFileCache FileCacheType
createFileCache_fileCacheType = Lens.lens (\CreateFileCache' {fileCacheType} -> fileCacheType) (\s@CreateFileCache' {} a -> s {fileCacheType = a} :: CreateFileCache)

-- | Sets the Lustre version for the cache that you\'re creating, which must
-- be @2.12@.
createFileCache_fileCacheTypeVersion :: Lens.Lens' CreateFileCache Prelude.Text
createFileCache_fileCacheTypeVersion = Lens.lens (\CreateFileCache' {fileCacheTypeVersion} -> fileCacheTypeVersion) (\s@CreateFileCache' {} a -> s {fileCacheTypeVersion = a} :: CreateFileCache)

-- | The storage capacity of the cache in gibibytes (GiB). Valid values are
-- 1200 GiB, 2400 GiB, and increments of 2400 GiB.
createFileCache_storageCapacity :: Lens.Lens' CreateFileCache Prelude.Natural
createFileCache_storageCapacity = Lens.lens (\CreateFileCache' {storageCapacity} -> storageCapacity) (\s@CreateFileCache' {} a -> s {storageCapacity = a} :: CreateFileCache)

-- | Undocumented member.
createFileCache_subnetIds :: Lens.Lens' CreateFileCache [Prelude.Text]
createFileCache_subnetIds = Lens.lens (\CreateFileCache' {subnetIds} -> subnetIds) (\s@CreateFileCache' {} a -> s {subnetIds = a} :: CreateFileCache) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFileCache where
  type
    AWSResponse CreateFileCache =
      CreateFileCacheResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFileCacheResponse'
            Prelude.<$> (x Data..?> "FileCache")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFileCache where
  hashWithSalt _salt CreateFileCache' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` copyTagsToDataRepositoryAssociations
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` dataRepositoryAssociations
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` fileCacheType
      `Prelude.hashWithSalt` fileCacheTypeVersion
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData CreateFileCache where
  rnf CreateFileCache' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf copyTagsToDataRepositoryAssociations
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf dataRepositoryAssociations
      `Prelude.seq` Prelude.rnf lustreConfiguration
      `Prelude.seq` Prelude.rnf fileCacheType
      `Prelude.seq` Prelude.rnf fileCacheTypeVersion
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders CreateFileCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateFileCache" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFileCache where
  toJSON CreateFileCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("CopyTagsToDataRepositoryAssociations" Data..=)
              Prelude.<$> copyTagsToDataRepositoryAssociations,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("DataRepositoryAssociations" Data..=)
              Prelude.<$> dataRepositoryAssociations,
            ("LustreConfiguration" Data..=)
              Prelude.<$> lustreConfiguration,
            Prelude.Just ("FileCacheType" Data..= fileCacheType),
            Prelude.Just
              ( "FileCacheTypeVersion"
                  Data..= fileCacheTypeVersion
              ),
            Prelude.Just
              ("StorageCapacity" Data..= storageCapacity),
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )

instance Data.ToPath CreateFileCache where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFileCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFileCacheResponse' smart constructor.
data CreateFileCacheResponse = CreateFileCacheResponse'
  { -- | A description of the cache that was created.
    fileCache :: Prelude.Maybe FileCacheCreating,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileCache', 'createFileCacheResponse_fileCache' - A description of the cache that was created.
--
-- 'httpStatus', 'createFileCacheResponse_httpStatus' - The response's http status code.
newCreateFileCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFileCacheResponse
newCreateFileCacheResponse pHttpStatus_ =
  CreateFileCacheResponse'
    { fileCache =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the cache that was created.
createFileCacheResponse_fileCache :: Lens.Lens' CreateFileCacheResponse (Prelude.Maybe FileCacheCreating)
createFileCacheResponse_fileCache = Lens.lens (\CreateFileCacheResponse' {fileCache} -> fileCache) (\s@CreateFileCacheResponse' {} a -> s {fileCache = a} :: CreateFileCacheResponse)

-- | The response's http status code.
createFileCacheResponse_httpStatus :: Lens.Lens' CreateFileCacheResponse Prelude.Int
createFileCacheResponse_httpStatus = Lens.lens (\CreateFileCacheResponse' {httpStatus} -> httpStatus) (\s@CreateFileCacheResponse' {} a -> s {httpStatus = a} :: CreateFileCacheResponse)

instance Prelude.NFData CreateFileCacheResponse where
  rnf CreateFileCacheResponse' {..} =
    Prelude.rnf fileCache
      `Prelude.seq` Prelude.rnf httpStatus
