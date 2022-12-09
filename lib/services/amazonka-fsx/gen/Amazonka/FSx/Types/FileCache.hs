{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FSx.Types.FileCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileCache where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.FileCacheFailureDetails
import Amazonka.FSx.Types.FileCacheLifecycle
import Amazonka.FSx.Types.FileCacheLustreConfiguration
import Amazonka.FSx.Types.FileCacheType
import qualified Amazonka.Prelude as Prelude

-- | A description of a specific Amazon File Cache resource, which is a
-- response object from the @DescribeFileCaches@ operation.
--
-- /See:/ 'newFileCache' smart constructor.
data FileCache = FileCache'
  { creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Domain Name System (DNS) name for the cache.
    dNSName :: Prelude.Maybe Prelude.Text,
    -- | A list of IDs of data repository associations that are associated with
    -- this cache.
    dataRepositoryAssociationIds :: Prelude.Maybe [Prelude.Text],
    -- | A structure providing details of any failures that occurred.
    failureDetails :: Prelude.Maybe FileCacheFailureDetails,
    -- | The system-generated, unique ID of the cache.
    fileCacheId :: Prelude.Maybe Prelude.Text,
    -- | The type of cache, which must be @LUSTRE@.
    fileCacheType :: Prelude.Maybe FileCacheType,
    -- | The Lustre version of the cache, which must be @2.12@.
    fileCacheTypeVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of the Key Management Service (KMS) key to use for
    -- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
    -- specified, the Amazon FSx-managed KMS key for your account is used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
    -- in the /Key Management Service API Reference/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle status of the cache. The following are the possible values
    -- and what they mean:
    --
    -- -   @AVAILABLE@ - The cache is in a healthy state, and is reachable and
    --     available for use.
    --
    -- -   @CREATING@ - The new cache is being created.
    --
    -- -   @DELETING@ - An existing cache is being deleted.
    --
    -- -   @UPDATING@ - The cache is undergoing a customer-initiated update.
    --
    -- -   @FAILED@ - An existing cache has experienced an unrecoverable
    --     failure. When creating a new cache, the cache was unable to be
    --     created.
    lifecycle :: Prelude.Maybe FileCacheLifecycle,
    -- | The configuration for the Amazon File Cache resource.
    lustreConfiguration :: Prelude.Maybe FileCacheLustreConfiguration,
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    ownerId :: Prelude.Maybe Prelude.Text,
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The storage capacity of the cache in gibibytes (GiB).
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    subnetIds :: Prelude.Maybe [Prelude.Text],
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'fileCache_creationTime' - Undocumented member.
--
-- 'dNSName', 'fileCache_dNSName' - The Domain Name System (DNS) name for the cache.
--
-- 'dataRepositoryAssociationIds', 'fileCache_dataRepositoryAssociationIds' - A list of IDs of data repository associations that are associated with
-- this cache.
--
-- 'failureDetails', 'fileCache_failureDetails' - A structure providing details of any failures that occurred.
--
-- 'fileCacheId', 'fileCache_fileCacheId' - The system-generated, unique ID of the cache.
--
-- 'fileCacheType', 'fileCache_fileCacheType' - The type of cache, which must be @LUSTRE@.
--
-- 'fileCacheTypeVersion', 'fileCache_fileCacheTypeVersion' - The Lustre version of the cache, which must be @2.12@.
--
-- 'kmsKeyId', 'fileCache_kmsKeyId' - Specifies the ID of the Key Management Service (KMS) key to use for
-- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
-- specified, the Amazon FSx-managed KMS key for your account is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
-- in the /Key Management Service API Reference/.
--
-- 'lifecycle', 'fileCache_lifecycle' - The lifecycle status of the cache. The following are the possible values
-- and what they mean:
--
-- -   @AVAILABLE@ - The cache is in a healthy state, and is reachable and
--     available for use.
--
-- -   @CREATING@ - The new cache is being created.
--
-- -   @DELETING@ - An existing cache is being deleted.
--
-- -   @UPDATING@ - The cache is undergoing a customer-initiated update.
--
-- -   @FAILED@ - An existing cache has experienced an unrecoverable
--     failure. When creating a new cache, the cache was unable to be
--     created.
--
-- 'lustreConfiguration', 'fileCache_lustreConfiguration' - The configuration for the Amazon File Cache resource.
--
-- 'networkInterfaceIds', 'fileCache_networkInterfaceIds' - Undocumented member.
--
-- 'ownerId', 'fileCache_ownerId' - Undocumented member.
--
-- 'resourceARN', 'fileCache_resourceARN' - Undocumented member.
--
-- 'storageCapacity', 'fileCache_storageCapacity' - The storage capacity of the cache in gibibytes (GiB).
--
-- 'subnetIds', 'fileCache_subnetIds' - Undocumented member.
--
-- 'vpcId', 'fileCache_vpcId' - Undocumented member.
newFileCache ::
  FileCache
newFileCache =
  FileCache'
    { creationTime = Prelude.Nothing,
      dNSName = Prelude.Nothing,
      dataRepositoryAssociationIds = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      fileCacheId = Prelude.Nothing,
      fileCacheType = Prelude.Nothing,
      fileCacheTypeVersion = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      lustreConfiguration = Prelude.Nothing,
      networkInterfaceIds = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Undocumented member.
fileCache_creationTime :: Lens.Lens' FileCache (Prelude.Maybe Prelude.UTCTime)
fileCache_creationTime = Lens.lens (\FileCache' {creationTime} -> creationTime) (\s@FileCache' {} a -> s {creationTime = a} :: FileCache) Prelude.. Lens.mapping Data._Time

-- | The Domain Name System (DNS) name for the cache.
fileCache_dNSName :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Text)
fileCache_dNSName = Lens.lens (\FileCache' {dNSName} -> dNSName) (\s@FileCache' {} a -> s {dNSName = a} :: FileCache)

-- | A list of IDs of data repository associations that are associated with
-- this cache.
fileCache_dataRepositoryAssociationIds :: Lens.Lens' FileCache (Prelude.Maybe [Prelude.Text])
fileCache_dataRepositoryAssociationIds = Lens.lens (\FileCache' {dataRepositoryAssociationIds} -> dataRepositoryAssociationIds) (\s@FileCache' {} a -> s {dataRepositoryAssociationIds = a} :: FileCache) Prelude.. Lens.mapping Lens.coerced

-- | A structure providing details of any failures that occurred.
fileCache_failureDetails :: Lens.Lens' FileCache (Prelude.Maybe FileCacheFailureDetails)
fileCache_failureDetails = Lens.lens (\FileCache' {failureDetails} -> failureDetails) (\s@FileCache' {} a -> s {failureDetails = a} :: FileCache)

-- | The system-generated, unique ID of the cache.
fileCache_fileCacheId :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Text)
fileCache_fileCacheId = Lens.lens (\FileCache' {fileCacheId} -> fileCacheId) (\s@FileCache' {} a -> s {fileCacheId = a} :: FileCache)

-- | The type of cache, which must be @LUSTRE@.
fileCache_fileCacheType :: Lens.Lens' FileCache (Prelude.Maybe FileCacheType)
fileCache_fileCacheType = Lens.lens (\FileCache' {fileCacheType} -> fileCacheType) (\s@FileCache' {} a -> s {fileCacheType = a} :: FileCache)

-- | The Lustre version of the cache, which must be @2.12@.
fileCache_fileCacheTypeVersion :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Text)
fileCache_fileCacheTypeVersion = Lens.lens (\FileCache' {fileCacheTypeVersion} -> fileCacheTypeVersion) (\s@FileCache' {} a -> s {fileCacheTypeVersion = a} :: FileCache)

-- | Specifies the ID of the Key Management Service (KMS) key to use for
-- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
-- specified, the Amazon FSx-managed KMS key for your account is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
-- in the /Key Management Service API Reference/.
fileCache_kmsKeyId :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Text)
fileCache_kmsKeyId = Lens.lens (\FileCache' {kmsKeyId} -> kmsKeyId) (\s@FileCache' {} a -> s {kmsKeyId = a} :: FileCache)

-- | The lifecycle status of the cache. The following are the possible values
-- and what they mean:
--
-- -   @AVAILABLE@ - The cache is in a healthy state, and is reachable and
--     available for use.
--
-- -   @CREATING@ - The new cache is being created.
--
-- -   @DELETING@ - An existing cache is being deleted.
--
-- -   @UPDATING@ - The cache is undergoing a customer-initiated update.
--
-- -   @FAILED@ - An existing cache has experienced an unrecoverable
--     failure. When creating a new cache, the cache was unable to be
--     created.
fileCache_lifecycle :: Lens.Lens' FileCache (Prelude.Maybe FileCacheLifecycle)
fileCache_lifecycle = Lens.lens (\FileCache' {lifecycle} -> lifecycle) (\s@FileCache' {} a -> s {lifecycle = a} :: FileCache)

-- | The configuration for the Amazon File Cache resource.
fileCache_lustreConfiguration :: Lens.Lens' FileCache (Prelude.Maybe FileCacheLustreConfiguration)
fileCache_lustreConfiguration = Lens.lens (\FileCache' {lustreConfiguration} -> lustreConfiguration) (\s@FileCache' {} a -> s {lustreConfiguration = a} :: FileCache)

-- | Undocumented member.
fileCache_networkInterfaceIds :: Lens.Lens' FileCache (Prelude.Maybe [Prelude.Text])
fileCache_networkInterfaceIds = Lens.lens (\FileCache' {networkInterfaceIds} -> networkInterfaceIds) (\s@FileCache' {} a -> s {networkInterfaceIds = a} :: FileCache) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
fileCache_ownerId :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Text)
fileCache_ownerId = Lens.lens (\FileCache' {ownerId} -> ownerId) (\s@FileCache' {} a -> s {ownerId = a} :: FileCache)

-- | Undocumented member.
fileCache_resourceARN :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Text)
fileCache_resourceARN = Lens.lens (\FileCache' {resourceARN} -> resourceARN) (\s@FileCache' {} a -> s {resourceARN = a} :: FileCache)

-- | The storage capacity of the cache in gibibytes (GiB).
fileCache_storageCapacity :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Natural)
fileCache_storageCapacity = Lens.lens (\FileCache' {storageCapacity} -> storageCapacity) (\s@FileCache' {} a -> s {storageCapacity = a} :: FileCache)

-- | Undocumented member.
fileCache_subnetIds :: Lens.Lens' FileCache (Prelude.Maybe [Prelude.Text])
fileCache_subnetIds = Lens.lens (\FileCache' {subnetIds} -> subnetIds) (\s@FileCache' {} a -> s {subnetIds = a} :: FileCache) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
fileCache_vpcId :: Lens.Lens' FileCache (Prelude.Maybe Prelude.Text)
fileCache_vpcId = Lens.lens (\FileCache' {vpcId} -> vpcId) (\s@FileCache' {} a -> s {vpcId = a} :: FileCache)

instance Data.FromJSON FileCache where
  parseJSON =
    Data.withObject
      "FileCache"
      ( \x ->
          FileCache'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DNSName")
            Prelude.<*> ( x Data..:? "DataRepositoryAssociationIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "FileCacheId")
            Prelude.<*> (x Data..:? "FileCacheType")
            Prelude.<*> (x Data..:? "FileCacheTypeVersion")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "LustreConfiguration")
            Prelude.<*> ( x Data..:? "NetworkInterfaceIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "StorageCapacity")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable FileCache where
  hashWithSalt _salt FileCache' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` dataRepositoryAssociationIds
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` fileCacheId
      `Prelude.hashWithSalt` fileCacheType
      `Prelude.hashWithSalt` fileCacheTypeVersion
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` networkInterfaceIds
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData FileCache where
  rnf FileCache' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf dataRepositoryAssociationIds
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf fileCacheId
      `Prelude.seq` Prelude.rnf fileCacheType
      `Prelude.seq` Prelude.rnf fileCacheTypeVersion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf lustreConfiguration
      `Prelude.seq` Prelude.rnf networkInterfaceIds
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId
