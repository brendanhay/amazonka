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
-- Module      : Amazonka.FSx.Types.FileCacheCreating
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileCacheCreating where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.FileCacheFailureDetails
import Amazonka.FSx.Types.FileCacheLifecycle
import Amazonka.FSx.Types.FileCacheLustreConfiguration
import Amazonka.FSx.Types.FileCacheType
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The response object for the Amazon File Cache resource being created in
-- the @CreateFileCache@ operation.
--
-- /See:/ 'newFileCacheCreating' smart constructor.
data FileCacheCreating = FileCacheCreating'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    ownerId :: Prelude.Maybe Prelude.Text,
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
    -- | A boolean flag indicating whether tags for the cache should be copied to
    -- data repository associations.
    copyTagsToDataRepositoryAssociations :: Prelude.Maybe Prelude.Bool,
    -- | A list of IDs of data repository associations that are associated with
    -- this cache.
    dataRepositoryAssociationIds :: Prelude.Maybe [Prelude.Text],
    -- | The storage capacity of the cache in gibibytes (GiB).
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The system-generated, unique ID of the cache.
    fileCacheId :: Prelude.Maybe Prelude.Text,
    -- | A structure providing details of any failures that occurred.
    failureDetails :: Prelude.Maybe FileCacheFailureDetails,
    -- | The Lustre version of the cache, which must be @2.12@.
    fileCacheTypeVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of cache, which must be @LUSTRE@.
    fileCacheType :: Prelude.Maybe FileCacheType,
    -- | Specifies the ID of the Key Management Service (KMS) key to use for
    -- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
    -- specified, the Amazon FSx-managed KMS key for your account is used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
    -- in the /Key Management Service API Reference/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    creationTime :: Prelude.Maybe Data.POSIX,
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The Domain Name System (DNS) name for the cache.
    dNSName :: Prelude.Maybe Prelude.Text,
    vpcId :: Prelude.Maybe Prelude.Text,
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The configuration for the Amazon File Cache resource.
    lustreConfiguration :: Prelude.Maybe FileCacheLustreConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileCacheCreating' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'fileCacheCreating_tags' - Undocumented member.
--
-- 'ownerId', 'fileCacheCreating_ownerId' - Undocumented member.
--
-- 'lifecycle', 'fileCacheCreating_lifecycle' - The lifecycle status of the cache. The following are the possible values
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
-- 'copyTagsToDataRepositoryAssociations', 'fileCacheCreating_copyTagsToDataRepositoryAssociations' - A boolean flag indicating whether tags for the cache should be copied to
-- data repository associations.
--
-- 'dataRepositoryAssociationIds', 'fileCacheCreating_dataRepositoryAssociationIds' - A list of IDs of data repository associations that are associated with
-- this cache.
--
-- 'storageCapacity', 'fileCacheCreating_storageCapacity' - The storage capacity of the cache in gibibytes (GiB).
--
-- 'fileCacheId', 'fileCacheCreating_fileCacheId' - The system-generated, unique ID of the cache.
--
-- 'failureDetails', 'fileCacheCreating_failureDetails' - A structure providing details of any failures that occurred.
--
-- 'fileCacheTypeVersion', 'fileCacheCreating_fileCacheTypeVersion' - The Lustre version of the cache, which must be @2.12@.
--
-- 'fileCacheType', 'fileCacheCreating_fileCacheType' - The type of cache, which must be @LUSTRE@.
--
-- 'kmsKeyId', 'fileCacheCreating_kmsKeyId' - Specifies the ID of the Key Management Service (KMS) key to use for
-- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
-- specified, the Amazon FSx-managed KMS key for your account is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
-- in the /Key Management Service API Reference/.
--
-- 'creationTime', 'fileCacheCreating_creationTime' - Undocumented member.
--
-- 'resourceARN', 'fileCacheCreating_resourceARN' - Undocumented member.
--
-- 'dNSName', 'fileCacheCreating_dNSName' - The Domain Name System (DNS) name for the cache.
--
-- 'vpcId', 'fileCacheCreating_vpcId' - Undocumented member.
--
-- 'networkInterfaceIds', 'fileCacheCreating_networkInterfaceIds' - Undocumented member.
--
-- 'subnetIds', 'fileCacheCreating_subnetIds' - Undocumented member.
--
-- 'lustreConfiguration', 'fileCacheCreating_lustreConfiguration' - The configuration for the Amazon File Cache resource.
newFileCacheCreating ::
  FileCacheCreating
newFileCacheCreating =
  FileCacheCreating'
    { tags = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      copyTagsToDataRepositoryAssociations =
        Prelude.Nothing,
      dataRepositoryAssociationIds = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      fileCacheId = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      fileCacheTypeVersion = Prelude.Nothing,
      fileCacheType = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      dNSName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      networkInterfaceIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      lustreConfiguration = Prelude.Nothing
    }

-- | Undocumented member.
fileCacheCreating_tags :: Lens.Lens' FileCacheCreating (Prelude.Maybe (Prelude.NonEmpty Tag))
fileCacheCreating_tags = Lens.lens (\FileCacheCreating' {tags} -> tags) (\s@FileCacheCreating' {} a -> s {tags = a} :: FileCacheCreating) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
fileCacheCreating_ownerId :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Text)
fileCacheCreating_ownerId = Lens.lens (\FileCacheCreating' {ownerId} -> ownerId) (\s@FileCacheCreating' {} a -> s {ownerId = a} :: FileCacheCreating)

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
fileCacheCreating_lifecycle :: Lens.Lens' FileCacheCreating (Prelude.Maybe FileCacheLifecycle)
fileCacheCreating_lifecycle = Lens.lens (\FileCacheCreating' {lifecycle} -> lifecycle) (\s@FileCacheCreating' {} a -> s {lifecycle = a} :: FileCacheCreating)

-- | A boolean flag indicating whether tags for the cache should be copied to
-- data repository associations.
fileCacheCreating_copyTagsToDataRepositoryAssociations :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Bool)
fileCacheCreating_copyTagsToDataRepositoryAssociations = Lens.lens (\FileCacheCreating' {copyTagsToDataRepositoryAssociations} -> copyTagsToDataRepositoryAssociations) (\s@FileCacheCreating' {} a -> s {copyTagsToDataRepositoryAssociations = a} :: FileCacheCreating)

-- | A list of IDs of data repository associations that are associated with
-- this cache.
fileCacheCreating_dataRepositoryAssociationIds :: Lens.Lens' FileCacheCreating (Prelude.Maybe [Prelude.Text])
fileCacheCreating_dataRepositoryAssociationIds = Lens.lens (\FileCacheCreating' {dataRepositoryAssociationIds} -> dataRepositoryAssociationIds) (\s@FileCacheCreating' {} a -> s {dataRepositoryAssociationIds = a} :: FileCacheCreating) Prelude.. Lens.mapping Lens.coerced

-- | The storage capacity of the cache in gibibytes (GiB).
fileCacheCreating_storageCapacity :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Natural)
fileCacheCreating_storageCapacity = Lens.lens (\FileCacheCreating' {storageCapacity} -> storageCapacity) (\s@FileCacheCreating' {} a -> s {storageCapacity = a} :: FileCacheCreating)

-- | The system-generated, unique ID of the cache.
fileCacheCreating_fileCacheId :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Text)
fileCacheCreating_fileCacheId = Lens.lens (\FileCacheCreating' {fileCacheId} -> fileCacheId) (\s@FileCacheCreating' {} a -> s {fileCacheId = a} :: FileCacheCreating)

-- | A structure providing details of any failures that occurred.
fileCacheCreating_failureDetails :: Lens.Lens' FileCacheCreating (Prelude.Maybe FileCacheFailureDetails)
fileCacheCreating_failureDetails = Lens.lens (\FileCacheCreating' {failureDetails} -> failureDetails) (\s@FileCacheCreating' {} a -> s {failureDetails = a} :: FileCacheCreating)

-- | The Lustre version of the cache, which must be @2.12@.
fileCacheCreating_fileCacheTypeVersion :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Text)
fileCacheCreating_fileCacheTypeVersion = Lens.lens (\FileCacheCreating' {fileCacheTypeVersion} -> fileCacheTypeVersion) (\s@FileCacheCreating' {} a -> s {fileCacheTypeVersion = a} :: FileCacheCreating)

-- | The type of cache, which must be @LUSTRE@.
fileCacheCreating_fileCacheType :: Lens.Lens' FileCacheCreating (Prelude.Maybe FileCacheType)
fileCacheCreating_fileCacheType = Lens.lens (\FileCacheCreating' {fileCacheType} -> fileCacheType) (\s@FileCacheCreating' {} a -> s {fileCacheType = a} :: FileCacheCreating)

-- | Specifies the ID of the Key Management Service (KMS) key to use for
-- encrypting data on an Amazon File Cache. If a @KmsKeyId@ isn\'t
-- specified, the Amazon FSx-managed KMS key for your account is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html Encrypt>
-- in the /Key Management Service API Reference/.
fileCacheCreating_kmsKeyId :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Text)
fileCacheCreating_kmsKeyId = Lens.lens (\FileCacheCreating' {kmsKeyId} -> kmsKeyId) (\s@FileCacheCreating' {} a -> s {kmsKeyId = a} :: FileCacheCreating)

-- | Undocumented member.
fileCacheCreating_creationTime :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.UTCTime)
fileCacheCreating_creationTime = Lens.lens (\FileCacheCreating' {creationTime} -> creationTime) (\s@FileCacheCreating' {} a -> s {creationTime = a} :: FileCacheCreating) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
fileCacheCreating_resourceARN :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Text)
fileCacheCreating_resourceARN = Lens.lens (\FileCacheCreating' {resourceARN} -> resourceARN) (\s@FileCacheCreating' {} a -> s {resourceARN = a} :: FileCacheCreating)

-- | The Domain Name System (DNS) name for the cache.
fileCacheCreating_dNSName :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Text)
fileCacheCreating_dNSName = Lens.lens (\FileCacheCreating' {dNSName} -> dNSName) (\s@FileCacheCreating' {} a -> s {dNSName = a} :: FileCacheCreating)

-- | Undocumented member.
fileCacheCreating_vpcId :: Lens.Lens' FileCacheCreating (Prelude.Maybe Prelude.Text)
fileCacheCreating_vpcId = Lens.lens (\FileCacheCreating' {vpcId} -> vpcId) (\s@FileCacheCreating' {} a -> s {vpcId = a} :: FileCacheCreating)

-- | Undocumented member.
fileCacheCreating_networkInterfaceIds :: Lens.Lens' FileCacheCreating (Prelude.Maybe [Prelude.Text])
fileCacheCreating_networkInterfaceIds = Lens.lens (\FileCacheCreating' {networkInterfaceIds} -> networkInterfaceIds) (\s@FileCacheCreating' {} a -> s {networkInterfaceIds = a} :: FileCacheCreating) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
fileCacheCreating_subnetIds :: Lens.Lens' FileCacheCreating (Prelude.Maybe [Prelude.Text])
fileCacheCreating_subnetIds = Lens.lens (\FileCacheCreating' {subnetIds} -> subnetIds) (\s@FileCacheCreating' {} a -> s {subnetIds = a} :: FileCacheCreating) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for the Amazon File Cache resource.
fileCacheCreating_lustreConfiguration :: Lens.Lens' FileCacheCreating (Prelude.Maybe FileCacheLustreConfiguration)
fileCacheCreating_lustreConfiguration = Lens.lens (\FileCacheCreating' {lustreConfiguration} -> lustreConfiguration) (\s@FileCacheCreating' {} a -> s {lustreConfiguration = a} :: FileCacheCreating)

instance Data.FromJSON FileCacheCreating where
  parseJSON =
    Data.withObject
      "FileCacheCreating"
      ( \x ->
          FileCacheCreating'
            Prelude.<$> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "CopyTagsToDataRepositoryAssociations")
            Prelude.<*> ( x Data..:? "DataRepositoryAssociationIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StorageCapacity")
            Prelude.<*> (x Data..:? "FileCacheId")
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "FileCacheTypeVersion")
            Prelude.<*> (x Data..:? "FileCacheType")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "DNSName")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> ( x Data..:? "NetworkInterfaceIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LustreConfiguration")
      )

instance Prelude.Hashable FileCacheCreating where
  hashWithSalt _salt FileCacheCreating' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` copyTagsToDataRepositoryAssociations
      `Prelude.hashWithSalt` dataRepositoryAssociationIds
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` fileCacheId
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` fileCacheTypeVersion
      `Prelude.hashWithSalt` fileCacheType
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` networkInterfaceIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` lustreConfiguration

instance Prelude.NFData FileCacheCreating where
  rnf FileCacheCreating' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf copyTagsToDataRepositoryAssociations
      `Prelude.seq` Prelude.rnf dataRepositoryAssociationIds
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf fileCacheId
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf fileCacheTypeVersion
      `Prelude.seq` Prelude.rnf fileCacheType
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf networkInterfaceIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf lustreConfiguration
