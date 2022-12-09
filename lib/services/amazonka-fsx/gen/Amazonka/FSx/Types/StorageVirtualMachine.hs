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
-- Module      : Amazonka.FSx.Types.StorageVirtualMachine
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.StorageVirtualMachine where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.LifecycleTransitionReason
import Amazonka.FSx.Types.StorageVirtualMachineLifecycle
import Amazonka.FSx.Types.StorageVirtualMachineRootVolumeSecurityStyle
import Amazonka.FSx.Types.SvmActiveDirectoryConfiguration
import Amazonka.FSx.Types.SvmEndpoints
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon FSx for NetApp ONTAP storage virtual machine (SVM)
-- configuration.
--
-- /See:/ 'newStorageVirtualMachine' smart constructor.
data StorageVirtualMachine = StorageVirtualMachine'
  { -- | Describes the Microsoft Active Directory configuration to which the SVM
    -- is joined, if applicable.
    activeDirectoryConfiguration :: Prelude.Maybe SvmActiveDirectoryConfiguration,
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The endpoints that are used to access data or to manage the SVM using
    -- the NetApp ONTAP CLI, REST API, or NetApp CloudManager. They are the
    -- @Iscsi@, @Management@, @Nfs@, and @Smb@ endpoints.
    endpoints :: Prelude.Maybe SvmEndpoints,
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | Describes the SVM\'s lifecycle status.
    --
    -- -   @CREATED@ - The SVM is fully available for use.
    --
    -- -   @CREATING@ - Amazon FSx is creating the new SVM.
    --
    -- -   @DELETING@ - Amazon FSx is deleting an existing SVM.
    --
    -- -   @FAILED@ - Amazon FSx was unable to create the SVM.
    --
    -- -   @MISCONFIGURED@ - The SVM is in a failed but recoverable state.
    --
    -- -   @PENDING@ - Amazon FSx has not started creating the SVM.
    lifecycle :: Prelude.Maybe StorageVirtualMachineLifecycle,
    -- | Describes why the SVM lifecycle state changed.
    lifecycleTransitionReason :: Prelude.Maybe LifecycleTransitionReason,
    -- | The name of the SVM, if provisioned.
    name :: Prelude.Maybe Prelude.Text,
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The security style of the root volume of the SVM.
    rootVolumeSecurityStyle :: Prelude.Maybe StorageVirtualMachineRootVolumeSecurityStyle,
    -- | The SVM\'s system generated unique ID.
    storageVirtualMachineId :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The SVM\'s UUID (universally unique identifier).
    uuid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageVirtualMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryConfiguration', 'storageVirtualMachine_activeDirectoryConfiguration' - Describes the Microsoft Active Directory configuration to which the SVM
-- is joined, if applicable.
--
-- 'creationTime', 'storageVirtualMachine_creationTime' - Undocumented member.
--
-- 'endpoints', 'storageVirtualMachine_endpoints' - The endpoints that are used to access data or to manage the SVM using
-- the NetApp ONTAP CLI, REST API, or NetApp CloudManager. They are the
-- @Iscsi@, @Management@, @Nfs@, and @Smb@ endpoints.
--
-- 'fileSystemId', 'storageVirtualMachine_fileSystemId' - Undocumented member.
--
-- 'lifecycle', 'storageVirtualMachine_lifecycle' - Describes the SVM\'s lifecycle status.
--
-- -   @CREATED@ - The SVM is fully available for use.
--
-- -   @CREATING@ - Amazon FSx is creating the new SVM.
--
-- -   @DELETING@ - Amazon FSx is deleting an existing SVM.
--
-- -   @FAILED@ - Amazon FSx was unable to create the SVM.
--
-- -   @MISCONFIGURED@ - The SVM is in a failed but recoverable state.
--
-- -   @PENDING@ - Amazon FSx has not started creating the SVM.
--
-- 'lifecycleTransitionReason', 'storageVirtualMachine_lifecycleTransitionReason' - Describes why the SVM lifecycle state changed.
--
-- 'name', 'storageVirtualMachine_name' - The name of the SVM, if provisioned.
--
-- 'resourceARN', 'storageVirtualMachine_resourceARN' - Undocumented member.
--
-- 'rootVolumeSecurityStyle', 'storageVirtualMachine_rootVolumeSecurityStyle' - The security style of the root volume of the SVM.
--
-- 'storageVirtualMachineId', 'storageVirtualMachine_storageVirtualMachineId' - The SVM\'s system generated unique ID.
--
-- 'tags', 'storageVirtualMachine_tags' - Undocumented member.
--
-- 'uuid', 'storageVirtualMachine_uuid' - The SVM\'s UUID (universally unique identifier).
newStorageVirtualMachine ::
  StorageVirtualMachine
newStorageVirtualMachine =
  StorageVirtualMachine'
    { activeDirectoryConfiguration =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      lifecycleTransitionReason = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      rootVolumeSecurityStyle = Prelude.Nothing,
      storageVirtualMachineId = Prelude.Nothing,
      tags = Prelude.Nothing,
      uuid = Prelude.Nothing
    }

-- | Describes the Microsoft Active Directory configuration to which the SVM
-- is joined, if applicable.
storageVirtualMachine_activeDirectoryConfiguration :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe SvmActiveDirectoryConfiguration)
storageVirtualMachine_activeDirectoryConfiguration = Lens.lens (\StorageVirtualMachine' {activeDirectoryConfiguration} -> activeDirectoryConfiguration) (\s@StorageVirtualMachine' {} a -> s {activeDirectoryConfiguration = a} :: StorageVirtualMachine)

-- | Undocumented member.
storageVirtualMachine_creationTime :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe Prelude.UTCTime)
storageVirtualMachine_creationTime = Lens.lens (\StorageVirtualMachine' {creationTime} -> creationTime) (\s@StorageVirtualMachine' {} a -> s {creationTime = a} :: StorageVirtualMachine) Prelude.. Lens.mapping Data._Time

-- | The endpoints that are used to access data or to manage the SVM using
-- the NetApp ONTAP CLI, REST API, or NetApp CloudManager. They are the
-- @Iscsi@, @Management@, @Nfs@, and @Smb@ endpoints.
storageVirtualMachine_endpoints :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe SvmEndpoints)
storageVirtualMachine_endpoints = Lens.lens (\StorageVirtualMachine' {endpoints} -> endpoints) (\s@StorageVirtualMachine' {} a -> s {endpoints = a} :: StorageVirtualMachine)

-- | Undocumented member.
storageVirtualMachine_fileSystemId :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe Prelude.Text)
storageVirtualMachine_fileSystemId = Lens.lens (\StorageVirtualMachine' {fileSystemId} -> fileSystemId) (\s@StorageVirtualMachine' {} a -> s {fileSystemId = a} :: StorageVirtualMachine)

-- | Describes the SVM\'s lifecycle status.
--
-- -   @CREATED@ - The SVM is fully available for use.
--
-- -   @CREATING@ - Amazon FSx is creating the new SVM.
--
-- -   @DELETING@ - Amazon FSx is deleting an existing SVM.
--
-- -   @FAILED@ - Amazon FSx was unable to create the SVM.
--
-- -   @MISCONFIGURED@ - The SVM is in a failed but recoverable state.
--
-- -   @PENDING@ - Amazon FSx has not started creating the SVM.
storageVirtualMachine_lifecycle :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe StorageVirtualMachineLifecycle)
storageVirtualMachine_lifecycle = Lens.lens (\StorageVirtualMachine' {lifecycle} -> lifecycle) (\s@StorageVirtualMachine' {} a -> s {lifecycle = a} :: StorageVirtualMachine)

-- | Describes why the SVM lifecycle state changed.
storageVirtualMachine_lifecycleTransitionReason :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe LifecycleTransitionReason)
storageVirtualMachine_lifecycleTransitionReason = Lens.lens (\StorageVirtualMachine' {lifecycleTransitionReason} -> lifecycleTransitionReason) (\s@StorageVirtualMachine' {} a -> s {lifecycleTransitionReason = a} :: StorageVirtualMachine)

-- | The name of the SVM, if provisioned.
storageVirtualMachine_name :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe Prelude.Text)
storageVirtualMachine_name = Lens.lens (\StorageVirtualMachine' {name} -> name) (\s@StorageVirtualMachine' {} a -> s {name = a} :: StorageVirtualMachine)

-- | Undocumented member.
storageVirtualMachine_resourceARN :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe Prelude.Text)
storageVirtualMachine_resourceARN = Lens.lens (\StorageVirtualMachine' {resourceARN} -> resourceARN) (\s@StorageVirtualMachine' {} a -> s {resourceARN = a} :: StorageVirtualMachine)

-- | The security style of the root volume of the SVM.
storageVirtualMachine_rootVolumeSecurityStyle :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe StorageVirtualMachineRootVolumeSecurityStyle)
storageVirtualMachine_rootVolumeSecurityStyle = Lens.lens (\StorageVirtualMachine' {rootVolumeSecurityStyle} -> rootVolumeSecurityStyle) (\s@StorageVirtualMachine' {} a -> s {rootVolumeSecurityStyle = a} :: StorageVirtualMachine)

-- | The SVM\'s system generated unique ID.
storageVirtualMachine_storageVirtualMachineId :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe Prelude.Text)
storageVirtualMachine_storageVirtualMachineId = Lens.lens (\StorageVirtualMachine' {storageVirtualMachineId} -> storageVirtualMachineId) (\s@StorageVirtualMachine' {} a -> s {storageVirtualMachineId = a} :: StorageVirtualMachine)

-- | Undocumented member.
storageVirtualMachine_tags :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe (Prelude.NonEmpty Tag))
storageVirtualMachine_tags = Lens.lens (\StorageVirtualMachine' {tags} -> tags) (\s@StorageVirtualMachine' {} a -> s {tags = a} :: StorageVirtualMachine) Prelude.. Lens.mapping Lens.coerced

-- | The SVM\'s UUID (universally unique identifier).
storageVirtualMachine_uuid :: Lens.Lens' StorageVirtualMachine (Prelude.Maybe Prelude.Text)
storageVirtualMachine_uuid = Lens.lens (\StorageVirtualMachine' {uuid} -> uuid) (\s@StorageVirtualMachine' {} a -> s {uuid = a} :: StorageVirtualMachine)

instance Data.FromJSON StorageVirtualMachine where
  parseJSON =
    Data.withObject
      "StorageVirtualMachine"
      ( \x ->
          StorageVirtualMachine'
            Prelude.<$> (x Data..:? "ActiveDirectoryConfiguration")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Endpoints")
            Prelude.<*> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "LifecycleTransitionReason")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "RootVolumeSecurityStyle")
            Prelude.<*> (x Data..:? "StorageVirtualMachineId")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "UUID")
      )

instance Prelude.Hashable StorageVirtualMachine where
  hashWithSalt _salt StorageVirtualMachine' {..} =
    _salt
      `Prelude.hashWithSalt` activeDirectoryConfiguration
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` lifecycleTransitionReason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` rootVolumeSecurityStyle
      `Prelude.hashWithSalt` storageVirtualMachineId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` uuid

instance Prelude.NFData StorageVirtualMachine where
  rnf StorageVirtualMachine' {..} =
    Prelude.rnf activeDirectoryConfiguration
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf lifecycleTransitionReason
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf rootVolumeSecurityStyle
      `Prelude.seq` Prelude.rnf storageVirtualMachineId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf uuid
