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
-- Module      : Amazonka.ECS.Types.EFSVolumeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.EFSVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.EFSAuthorizationConfig
import Amazonka.ECS.Types.EFSTransitEncryption
import qualified Amazonka.Prelude as Prelude

-- | This parameter is specified when you\'re using an Amazon Elastic File
-- System file system for task storage. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html Amazon EFS volumes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newEFSVolumeConfiguration' smart constructor.
data EFSVolumeConfiguration = EFSVolumeConfiguration'
  { -- | The authorization configuration details for the Amazon EFS file system.
    authorizationConfig :: Prelude.Maybe EFSAuthorizationConfig,
    -- | The directory within the Amazon EFS file system to mount as the root
    -- directory inside the host. If this parameter is omitted, the root of the
    -- Amazon EFS volume will be used. Specifying @\/@ will have the same
    -- effect as omitting this parameter.
    --
    -- If an EFS access point is specified in the @authorizationConfig@, the
    -- root directory parameter must either be omitted or set to @\/@ which
    -- will enforce the path set on the EFS access point.
    rootDirectory :: Prelude.Maybe Prelude.Text,
    -- | Determines whether to use encryption for Amazon EFS data in transit
    -- between the Amazon ECS host and the Amazon EFS server. Transit
    -- encryption must be turned on if Amazon EFS IAM authorization is used. If
    -- this parameter is omitted, the default value of @DISABLED@ is used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting data in transit>
    -- in the /Amazon Elastic File System User Guide/.
    transitEncryption :: Prelude.Maybe EFSTransitEncryption,
    -- | The port to use when sending encrypted data between the Amazon ECS host
    -- and the Amazon EFS server. If you do not specify a transit encryption
    -- port, it will use the port selection strategy that the Amazon EFS mount
    -- helper uses. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS mount helper>
    -- in the /Amazon Elastic File System User Guide/.
    transitEncryptionPort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon EFS file system ID to use.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EFSVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationConfig', 'eFSVolumeConfiguration_authorizationConfig' - The authorization configuration details for the Amazon EFS file system.
--
-- 'rootDirectory', 'eFSVolumeConfiguration_rootDirectory' - The directory within the Amazon EFS file system to mount as the root
-- directory inside the host. If this parameter is omitted, the root of the
-- Amazon EFS volume will be used. Specifying @\/@ will have the same
-- effect as omitting this parameter.
--
-- If an EFS access point is specified in the @authorizationConfig@, the
-- root directory parameter must either be omitted or set to @\/@ which
-- will enforce the path set on the EFS access point.
--
-- 'transitEncryption', 'eFSVolumeConfiguration_transitEncryption' - Determines whether to use encryption for Amazon EFS data in transit
-- between the Amazon ECS host and the Amazon EFS server. Transit
-- encryption must be turned on if Amazon EFS IAM authorization is used. If
-- this parameter is omitted, the default value of @DISABLED@ is used. For
-- more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting data in transit>
-- in the /Amazon Elastic File System User Guide/.
--
-- 'transitEncryptionPort', 'eFSVolumeConfiguration_transitEncryptionPort' - The port to use when sending encrypted data between the Amazon ECS host
-- and the Amazon EFS server. If you do not specify a transit encryption
-- port, it will use the port selection strategy that the Amazon EFS mount
-- helper uses. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS mount helper>
-- in the /Amazon Elastic File System User Guide/.
--
-- 'fileSystemId', 'eFSVolumeConfiguration_fileSystemId' - The Amazon EFS file system ID to use.
newEFSVolumeConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  EFSVolumeConfiguration
newEFSVolumeConfiguration pFileSystemId_ =
  EFSVolumeConfiguration'
    { authorizationConfig =
        Prelude.Nothing,
      rootDirectory = Prelude.Nothing,
      transitEncryption = Prelude.Nothing,
      transitEncryptionPort = Prelude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | The authorization configuration details for the Amazon EFS file system.
eFSVolumeConfiguration_authorizationConfig :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe EFSAuthorizationConfig)
eFSVolumeConfiguration_authorizationConfig = Lens.lens (\EFSVolumeConfiguration' {authorizationConfig} -> authorizationConfig) (\s@EFSVolumeConfiguration' {} a -> s {authorizationConfig = a} :: EFSVolumeConfiguration)

-- | The directory within the Amazon EFS file system to mount as the root
-- directory inside the host. If this parameter is omitted, the root of the
-- Amazon EFS volume will be used. Specifying @\/@ will have the same
-- effect as omitting this parameter.
--
-- If an EFS access point is specified in the @authorizationConfig@, the
-- root directory parameter must either be omitted or set to @\/@ which
-- will enforce the path set on the EFS access point.
eFSVolumeConfiguration_rootDirectory :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe Prelude.Text)
eFSVolumeConfiguration_rootDirectory = Lens.lens (\EFSVolumeConfiguration' {rootDirectory} -> rootDirectory) (\s@EFSVolumeConfiguration' {} a -> s {rootDirectory = a} :: EFSVolumeConfiguration)

-- | Determines whether to use encryption for Amazon EFS data in transit
-- between the Amazon ECS host and the Amazon EFS server. Transit
-- encryption must be turned on if Amazon EFS IAM authorization is used. If
-- this parameter is omitted, the default value of @DISABLED@ is used. For
-- more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting data in transit>
-- in the /Amazon Elastic File System User Guide/.
eFSVolumeConfiguration_transitEncryption :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe EFSTransitEncryption)
eFSVolumeConfiguration_transitEncryption = Lens.lens (\EFSVolumeConfiguration' {transitEncryption} -> transitEncryption) (\s@EFSVolumeConfiguration' {} a -> s {transitEncryption = a} :: EFSVolumeConfiguration)

-- | The port to use when sending encrypted data between the Amazon ECS host
-- and the Amazon EFS server. If you do not specify a transit encryption
-- port, it will use the port selection strategy that the Amazon EFS mount
-- helper uses. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS mount helper>
-- in the /Amazon Elastic File System User Guide/.
eFSVolumeConfiguration_transitEncryptionPort :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe Prelude.Int)
eFSVolumeConfiguration_transitEncryptionPort = Lens.lens (\EFSVolumeConfiguration' {transitEncryptionPort} -> transitEncryptionPort) (\s@EFSVolumeConfiguration' {} a -> s {transitEncryptionPort = a} :: EFSVolumeConfiguration)

-- | The Amazon EFS file system ID to use.
eFSVolumeConfiguration_fileSystemId :: Lens.Lens' EFSVolumeConfiguration Prelude.Text
eFSVolumeConfiguration_fileSystemId = Lens.lens (\EFSVolumeConfiguration' {fileSystemId} -> fileSystemId) (\s@EFSVolumeConfiguration' {} a -> s {fileSystemId = a} :: EFSVolumeConfiguration)

instance Data.FromJSON EFSVolumeConfiguration where
  parseJSON =
    Data.withObject
      "EFSVolumeConfiguration"
      ( \x ->
          EFSVolumeConfiguration'
            Prelude.<$> (x Data..:? "authorizationConfig")
            Prelude.<*> (x Data..:? "rootDirectory")
            Prelude.<*> (x Data..:? "transitEncryption")
            Prelude.<*> (x Data..:? "transitEncryptionPort")
            Prelude.<*> (x Data..: "fileSystemId")
      )

instance Prelude.Hashable EFSVolumeConfiguration where
  hashWithSalt _salt EFSVolumeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` authorizationConfig
      `Prelude.hashWithSalt` rootDirectory
      `Prelude.hashWithSalt` transitEncryption
      `Prelude.hashWithSalt` transitEncryptionPort
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData EFSVolumeConfiguration where
  rnf EFSVolumeConfiguration' {..} =
    Prelude.rnf authorizationConfig
      `Prelude.seq` Prelude.rnf rootDirectory
      `Prelude.seq` Prelude.rnf transitEncryption
      `Prelude.seq` Prelude.rnf transitEncryptionPort
      `Prelude.seq` Prelude.rnf fileSystemId

instance Data.ToJSON EFSVolumeConfiguration where
  toJSON EFSVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorizationConfig" Data..=)
              Prelude.<$> authorizationConfig,
            ("rootDirectory" Data..=) Prelude.<$> rootDirectory,
            ("transitEncryption" Data..=)
              Prelude.<$> transitEncryption,
            ("transitEncryptionPort" Data..=)
              Prelude.<$> transitEncryptionPort,
            Prelude.Just ("fileSystemId" Data..= fileSystemId)
          ]
      )
