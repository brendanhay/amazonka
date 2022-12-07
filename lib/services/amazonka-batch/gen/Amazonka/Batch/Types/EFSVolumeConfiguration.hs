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
-- Module      : Amazonka.Batch.Types.EFSVolumeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EFSVolumeConfiguration where

import Amazonka.Batch.Types.EFSAuthorizationConfig
import Amazonka.Batch.Types.EFSTransitEncryption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This is used when you\'re using an Amazon Elastic File System file
-- system for job storage. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/efs-volumes.html Amazon EFS Volumes>
-- in the /Batch User Guide/.
--
-- /See:/ 'newEFSVolumeConfiguration' smart constructor.
data EFSVolumeConfiguration = EFSVolumeConfiguration'
  { -- | The port to use when sending encrypted data between the Amazon ECS host
    -- and the Amazon EFS server. If you don\'t specify a transit encryption
    -- port, it uses the port selection strategy that the Amazon EFS mount
    -- helper uses. The value must be between 0 and 65,535. For more
    -- information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS mount helper>
    -- in the /Amazon Elastic File System User Guide/.
    transitEncryptionPort :: Prelude.Maybe Prelude.Int,
    -- | The directory within the Amazon EFS file system to mount as the root
    -- directory inside the host. If this parameter is omitted, the root of the
    -- Amazon EFS volume is used instead. Specifying @\/@ has the same effect
    -- as omitting this parameter. The maximum length is 4,096 characters.
    --
    -- If an EFS access point is specified in the @authorizationConfig@, the
    -- root directory parameter must either be omitted or set to @\/@, which
    -- enforces the path set on the Amazon EFS access point.
    rootDirectory :: Prelude.Maybe Prelude.Text,
    -- | Determines whether to enable encryption for Amazon EFS data in transit
    -- between the Amazon ECS host and the Amazon EFS server. Transit
    -- encryption must be enabled if Amazon EFS IAM authorization is used. If
    -- this parameter is omitted, the default value of @DISABLED@ is used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting data in transit>
    -- in the /Amazon Elastic File System User Guide/.
    transitEncryption :: Prelude.Maybe EFSTransitEncryption,
    -- | The authorization configuration details for the Amazon EFS file system.
    authorizationConfig :: Prelude.Maybe EFSAuthorizationConfig,
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
-- 'transitEncryptionPort', 'eFSVolumeConfiguration_transitEncryptionPort' - The port to use when sending encrypted data between the Amazon ECS host
-- and the Amazon EFS server. If you don\'t specify a transit encryption
-- port, it uses the port selection strategy that the Amazon EFS mount
-- helper uses. The value must be between 0 and 65,535. For more
-- information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS mount helper>
-- in the /Amazon Elastic File System User Guide/.
--
-- 'rootDirectory', 'eFSVolumeConfiguration_rootDirectory' - The directory within the Amazon EFS file system to mount as the root
-- directory inside the host. If this parameter is omitted, the root of the
-- Amazon EFS volume is used instead. Specifying @\/@ has the same effect
-- as omitting this parameter. The maximum length is 4,096 characters.
--
-- If an EFS access point is specified in the @authorizationConfig@, the
-- root directory parameter must either be omitted or set to @\/@, which
-- enforces the path set on the Amazon EFS access point.
--
-- 'transitEncryption', 'eFSVolumeConfiguration_transitEncryption' - Determines whether to enable encryption for Amazon EFS data in transit
-- between the Amazon ECS host and the Amazon EFS server. Transit
-- encryption must be enabled if Amazon EFS IAM authorization is used. If
-- this parameter is omitted, the default value of @DISABLED@ is used. For
-- more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting data in transit>
-- in the /Amazon Elastic File System User Guide/.
--
-- 'authorizationConfig', 'eFSVolumeConfiguration_authorizationConfig' - The authorization configuration details for the Amazon EFS file system.
--
-- 'fileSystemId', 'eFSVolumeConfiguration_fileSystemId' - The Amazon EFS file system ID to use.
newEFSVolumeConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  EFSVolumeConfiguration
newEFSVolumeConfiguration pFileSystemId_ =
  EFSVolumeConfiguration'
    { transitEncryptionPort =
        Prelude.Nothing,
      rootDirectory = Prelude.Nothing,
      transitEncryption = Prelude.Nothing,
      authorizationConfig = Prelude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | The port to use when sending encrypted data between the Amazon ECS host
-- and the Amazon EFS server. If you don\'t specify a transit encryption
-- port, it uses the port selection strategy that the Amazon EFS mount
-- helper uses. The value must be between 0 and 65,535. For more
-- information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-mount-helper.html EFS mount helper>
-- in the /Amazon Elastic File System User Guide/.
eFSVolumeConfiguration_transitEncryptionPort :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe Prelude.Int)
eFSVolumeConfiguration_transitEncryptionPort = Lens.lens (\EFSVolumeConfiguration' {transitEncryptionPort} -> transitEncryptionPort) (\s@EFSVolumeConfiguration' {} a -> s {transitEncryptionPort = a} :: EFSVolumeConfiguration)

-- | The directory within the Amazon EFS file system to mount as the root
-- directory inside the host. If this parameter is omitted, the root of the
-- Amazon EFS volume is used instead. Specifying @\/@ has the same effect
-- as omitting this parameter. The maximum length is 4,096 characters.
--
-- If an EFS access point is specified in the @authorizationConfig@, the
-- root directory parameter must either be omitted or set to @\/@, which
-- enforces the path set on the Amazon EFS access point.
eFSVolumeConfiguration_rootDirectory :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe Prelude.Text)
eFSVolumeConfiguration_rootDirectory = Lens.lens (\EFSVolumeConfiguration' {rootDirectory} -> rootDirectory) (\s@EFSVolumeConfiguration' {} a -> s {rootDirectory = a} :: EFSVolumeConfiguration)

-- | Determines whether to enable encryption for Amazon EFS data in transit
-- between the Amazon ECS host and the Amazon EFS server. Transit
-- encryption must be enabled if Amazon EFS IAM authorization is used. If
-- this parameter is omitted, the default value of @DISABLED@ is used. For
-- more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/encryption-in-transit.html Encrypting data in transit>
-- in the /Amazon Elastic File System User Guide/.
eFSVolumeConfiguration_transitEncryption :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe EFSTransitEncryption)
eFSVolumeConfiguration_transitEncryption = Lens.lens (\EFSVolumeConfiguration' {transitEncryption} -> transitEncryption) (\s@EFSVolumeConfiguration' {} a -> s {transitEncryption = a} :: EFSVolumeConfiguration)

-- | The authorization configuration details for the Amazon EFS file system.
eFSVolumeConfiguration_authorizationConfig :: Lens.Lens' EFSVolumeConfiguration (Prelude.Maybe EFSAuthorizationConfig)
eFSVolumeConfiguration_authorizationConfig = Lens.lens (\EFSVolumeConfiguration' {authorizationConfig} -> authorizationConfig) (\s@EFSVolumeConfiguration' {} a -> s {authorizationConfig = a} :: EFSVolumeConfiguration)

-- | The Amazon EFS file system ID to use.
eFSVolumeConfiguration_fileSystemId :: Lens.Lens' EFSVolumeConfiguration Prelude.Text
eFSVolumeConfiguration_fileSystemId = Lens.lens (\EFSVolumeConfiguration' {fileSystemId} -> fileSystemId) (\s@EFSVolumeConfiguration' {} a -> s {fileSystemId = a} :: EFSVolumeConfiguration)

instance Data.FromJSON EFSVolumeConfiguration where
  parseJSON =
    Data.withObject
      "EFSVolumeConfiguration"
      ( \x ->
          EFSVolumeConfiguration'
            Prelude.<$> (x Data..:? "transitEncryptionPort")
            Prelude.<*> (x Data..:? "rootDirectory")
            Prelude.<*> (x Data..:? "transitEncryption")
            Prelude.<*> (x Data..:? "authorizationConfig")
            Prelude.<*> (x Data..: "fileSystemId")
      )

instance Prelude.Hashable EFSVolumeConfiguration where
  hashWithSalt _salt EFSVolumeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` transitEncryptionPort
      `Prelude.hashWithSalt` rootDirectory
      `Prelude.hashWithSalt` transitEncryption
      `Prelude.hashWithSalt` authorizationConfig
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData EFSVolumeConfiguration where
  rnf EFSVolumeConfiguration' {..} =
    Prelude.rnf transitEncryptionPort
      `Prelude.seq` Prelude.rnf rootDirectory
      `Prelude.seq` Prelude.rnf transitEncryption
      `Prelude.seq` Prelude.rnf authorizationConfig
      `Prelude.seq` Prelude.rnf fileSystemId

instance Data.ToJSON EFSVolumeConfiguration where
  toJSON EFSVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("transitEncryptionPort" Data..=)
              Prelude.<$> transitEncryptionPort,
            ("rootDirectory" Data..=) Prelude.<$> rootDirectory,
            ("transitEncryption" Data..=)
              Prelude.<$> transitEncryption,
            ("authorizationConfig" Data..=)
              Prelude.<$> authorizationConfig,
            Prelude.Just ("fileSystemId" Data..= fileSystemId)
          ]
      )
