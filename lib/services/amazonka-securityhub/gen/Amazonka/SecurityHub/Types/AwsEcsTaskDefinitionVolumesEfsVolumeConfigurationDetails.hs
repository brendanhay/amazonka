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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails

-- | Information about the Amazon Elastic File System file system that is
-- used for task storage.
--
-- /See:/ 'newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' smart constructor.
data AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails = AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails'
  { -- | The directory within the Amazon EFS file system to mount as the root
    -- directory inside the host.
    rootDirectory :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable encryption for Amazon EFS data in transit between the
    -- Amazon ECS host and the Amazon EFS server.
    transitEncryption :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EFS file system identifier to use.
    filesystemId :: Prelude.Maybe Prelude.Text,
    -- | The authorization configuration details for the Amazon EFS file system.
    authorizationConfig :: Prelude.Maybe AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails,
    -- | The port to use when sending encrypted data between the Amazon ECS host
    -- and the Amazon EFS server.
    transitEncryptionPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootDirectory', 'awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_rootDirectory' - The directory within the Amazon EFS file system to mount as the root
-- directory inside the host.
--
-- 'transitEncryption', 'awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryption' - Whether to enable encryption for Amazon EFS data in transit between the
-- Amazon ECS host and the Amazon EFS server.
--
-- 'filesystemId', 'awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_filesystemId' - The Amazon EFS file system identifier to use.
--
-- 'authorizationConfig', 'awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_authorizationConfig' - The authorization configuration details for the Amazon EFS file system.
--
-- 'transitEncryptionPort', 'awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryptionPort' - The port to use when sending encrypted data between the Amazon ECS host
-- and the Amazon EFS server.
newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails ::
  AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails =
  AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails'
    { rootDirectory =
        Prelude.Nothing,
      transitEncryption =
        Prelude.Nothing,
      filesystemId =
        Prelude.Nothing,
      authorizationConfig =
        Prelude.Nothing,
      transitEncryptionPort =
        Prelude.Nothing
    }

-- | The directory within the Amazon EFS file system to mount as the root
-- directory inside the host.
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_rootDirectory :: Lens.Lens' AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_rootDirectory = Lens.lens (\AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {rootDirectory} -> rootDirectory) (\s@AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {} a -> s {rootDirectory = a} :: AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails)

-- | Whether to enable encryption for Amazon EFS data in transit between the
-- Amazon ECS host and the Amazon EFS server.
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryption :: Lens.Lens' AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryption = Lens.lens (\AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {transitEncryption} -> transitEncryption) (\s@AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {} a -> s {transitEncryption = a} :: AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails)

-- | The Amazon EFS file system identifier to use.
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_filesystemId :: Lens.Lens' AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_filesystemId = Lens.lens (\AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {filesystemId} -> filesystemId) (\s@AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {} a -> s {filesystemId = a} :: AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails)

-- | The authorization configuration details for the Amazon EFS file system.
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_authorizationConfig :: Lens.Lens' AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (Prelude.Maybe AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails)
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_authorizationConfig = Lens.lens (\AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {authorizationConfig} -> authorizationConfig) (\s@AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {} a -> s {authorizationConfig = a} :: AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails)

-- | The port to use when sending encrypted data between the Amazon ECS host
-- and the Amazon EFS server.
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryptionPort :: Lens.Lens' AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryptionPort = Lens.lens (\AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {transitEncryptionPort} -> transitEncryptionPort) (\s@AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {} a -> s {transitEncryptionPort = a} :: AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails"
      ( \x ->
          AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails'
            Prelude.<$> (x Core..:? "RootDirectory")
              Prelude.<*> (x Core..:? "TransitEncryption")
              Prelude.<*> (x Core..:? "FilesystemId")
              Prelude.<*> (x Core..:? "AuthorizationConfig")
              Prelude.<*> (x Core..:? "TransitEncryptionPort")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails

instance
  Prelude.NFData
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails

instance
  Core.ToJSON
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
  where
  toJSON
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("RootDirectory" Core..=) Prelude.<$> rootDirectory,
              ("TransitEncryption" Core..=)
                Prelude.<$> transitEncryption,
              ("FilesystemId" Core..=) Prelude.<$> filesystemId,
              ("AuthorizationConfig" Core..=)
                Prelude.<$> authorizationConfig,
              ("TransitEncryptionPort" Core..=)
                Prelude.<$> transitEncryptionPort
            ]
        )
