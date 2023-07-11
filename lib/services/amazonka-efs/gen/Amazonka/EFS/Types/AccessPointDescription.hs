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
-- Module      : Amazonka.EFS.Types.AccessPointDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.AccessPointDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types.LifeCycleState
import Amazonka.EFS.Types.PosixUser
import Amazonka.EFS.Types.RootDirectory
import Amazonka.EFS.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Provides a description of an EFS file system access point.
--
-- /See:/ 'newAccessPointDescription' smart constructor.
data AccessPointDescription = AccessPointDescription'
  { -- | The unique Amazon Resource Name (ARN) associated with the access point.
    accessPointArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the access point, assigned by Amazon EFS.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | The opaque string specified in the request to ensure idempotent
    -- creation.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EFS file system that the access point applies to.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the lifecycle phase of the access point.
    lifeCycleState :: Prelude.Maybe LifeCycleState,
    -- | The name of the access point. This is the value of the @Name@ tag.
    name :: Prelude.Maybe Prelude.Text,
    -- | Identified the Amazon Web Services account that owns the access point
    -- resource.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The full POSIX identity, including the user ID, group ID, and secondary
    -- group IDs on the access point that is used for all file operations by
    -- NFS clients using the access point.
    posixUser :: Prelude.Maybe PosixUser,
    -- | The directory on the Amazon EFS file system that the access point
    -- exposes as the root directory to NFS clients using the access point.
    rootDirectory :: Prelude.Maybe RootDirectory,
    -- | The tags associated with the access point, presented as an array of Tag
    -- objects.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessPointDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointArn', 'accessPointDescription_accessPointArn' - The unique Amazon Resource Name (ARN) associated with the access point.
--
-- 'accessPointId', 'accessPointDescription_accessPointId' - The ID of the access point, assigned by Amazon EFS.
--
-- 'clientToken', 'accessPointDescription_clientToken' - The opaque string specified in the request to ensure idempotent
-- creation.
--
-- 'fileSystemId', 'accessPointDescription_fileSystemId' - The ID of the EFS file system that the access point applies to.
--
-- 'lifeCycleState', 'accessPointDescription_lifeCycleState' - Identifies the lifecycle phase of the access point.
--
-- 'name', 'accessPointDescription_name' - The name of the access point. This is the value of the @Name@ tag.
--
-- 'ownerId', 'accessPointDescription_ownerId' - Identified the Amazon Web Services account that owns the access point
-- resource.
--
-- 'posixUser', 'accessPointDescription_posixUser' - The full POSIX identity, including the user ID, group ID, and secondary
-- group IDs on the access point that is used for all file operations by
-- NFS clients using the access point.
--
-- 'rootDirectory', 'accessPointDescription_rootDirectory' - The directory on the Amazon EFS file system that the access point
-- exposes as the root directory to NFS clients using the access point.
--
-- 'tags', 'accessPointDescription_tags' - The tags associated with the access point, presented as an array of Tag
-- objects.
newAccessPointDescription ::
  AccessPointDescription
newAccessPointDescription =
  AccessPointDescription'
    { accessPointArn =
        Prelude.Nothing,
      accessPointId = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      lifeCycleState = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      posixUser = Prelude.Nothing,
      rootDirectory = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The unique Amazon Resource Name (ARN) associated with the access point.
accessPointDescription_accessPointArn :: Lens.Lens' AccessPointDescription (Prelude.Maybe Prelude.Text)
accessPointDescription_accessPointArn = Lens.lens (\AccessPointDescription' {accessPointArn} -> accessPointArn) (\s@AccessPointDescription' {} a -> s {accessPointArn = a} :: AccessPointDescription)

-- | The ID of the access point, assigned by Amazon EFS.
accessPointDescription_accessPointId :: Lens.Lens' AccessPointDescription (Prelude.Maybe Prelude.Text)
accessPointDescription_accessPointId = Lens.lens (\AccessPointDescription' {accessPointId} -> accessPointId) (\s@AccessPointDescription' {} a -> s {accessPointId = a} :: AccessPointDescription)

-- | The opaque string specified in the request to ensure idempotent
-- creation.
accessPointDescription_clientToken :: Lens.Lens' AccessPointDescription (Prelude.Maybe Prelude.Text)
accessPointDescription_clientToken = Lens.lens (\AccessPointDescription' {clientToken} -> clientToken) (\s@AccessPointDescription' {} a -> s {clientToken = a} :: AccessPointDescription)

-- | The ID of the EFS file system that the access point applies to.
accessPointDescription_fileSystemId :: Lens.Lens' AccessPointDescription (Prelude.Maybe Prelude.Text)
accessPointDescription_fileSystemId = Lens.lens (\AccessPointDescription' {fileSystemId} -> fileSystemId) (\s@AccessPointDescription' {} a -> s {fileSystemId = a} :: AccessPointDescription)

-- | Identifies the lifecycle phase of the access point.
accessPointDescription_lifeCycleState :: Lens.Lens' AccessPointDescription (Prelude.Maybe LifeCycleState)
accessPointDescription_lifeCycleState = Lens.lens (\AccessPointDescription' {lifeCycleState} -> lifeCycleState) (\s@AccessPointDescription' {} a -> s {lifeCycleState = a} :: AccessPointDescription)

-- | The name of the access point. This is the value of the @Name@ tag.
accessPointDescription_name :: Lens.Lens' AccessPointDescription (Prelude.Maybe Prelude.Text)
accessPointDescription_name = Lens.lens (\AccessPointDescription' {name} -> name) (\s@AccessPointDescription' {} a -> s {name = a} :: AccessPointDescription)

-- | Identified the Amazon Web Services account that owns the access point
-- resource.
accessPointDescription_ownerId :: Lens.Lens' AccessPointDescription (Prelude.Maybe Prelude.Text)
accessPointDescription_ownerId = Lens.lens (\AccessPointDescription' {ownerId} -> ownerId) (\s@AccessPointDescription' {} a -> s {ownerId = a} :: AccessPointDescription)

-- | The full POSIX identity, including the user ID, group ID, and secondary
-- group IDs on the access point that is used for all file operations by
-- NFS clients using the access point.
accessPointDescription_posixUser :: Lens.Lens' AccessPointDescription (Prelude.Maybe PosixUser)
accessPointDescription_posixUser = Lens.lens (\AccessPointDescription' {posixUser} -> posixUser) (\s@AccessPointDescription' {} a -> s {posixUser = a} :: AccessPointDescription)

-- | The directory on the Amazon EFS file system that the access point
-- exposes as the root directory to NFS clients using the access point.
accessPointDescription_rootDirectory :: Lens.Lens' AccessPointDescription (Prelude.Maybe RootDirectory)
accessPointDescription_rootDirectory = Lens.lens (\AccessPointDescription' {rootDirectory} -> rootDirectory) (\s@AccessPointDescription' {} a -> s {rootDirectory = a} :: AccessPointDescription)

-- | The tags associated with the access point, presented as an array of Tag
-- objects.
accessPointDescription_tags :: Lens.Lens' AccessPointDescription (Prelude.Maybe [Tag])
accessPointDescription_tags = Lens.lens (\AccessPointDescription' {tags} -> tags) (\s@AccessPointDescription' {} a -> s {tags = a} :: AccessPointDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AccessPointDescription where
  parseJSON =
    Data.withObject
      "AccessPointDescription"
      ( \x ->
          AccessPointDescription'
            Prelude.<$> (x Data..:? "AccessPointArn")
            Prelude.<*> (x Data..:? "AccessPointId")
            Prelude.<*> (x Data..:? "ClientToken")
            Prelude.<*> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "LifeCycleState")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "PosixUser")
            Prelude.<*> (x Data..:? "RootDirectory")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AccessPointDescription where
  hashWithSalt _salt AccessPointDescription' {..} =
    _salt
      `Prelude.hashWithSalt` accessPointArn
      `Prelude.hashWithSalt` accessPointId
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` lifeCycleState
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` posixUser
      `Prelude.hashWithSalt` rootDirectory
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AccessPointDescription where
  rnf AccessPointDescription' {..} =
    Prelude.rnf accessPointArn
      `Prelude.seq` Prelude.rnf accessPointId
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf lifeCycleState
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf posixUser
      `Prelude.seq` Prelude.rnf rootDirectory
      `Prelude.seq` Prelude.rnf tags
