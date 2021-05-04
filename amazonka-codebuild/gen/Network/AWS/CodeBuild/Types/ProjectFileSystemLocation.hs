{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectFileSystemLocation where

import Network.AWS.CodeBuild.Types.FileSystemType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a file system created by Amazon Elastic File System
-- (EFS). For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/whatisefs.html What Is Amazon Elastic File System?>
--
-- /See:/ 'newProjectFileSystemLocation' smart constructor.
data ProjectFileSystemLocation = ProjectFileSystemLocation'
  { -- | The name used to access a file system created by Amazon EFS. CodeBuild
    -- creates an environment variable by appending the @identifier@ in all
    -- capital letters to @CODEBUILD_@. For example, if you specify @my_efs@
    -- for @identifier@, a new environment variable is create named
    -- @CODEBUILD_MY_EFS@.
    --
    -- The @identifier@ is used to mount your file system.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The mount options for a file system created by AWS EFS. The default
    -- mount options used by CodeBuild are
    -- @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options>.
    mountOptions :: Prelude.Maybe Prelude.Text,
    -- | The location in the container where you mount the file system.
    mountPoint :: Prelude.Maybe Prelude.Text,
    -- | The type of the file system. The one supported type is @EFS@.
    type' :: Prelude.Maybe FileSystemType,
    -- | A string that specifies the location of the file system created by
    -- Amazon EFS. Its format is @efs-dns-name:\/directory-path@. You can find
    -- the DNS name of file system when you view it in the AWS EFS console. The
    -- directory path is a path to a directory in the file system that
    -- CodeBuild mounts. For example, if the DNS name of a file system is
    -- @fs-abcd1234.efs.us-west-2.amazonaws.com@, and its mount directory is
    -- @my-efs-mount-directory@, then the @location@ is
    -- @fs-abcd1234.efs.us-west-2.amazonaws.com:\/my-efs-mount-directory@.
    --
    -- The directory path in the format @efs-dns-name:\/directory-path@ is
    -- optional. If you do not specify a directory path, the location is only
    -- the DNS name and CodeBuild mounts the entire file system.
    location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProjectFileSystemLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'projectFileSystemLocation_identifier' - The name used to access a file system created by Amazon EFS. CodeBuild
-- creates an environment variable by appending the @identifier@ in all
-- capital letters to @CODEBUILD_@. For example, if you specify @my_efs@
-- for @identifier@, a new environment variable is create named
-- @CODEBUILD_MY_EFS@.
--
-- The @identifier@ is used to mount your file system.
--
-- 'mountOptions', 'projectFileSystemLocation_mountOptions' - The mount options for a file system created by AWS EFS. The default
-- mount options used by CodeBuild are
-- @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@. For
-- more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options>.
--
-- 'mountPoint', 'projectFileSystemLocation_mountPoint' - The location in the container where you mount the file system.
--
-- 'type'', 'projectFileSystemLocation_type' - The type of the file system. The one supported type is @EFS@.
--
-- 'location', 'projectFileSystemLocation_location' - A string that specifies the location of the file system created by
-- Amazon EFS. Its format is @efs-dns-name:\/directory-path@. You can find
-- the DNS name of file system when you view it in the AWS EFS console. The
-- directory path is a path to a directory in the file system that
-- CodeBuild mounts. For example, if the DNS name of a file system is
-- @fs-abcd1234.efs.us-west-2.amazonaws.com@, and its mount directory is
-- @my-efs-mount-directory@, then the @location@ is
-- @fs-abcd1234.efs.us-west-2.amazonaws.com:\/my-efs-mount-directory@.
--
-- The directory path in the format @efs-dns-name:\/directory-path@ is
-- optional. If you do not specify a directory path, the location is only
-- the DNS name and CodeBuild mounts the entire file system.
newProjectFileSystemLocation ::
  ProjectFileSystemLocation
newProjectFileSystemLocation =
  ProjectFileSystemLocation'
    { identifier =
        Prelude.Nothing,
      mountOptions = Prelude.Nothing,
      mountPoint = Prelude.Nothing,
      type' = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The name used to access a file system created by Amazon EFS. CodeBuild
-- creates an environment variable by appending the @identifier@ in all
-- capital letters to @CODEBUILD_@. For example, if you specify @my_efs@
-- for @identifier@, a new environment variable is create named
-- @CODEBUILD_MY_EFS@.
--
-- The @identifier@ is used to mount your file system.
projectFileSystemLocation_identifier :: Lens.Lens' ProjectFileSystemLocation (Prelude.Maybe Prelude.Text)
projectFileSystemLocation_identifier = Lens.lens (\ProjectFileSystemLocation' {identifier} -> identifier) (\s@ProjectFileSystemLocation' {} a -> s {identifier = a} :: ProjectFileSystemLocation)

-- | The mount options for a file system created by AWS EFS. The default
-- mount options used by CodeBuild are
-- @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@. For
-- more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options>.
projectFileSystemLocation_mountOptions :: Lens.Lens' ProjectFileSystemLocation (Prelude.Maybe Prelude.Text)
projectFileSystemLocation_mountOptions = Lens.lens (\ProjectFileSystemLocation' {mountOptions} -> mountOptions) (\s@ProjectFileSystemLocation' {} a -> s {mountOptions = a} :: ProjectFileSystemLocation)

-- | The location in the container where you mount the file system.
projectFileSystemLocation_mountPoint :: Lens.Lens' ProjectFileSystemLocation (Prelude.Maybe Prelude.Text)
projectFileSystemLocation_mountPoint = Lens.lens (\ProjectFileSystemLocation' {mountPoint} -> mountPoint) (\s@ProjectFileSystemLocation' {} a -> s {mountPoint = a} :: ProjectFileSystemLocation)

-- | The type of the file system. The one supported type is @EFS@.
projectFileSystemLocation_type :: Lens.Lens' ProjectFileSystemLocation (Prelude.Maybe FileSystemType)
projectFileSystemLocation_type = Lens.lens (\ProjectFileSystemLocation' {type'} -> type') (\s@ProjectFileSystemLocation' {} a -> s {type' = a} :: ProjectFileSystemLocation)

-- | A string that specifies the location of the file system created by
-- Amazon EFS. Its format is @efs-dns-name:\/directory-path@. You can find
-- the DNS name of file system when you view it in the AWS EFS console. The
-- directory path is a path to a directory in the file system that
-- CodeBuild mounts. For example, if the DNS name of a file system is
-- @fs-abcd1234.efs.us-west-2.amazonaws.com@, and its mount directory is
-- @my-efs-mount-directory@, then the @location@ is
-- @fs-abcd1234.efs.us-west-2.amazonaws.com:\/my-efs-mount-directory@.
--
-- The directory path in the format @efs-dns-name:\/directory-path@ is
-- optional. If you do not specify a directory path, the location is only
-- the DNS name and CodeBuild mounts the entire file system.
projectFileSystemLocation_location :: Lens.Lens' ProjectFileSystemLocation (Prelude.Maybe Prelude.Text)
projectFileSystemLocation_location = Lens.lens (\ProjectFileSystemLocation' {location} -> location) (\s@ProjectFileSystemLocation' {} a -> s {location = a} :: ProjectFileSystemLocation)

instance Prelude.FromJSON ProjectFileSystemLocation where
  parseJSON =
    Prelude.withObject
      "ProjectFileSystemLocation"
      ( \x ->
          ProjectFileSystemLocation'
            Prelude.<$> (x Prelude..:? "identifier")
            Prelude.<*> (x Prelude..:? "mountOptions")
            Prelude.<*> (x Prelude..:? "mountPoint")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "location")
      )

instance Prelude.Hashable ProjectFileSystemLocation

instance Prelude.NFData ProjectFileSystemLocation

instance Prelude.ToJSON ProjectFileSystemLocation where
  toJSON ProjectFileSystemLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("identifier" Prelude..=) Prelude.<$> identifier,
            ("mountOptions" Prelude..=) Prelude.<$> mountOptions,
            ("mountPoint" Prelude..=) Prelude.<$> mountPoint,
            ("type" Prelude..=) Prelude.<$> type',
            ("location" Prelude..=) Prelude.<$> location
          ]
      )
