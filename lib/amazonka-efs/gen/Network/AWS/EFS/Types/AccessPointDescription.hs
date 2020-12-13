{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.AccessPointDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.AccessPointDescription
  ( AccessPointDescription (..),

    -- * Smart constructor
    mkAccessPointDescription,

    -- * Lenses
    apdPosixUser,
    apdRootDirectory,
    apdClientToken,
    apdAccessPointId,
    apdFileSystemId,
    apdOwnerId,
    apdName,
    apdAccessPointARN,
    apdLifeCycleState,
    apdTags,
  )
where

import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.EFS.Types.PosixUser
import Network.AWS.EFS.Types.RootDirectory
import Network.AWS.EFS.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a description of an EFS file system access point.
--
-- /See:/ 'mkAccessPointDescription' smart constructor.
data AccessPointDescription = AccessPointDescription'
  { -- | The full POSIX identity, including the user ID, group ID, and secondary group IDs on the access point that is used for all file operations by NFS clients using the access point.
    posixUser :: Lude.Maybe PosixUser,
    -- | The directory on the Amazon EFS file system that the access point exposes as the root directory to NFS clients using the access point.
    rootDirectory :: Lude.Maybe RootDirectory,
    -- | The opaque string specified in the request to ensure idempotent creation.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The ID of the access point, assigned by Amazon EFS.
    accessPointId :: Lude.Maybe Lude.Text,
    -- | The ID of the EFS file system that the access point applies to.
    fileSystemId :: Lude.Maybe Lude.Text,
    -- | Identified the AWS account that owns the access point resource.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The name of the access point. This is the value of the @Name@ tag.
    name :: Lude.Maybe Lude.Text,
    -- | The unique Amazon Resource Name (ARN) associated with the access point.
    accessPointARN :: Lude.Maybe Lude.Text,
    -- | Identifies the lifecycle phase of the access point.
    lifeCycleState :: Lude.Maybe LifeCycleState,
    -- | The tags associated with the access point, presented as an array of Tag objects.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessPointDescription' with the minimum fields required to make a request.
--
-- * 'posixUser' - The full POSIX identity, including the user ID, group ID, and secondary group IDs on the access point that is used for all file operations by NFS clients using the access point.
-- * 'rootDirectory' - The directory on the Amazon EFS file system that the access point exposes as the root directory to NFS clients using the access point.
-- * 'clientToken' - The opaque string specified in the request to ensure idempotent creation.
-- * 'accessPointId' - The ID of the access point, assigned by Amazon EFS.
-- * 'fileSystemId' - The ID of the EFS file system that the access point applies to.
-- * 'ownerId' - Identified the AWS account that owns the access point resource.
-- * 'name' - The name of the access point. This is the value of the @Name@ tag.
-- * 'accessPointARN' - The unique Amazon Resource Name (ARN) associated with the access point.
-- * 'lifeCycleState' - Identifies the lifecycle phase of the access point.
-- * 'tags' - The tags associated with the access point, presented as an array of Tag objects.
mkAccessPointDescription ::
  AccessPointDescription
mkAccessPointDescription =
  AccessPointDescription'
    { posixUser = Lude.Nothing,
      rootDirectory = Lude.Nothing,
      clientToken = Lude.Nothing,
      accessPointId = Lude.Nothing,
      fileSystemId = Lude.Nothing,
      ownerId = Lude.Nothing,
      name = Lude.Nothing,
      accessPointARN = Lude.Nothing,
      lifeCycleState = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The full POSIX identity, including the user ID, group ID, and secondary group IDs on the access point that is used for all file operations by NFS clients using the access point.
--
-- /Note:/ Consider using 'posixUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdPosixUser :: Lens.Lens' AccessPointDescription (Lude.Maybe PosixUser)
apdPosixUser = Lens.lens (posixUser :: AccessPointDescription -> Lude.Maybe PosixUser) (\s a -> s {posixUser = a} :: AccessPointDescription)
{-# DEPRECATED apdPosixUser "Use generic-lens or generic-optics with 'posixUser' instead." #-}

-- | The directory on the Amazon EFS file system that the access point exposes as the root directory to NFS clients using the access point.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdRootDirectory :: Lens.Lens' AccessPointDescription (Lude.Maybe RootDirectory)
apdRootDirectory = Lens.lens (rootDirectory :: AccessPointDescription -> Lude.Maybe RootDirectory) (\s a -> s {rootDirectory = a} :: AccessPointDescription)
{-# DEPRECATED apdRootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead." #-}

-- | The opaque string specified in the request to ensure idempotent creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdClientToken :: Lens.Lens' AccessPointDescription (Lude.Maybe Lude.Text)
apdClientToken = Lens.lens (clientToken :: AccessPointDescription -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: AccessPointDescription)
{-# DEPRECATED apdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the access point, assigned by Amazon EFS.
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdAccessPointId :: Lens.Lens' AccessPointDescription (Lude.Maybe Lude.Text)
apdAccessPointId = Lens.lens (accessPointId :: AccessPointDescription -> Lude.Maybe Lude.Text) (\s a -> s {accessPointId = a} :: AccessPointDescription)
{-# DEPRECATED apdAccessPointId "Use generic-lens or generic-optics with 'accessPointId' instead." #-}

-- | The ID of the EFS file system that the access point applies to.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdFileSystemId :: Lens.Lens' AccessPointDescription (Lude.Maybe Lude.Text)
apdFileSystemId = Lens.lens (fileSystemId :: AccessPointDescription -> Lude.Maybe Lude.Text) (\s a -> s {fileSystemId = a} :: AccessPointDescription)
{-# DEPRECATED apdFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | Identified the AWS account that owns the access point resource.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdOwnerId :: Lens.Lens' AccessPointDescription (Lude.Maybe Lude.Text)
apdOwnerId = Lens.lens (ownerId :: AccessPointDescription -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: AccessPointDescription)
{-# DEPRECATED apdOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The name of the access point. This is the value of the @Name@ tag.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdName :: Lens.Lens' AccessPointDescription (Lude.Maybe Lude.Text)
apdName = Lens.lens (name :: AccessPointDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AccessPointDescription)
{-# DEPRECATED apdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique Amazon Resource Name (ARN) associated with the access point.
--
-- /Note:/ Consider using 'accessPointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdAccessPointARN :: Lens.Lens' AccessPointDescription (Lude.Maybe Lude.Text)
apdAccessPointARN = Lens.lens (accessPointARN :: AccessPointDescription -> Lude.Maybe Lude.Text) (\s a -> s {accessPointARN = a} :: AccessPointDescription)
{-# DEPRECATED apdAccessPointARN "Use generic-lens or generic-optics with 'accessPointARN' instead." #-}

-- | Identifies the lifecycle phase of the access point.
--
-- /Note:/ Consider using 'lifeCycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdLifeCycleState :: Lens.Lens' AccessPointDescription (Lude.Maybe LifeCycleState)
apdLifeCycleState = Lens.lens (lifeCycleState :: AccessPointDescription -> Lude.Maybe LifeCycleState) (\s a -> s {lifeCycleState = a} :: AccessPointDescription)
{-# DEPRECATED apdLifeCycleState "Use generic-lens or generic-optics with 'lifeCycleState' instead." #-}

-- | The tags associated with the access point, presented as an array of Tag objects.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdTags :: Lens.Lens' AccessPointDescription (Lude.Maybe [Tag])
apdTags = Lens.lens (tags :: AccessPointDescription -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: AccessPointDescription)
{-# DEPRECATED apdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON AccessPointDescription where
  parseJSON =
    Lude.withObject
      "AccessPointDescription"
      ( \x ->
          AccessPointDescription'
            Lude.<$> (x Lude..:? "PosixUser")
            Lude.<*> (x Lude..:? "RootDirectory")
            Lude.<*> (x Lude..:? "ClientToken")
            Lude.<*> (x Lude..:? "AccessPointId")
            Lude.<*> (x Lude..:? "FileSystemId")
            Lude.<*> (x Lude..:? "OwnerId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "AccessPointArn")
            Lude.<*> (x Lude..:? "LifeCycleState")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
