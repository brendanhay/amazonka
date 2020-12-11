-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
  ( ProjectFileSystemLocation (..),

    -- * Smart constructor
    mkProjectFileSystemLocation,

    -- * Lenses
    pfslLocation,
    pfslIdentifier,
    pfslMountOptions,
    pfslType,
    pfslMountPoint,
  )
where

import Network.AWS.CodeBuild.Types.FileSystemType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a file system created by Amazon Elastic File System (EFS). For more information, see <https://docs.aws.amazon.com/efs/latest/ug/whatisefs.html What Is Amazon Elastic File System?>
--
-- /See:/ 'mkProjectFileSystemLocation' smart constructor.
data ProjectFileSystemLocation = ProjectFileSystemLocation'
  { location ::
      Lude.Maybe Lude.Text,
    identifier :: Lude.Maybe Lude.Text,
    mountOptions :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe FileSystemType,
    mountPoint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectFileSystemLocation' with the minimum fields required to make a request.
--
-- * 'identifier' - The name used to access a file system created by Amazon EFS. CodeBuild creates an environment variable by appending the @identifier@ in all capital letters to @CODEBUILD_@ . For example, if you specify @my_efs@ for @identifier@ , a new environment variable is create named @CODEBUILD_MY_EFS@ .
--
-- The @identifier@ is used to mount your file system.
-- * 'location' - A string that specifies the location of the file system created by Amazon EFS. Its format is @efs-dns-name:/directory-path@ . You can find the DNS name of file system when you view it in the AWS EFS console. The directory path is a path to a directory in the file system that CodeBuild mounts. For example, if the DNS name of a file system is @fs-abcd1234.efs.us-west-2.amazonaws.com@ , and its mount directory is @my-efs-mount-directory@ , then the @location@ is @fs-abcd1234.efs.us-west-2.amazonaws.com:/my-efs-mount-directory@ .
--
-- The directory path in the format @efs-dns-name:/directory-path@ is optional. If you do not specify a directory path, the location is only the DNS name and CodeBuild mounts the entire file system.
-- * 'mountOptions' - The mount options for a file system created by AWS EFS. The default mount options used by CodeBuild are @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options> .
-- * 'mountPoint' - The location in the container where you mount the file system.
-- * 'type'' - The type of the file system. The one supported type is @EFS@ .
mkProjectFileSystemLocation ::
  ProjectFileSystemLocation
mkProjectFileSystemLocation =
  ProjectFileSystemLocation'
    { location = Lude.Nothing,
      identifier = Lude.Nothing,
      mountOptions = Lude.Nothing,
      type' = Lude.Nothing,
      mountPoint = Lude.Nothing
    }

-- | A string that specifies the location of the file system created by Amazon EFS. Its format is @efs-dns-name:/directory-path@ . You can find the DNS name of file system when you view it in the AWS EFS console. The directory path is a path to a directory in the file system that CodeBuild mounts. For example, if the DNS name of a file system is @fs-abcd1234.efs.us-west-2.amazonaws.com@ , and its mount directory is @my-efs-mount-directory@ , then the @location@ is @fs-abcd1234.efs.us-west-2.amazonaws.com:/my-efs-mount-directory@ .
--
-- The directory path in the format @efs-dns-name:/directory-path@ is optional. If you do not specify a directory path, the location is only the DNS name and CodeBuild mounts the entire file system.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslLocation :: Lens.Lens' ProjectFileSystemLocation (Lude.Maybe Lude.Text)
pfslLocation = Lens.lens (location :: ProjectFileSystemLocation -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: ProjectFileSystemLocation)
{-# DEPRECATED pfslLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name used to access a file system created by Amazon EFS. CodeBuild creates an environment variable by appending the @identifier@ in all capital letters to @CODEBUILD_@ . For example, if you specify @my_efs@ for @identifier@ , a new environment variable is create named @CODEBUILD_MY_EFS@ .
--
-- The @identifier@ is used to mount your file system.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslIdentifier :: Lens.Lens' ProjectFileSystemLocation (Lude.Maybe Lude.Text)
pfslIdentifier = Lens.lens (identifier :: ProjectFileSystemLocation -> Lude.Maybe Lude.Text) (\s a -> s {identifier = a} :: ProjectFileSystemLocation)
{-# DEPRECATED pfslIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | The mount options for a file system created by AWS EFS. The default mount options used by CodeBuild are @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options> .
--
-- /Note:/ Consider using 'mountOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslMountOptions :: Lens.Lens' ProjectFileSystemLocation (Lude.Maybe Lude.Text)
pfslMountOptions = Lens.lens (mountOptions :: ProjectFileSystemLocation -> Lude.Maybe Lude.Text) (\s a -> s {mountOptions = a} :: ProjectFileSystemLocation)
{-# DEPRECATED pfslMountOptions "Use generic-lens or generic-optics with 'mountOptions' instead." #-}

-- | The type of the file system. The one supported type is @EFS@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslType :: Lens.Lens' ProjectFileSystemLocation (Lude.Maybe FileSystemType)
pfslType = Lens.lens (type' :: ProjectFileSystemLocation -> Lude.Maybe FileSystemType) (\s a -> s {type' = a} :: ProjectFileSystemLocation)
{-# DEPRECATED pfslType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The location in the container where you mount the file system.
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslMountPoint :: Lens.Lens' ProjectFileSystemLocation (Lude.Maybe Lude.Text)
pfslMountPoint = Lens.lens (mountPoint :: ProjectFileSystemLocation -> Lude.Maybe Lude.Text) (\s a -> s {mountPoint = a} :: ProjectFileSystemLocation)
{-# DEPRECATED pfslMountPoint "Use generic-lens or generic-optics with 'mountPoint' instead." #-}

instance Lude.FromJSON ProjectFileSystemLocation where
  parseJSON =
    Lude.withObject
      "ProjectFileSystemLocation"
      ( \x ->
          ProjectFileSystemLocation'
            Lude.<$> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "identifier")
            Lude.<*> (x Lude..:? "mountOptions")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "mountPoint")
      )

instance Lude.ToJSON ProjectFileSystemLocation where
  toJSON ProjectFileSystemLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("location" Lude..=) Lude.<$> location,
            ("identifier" Lude..=) Lude.<$> identifier,
            ("mountOptions" Lude..=) Lude.<$> mountOptions,
            ("type" Lude..=) Lude.<$> type',
            ("mountPoint" Lude..=) Lude.<$> mountPoint
          ]
      )
