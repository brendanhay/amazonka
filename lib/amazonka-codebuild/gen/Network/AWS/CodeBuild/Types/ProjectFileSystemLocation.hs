{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
  ( ProjectFileSystemLocation (..)
  -- * Smart constructor
  , mkProjectFileSystemLocation
  -- * Lenses
  , pfslIdentifier
  , pfslLocation
  , pfslMountOptions
  , pfslMountPoint
  , pfslType
  ) where

import qualified Network.AWS.CodeBuild.Types.FileSystemType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a file system created by Amazon Elastic File System (EFS). For more information, see <https://docs.aws.amazon.com/efs/latest/ug/whatisefs.html What Is Amazon Elastic File System?> 
--
-- /See:/ 'mkProjectFileSystemLocation' smart constructor.
data ProjectFileSystemLocation = ProjectFileSystemLocation'
  { identifier :: Core.Maybe Core.Text
    -- ^ The name used to access a file system created by Amazon EFS. CodeBuild creates an environment variable by appending the @identifier@ in all capital letters to @CODEBUILD_@ . For example, if you specify @my_efs@ for @identifier@ , a new environment variable is create named @CODEBUILD_MY_EFS@ . 
--
-- The @identifier@ is used to mount your file system. 
  , location :: Core.Maybe Core.Text
    -- ^ A string that specifies the location of the file system created by Amazon EFS. Its format is @efs-dns-name:/directory-path@ . You can find the DNS name of file system when you view it in the AWS EFS console. The directory path is a path to a directory in the file system that CodeBuild mounts. For example, if the DNS name of a file system is @fs-abcd1234.efs.us-west-2.amazonaws.com@ , and its mount directory is @my-efs-mount-directory@ , then the @location@ is @fs-abcd1234.efs.us-west-2.amazonaws.com:/my-efs-mount-directory@ . 
--
-- The directory path in the format @efs-dns-name:/directory-path@ is optional. If you do not specify a directory path, the location is only the DNS name and CodeBuild mounts the entire file system. 
  , mountOptions :: Core.Maybe Core.Text
    -- ^ The mount options for a file system created by AWS EFS. The default mount options used by CodeBuild are @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options> . 
  , mountPoint :: Core.Maybe Core.Text
    -- ^ The location in the container where you mount the file system. 
  , type' :: Core.Maybe Types.FileSystemType
    -- ^ The type of the file system. The one supported type is @EFS@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectFileSystemLocation' value with any optional fields omitted.
mkProjectFileSystemLocation
    :: ProjectFileSystemLocation
mkProjectFileSystemLocation
  = ProjectFileSystemLocation'{identifier = Core.Nothing,
                               location = Core.Nothing, mountOptions = Core.Nothing,
                               mountPoint = Core.Nothing, type' = Core.Nothing}

-- | The name used to access a file system created by Amazon EFS. CodeBuild creates an environment variable by appending the @identifier@ in all capital letters to @CODEBUILD_@ . For example, if you specify @my_efs@ for @identifier@ , a new environment variable is create named @CODEBUILD_MY_EFS@ . 
--
-- The @identifier@ is used to mount your file system. 
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslIdentifier :: Lens.Lens' ProjectFileSystemLocation (Core.Maybe Core.Text)
pfslIdentifier = Lens.field @"identifier"
{-# INLINEABLE pfslIdentifier #-}
{-# DEPRECATED identifier "Use generic-lens or generic-optics with 'identifier' instead"  #-}

-- | A string that specifies the location of the file system created by Amazon EFS. Its format is @efs-dns-name:/directory-path@ . You can find the DNS name of file system when you view it in the AWS EFS console. The directory path is a path to a directory in the file system that CodeBuild mounts. For example, if the DNS name of a file system is @fs-abcd1234.efs.us-west-2.amazonaws.com@ , and its mount directory is @my-efs-mount-directory@ , then the @location@ is @fs-abcd1234.efs.us-west-2.amazonaws.com:/my-efs-mount-directory@ . 
--
-- The directory path in the format @efs-dns-name:/directory-path@ is optional. If you do not specify a directory path, the location is only the DNS name and CodeBuild mounts the entire file system. 
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslLocation :: Lens.Lens' ProjectFileSystemLocation (Core.Maybe Core.Text)
pfslLocation = Lens.field @"location"
{-# INLINEABLE pfslLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The mount options for a file system created by AWS EFS. The default mount options used by CodeBuild are @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options> . 
--
-- /Note:/ Consider using 'mountOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslMountOptions :: Lens.Lens' ProjectFileSystemLocation (Core.Maybe Core.Text)
pfslMountOptions = Lens.field @"mountOptions"
{-# INLINEABLE pfslMountOptions #-}
{-# DEPRECATED mountOptions "Use generic-lens or generic-optics with 'mountOptions' instead"  #-}

-- | The location in the container where you mount the file system. 
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslMountPoint :: Lens.Lens' ProjectFileSystemLocation (Core.Maybe Core.Text)
pfslMountPoint = Lens.field @"mountPoint"
{-# INLINEABLE pfslMountPoint #-}
{-# DEPRECATED mountPoint "Use generic-lens or generic-optics with 'mountPoint' instead"  #-}

-- | The type of the file system. The one supported type is @EFS@ . 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfslType :: Lens.Lens' ProjectFileSystemLocation (Core.Maybe Types.FileSystemType)
pfslType = Lens.field @"type'"
{-# INLINEABLE pfslType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ProjectFileSystemLocation where
        toJSON ProjectFileSystemLocation{..}
          = Core.object
              (Core.catMaybes
                 [("identifier" Core..=) Core.<$> identifier,
                  ("location" Core..=) Core.<$> location,
                  ("mountOptions" Core..=) Core.<$> mountOptions,
                  ("mountPoint" Core..=) Core.<$> mountPoint,
                  ("type" Core..=) Core.<$> type'])

instance Core.FromJSON ProjectFileSystemLocation where
        parseJSON
          = Core.withObject "ProjectFileSystemLocation" Core.$
              \ x ->
                ProjectFileSystemLocation' Core.<$>
                  (x Core..:? "identifier") Core.<*> x Core..:? "location" Core.<*>
                    x Core..:? "mountOptions"
                    Core.<*> x Core..:? "mountPoint"
                    Core.<*> x Core..:? "type"
