{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.AccessPointDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.AccessPointDescription
  ( AccessPointDescription (..)
  -- * Smart constructor
  , mkAccessPointDescription
  -- * Lenses
  , apdAccessPointArn
  , apdAccessPointId
  , apdClientToken
  , apdFileSystemId
  , apdLifeCycleState
  , apdName
  , apdOwnerId
  , apdPosixUser
  , apdRootDirectory
  , apdTags
  ) where

import qualified Network.AWS.EFS.Types.AccessPointArn as Types
import qualified Network.AWS.EFS.Types.AccessPointId as Types
import qualified Network.AWS.EFS.Types.ClientToken as Types
import qualified Network.AWS.EFS.Types.FileSystemId as Types
import qualified Network.AWS.EFS.Types.LifeCycleState as Types
import qualified Network.AWS.EFS.Types.Name as Types
import qualified Network.AWS.EFS.Types.OwnerId as Types
import qualified Network.AWS.EFS.Types.PosixUser as Types
import qualified Network.AWS.EFS.Types.RootDirectory as Types
import qualified Network.AWS.EFS.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a description of an EFS file system access point.
--
-- /See:/ 'mkAccessPointDescription' smart constructor.
data AccessPointDescription = AccessPointDescription'
  { accessPointArn :: Core.Maybe Types.AccessPointArn
    -- ^ The unique Amazon Resource Name (ARN) associated with the access point.
  , accessPointId :: Core.Maybe Types.AccessPointId
    -- ^ The ID of the access point, assigned by Amazon EFS.
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ The opaque string specified in the request to ensure idempotent creation.
  , fileSystemId :: Core.Maybe Types.FileSystemId
    -- ^ The ID of the EFS file system that the access point applies to.
  , lifeCycleState :: Core.Maybe Types.LifeCycleState
    -- ^ Identifies the lifecycle phase of the access point.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the access point. This is the value of the @Name@ tag.
  , ownerId :: Core.Maybe Types.OwnerId
    -- ^ Identified the AWS account that owns the access point resource.
  , posixUser :: Core.Maybe Types.PosixUser
    -- ^ The full POSIX identity, including the user ID, group ID, and secondary group IDs on the access point that is used for all file operations by NFS clients using the access point.
  , rootDirectory :: Core.Maybe Types.RootDirectory
    -- ^ The directory on the Amazon EFS file system that the access point exposes as the root directory to NFS clients using the access point.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the access point, presented as an array of Tag objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessPointDescription' value with any optional fields omitted.
mkAccessPointDescription
    :: AccessPointDescription
mkAccessPointDescription
  = AccessPointDescription'{accessPointArn = Core.Nothing,
                            accessPointId = Core.Nothing, clientToken = Core.Nothing,
                            fileSystemId = Core.Nothing, lifeCycleState = Core.Nothing,
                            name = Core.Nothing, ownerId = Core.Nothing,
                            posixUser = Core.Nothing, rootDirectory = Core.Nothing,
                            tags = Core.Nothing}

-- | The unique Amazon Resource Name (ARN) associated with the access point.
--
-- /Note:/ Consider using 'accessPointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdAccessPointArn :: Lens.Lens' AccessPointDescription (Core.Maybe Types.AccessPointArn)
apdAccessPointArn = Lens.field @"accessPointArn"
{-# INLINEABLE apdAccessPointArn #-}
{-# DEPRECATED accessPointArn "Use generic-lens or generic-optics with 'accessPointArn' instead"  #-}

-- | The ID of the access point, assigned by Amazon EFS.
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdAccessPointId :: Lens.Lens' AccessPointDescription (Core.Maybe Types.AccessPointId)
apdAccessPointId = Lens.field @"accessPointId"
{-# INLINEABLE apdAccessPointId #-}
{-# DEPRECATED accessPointId "Use generic-lens or generic-optics with 'accessPointId' instead"  #-}

-- | The opaque string specified in the request to ensure idempotent creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdClientToken :: Lens.Lens' AccessPointDescription (Core.Maybe Types.ClientToken)
apdClientToken = Lens.field @"clientToken"
{-# INLINEABLE apdClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The ID of the EFS file system that the access point applies to.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdFileSystemId :: Lens.Lens' AccessPointDescription (Core.Maybe Types.FileSystemId)
apdFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE apdFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | Identifies the lifecycle phase of the access point.
--
-- /Note:/ Consider using 'lifeCycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdLifeCycleState :: Lens.Lens' AccessPointDescription (Core.Maybe Types.LifeCycleState)
apdLifeCycleState = Lens.field @"lifeCycleState"
{-# INLINEABLE apdLifeCycleState #-}
{-# DEPRECATED lifeCycleState "Use generic-lens or generic-optics with 'lifeCycleState' instead"  #-}

-- | The name of the access point. This is the value of the @Name@ tag.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdName :: Lens.Lens' AccessPointDescription (Core.Maybe Types.Name)
apdName = Lens.field @"name"
{-# INLINEABLE apdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Identified the AWS account that owns the access point resource.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdOwnerId :: Lens.Lens' AccessPointDescription (Core.Maybe Types.OwnerId)
apdOwnerId = Lens.field @"ownerId"
{-# INLINEABLE apdOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The full POSIX identity, including the user ID, group ID, and secondary group IDs on the access point that is used for all file operations by NFS clients using the access point.
--
-- /Note:/ Consider using 'posixUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdPosixUser :: Lens.Lens' AccessPointDescription (Core.Maybe Types.PosixUser)
apdPosixUser = Lens.field @"posixUser"
{-# INLINEABLE apdPosixUser #-}
{-# DEPRECATED posixUser "Use generic-lens or generic-optics with 'posixUser' instead"  #-}

-- | The directory on the Amazon EFS file system that the access point exposes as the root directory to NFS clients using the access point.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdRootDirectory :: Lens.Lens' AccessPointDescription (Core.Maybe Types.RootDirectory)
apdRootDirectory = Lens.field @"rootDirectory"
{-# INLINEABLE apdRootDirectory #-}
{-# DEPRECATED rootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead"  #-}

-- | The tags associated with the access point, presented as an array of Tag objects.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdTags :: Lens.Lens' AccessPointDescription (Core.Maybe [Types.Tag])
apdTags = Lens.field @"tags"
{-# INLINEABLE apdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON AccessPointDescription where
        parseJSON
          = Core.withObject "AccessPointDescription" Core.$
              \ x ->
                AccessPointDescription' Core.<$>
                  (x Core..:? "AccessPointArn") Core.<*> x Core..:? "AccessPointId"
                    Core.<*> x Core..:? "ClientToken"
                    Core.<*> x Core..:? "FileSystemId"
                    Core.<*> x Core..:? "LifeCycleState"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "OwnerId"
                    Core.<*> x Core..:? "PosixUser"
                    Core.<*> x Core..:? "RootDirectory"
                    Core.<*> x Core..:? "Tags"
