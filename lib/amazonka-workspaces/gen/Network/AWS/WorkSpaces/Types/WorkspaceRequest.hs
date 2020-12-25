{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceRequest
  ( WorkspaceRequest (..),

    -- * Smart constructor
    mkWorkspaceRequest,

    -- * Lenses
    wrDirectoryId,
    wrUserName,
    wrBundleId,
    wrRootVolumeEncryptionEnabled,
    wrTags,
    wrUserVolumeEncryptionEnabled,
    wrVolumeEncryptionKey,
    wrWorkspaceProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.BundleId as Types
import qualified Network.AWS.WorkSpaces.Types.DirectoryId as Types
import qualified Network.AWS.WorkSpaces.Types.Tag as Types
import qualified Network.AWS.WorkSpaces.Types.UserName as Types
import qualified Network.AWS.WorkSpaces.Types.VolumeEncryptionKey as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceProperties as Types

-- | Describes the information used to create a WorkSpace.
--
-- /See:/ 'mkWorkspaceRequest' smart constructor.
data WorkspaceRequest = WorkspaceRequest'
  { -- | The identifier of the AWS Directory Service directory for the WorkSpace. You can use 'DescribeWorkspaceDirectories' to list the available directories.
    directoryId :: Types.DirectoryId,
    -- | The user name of the user for the WorkSpace. This user name must exist in the AWS Directory Service directory for the WorkSpace.
    userName :: Types.UserName,
    -- | The identifier of the bundle for the WorkSpace. You can use 'DescribeWorkspaceBundles' to list the available bundles.
    bundleId :: Types.BundleId,
    -- | Indicates whether the data stored on the root volume is encrypted.
    rootVolumeEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | The tags for the WorkSpace.
    tags :: Core.Maybe [Types.Tag],
    -- | Indicates whether the data stored on the user volume is encrypted.
    userVolumeEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
    volumeEncryptionKey :: Core.Maybe Types.VolumeEncryptionKey,
    -- | The WorkSpace properties.
    workspaceProperties :: Core.Maybe Types.WorkspaceProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkspaceRequest' value with any optional fields omitted.
mkWorkspaceRequest ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'userName'
  Types.UserName ->
  -- | 'bundleId'
  Types.BundleId ->
  WorkspaceRequest
mkWorkspaceRequest directoryId userName bundleId =
  WorkspaceRequest'
    { directoryId,
      userName,
      bundleId,
      rootVolumeEncryptionEnabled = Core.Nothing,
      tags = Core.Nothing,
      userVolumeEncryptionEnabled = Core.Nothing,
      volumeEncryptionKey = Core.Nothing,
      workspaceProperties = Core.Nothing
    }

-- | The identifier of the AWS Directory Service directory for the WorkSpace. You can use 'DescribeWorkspaceDirectories' to list the available directories.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrDirectoryId :: Lens.Lens' WorkspaceRequest Types.DirectoryId
wrDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED wrDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The user name of the user for the WorkSpace. This user name must exist in the AWS Directory Service directory for the WorkSpace.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrUserName :: Lens.Lens' WorkspaceRequest Types.UserName
wrUserName = Lens.field @"userName"
{-# DEPRECATED wrUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The identifier of the bundle for the WorkSpace. You can use 'DescribeWorkspaceBundles' to list the available bundles.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrBundleId :: Lens.Lens' WorkspaceRequest Types.BundleId
wrBundleId = Lens.field @"bundleId"
{-# DEPRECATED wrBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | Indicates whether the data stored on the root volume is encrypted.
--
-- /Note:/ Consider using 'rootVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrRootVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Core.Maybe Core.Bool)
wrRootVolumeEncryptionEnabled = Lens.field @"rootVolumeEncryptionEnabled"
{-# DEPRECATED wrRootVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'rootVolumeEncryptionEnabled' instead." #-}

-- | The tags for the WorkSpace.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrTags :: Lens.Lens' WorkspaceRequest (Core.Maybe [Types.Tag])
wrTags = Lens.field @"tags"
{-# DEPRECATED wrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether the data stored on the user volume is encrypted.
--
-- /Note:/ Consider using 'userVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrUserVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Core.Maybe Core.Bool)
wrUserVolumeEncryptionEnabled = Lens.field @"userVolumeEncryptionEnabled"
{-# DEPRECATED wrUserVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'userVolumeEncryptionEnabled' instead." #-}

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'volumeEncryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrVolumeEncryptionKey :: Lens.Lens' WorkspaceRequest (Core.Maybe Types.VolumeEncryptionKey)
wrVolumeEncryptionKey = Lens.field @"volumeEncryptionKey"
{-# DEPRECATED wrVolumeEncryptionKey "Use generic-lens or generic-optics with 'volumeEncryptionKey' instead." #-}

-- | The WorkSpace properties.
--
-- /Note:/ Consider using 'workspaceProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrWorkspaceProperties :: Lens.Lens' WorkspaceRequest (Core.Maybe Types.WorkspaceProperties)
wrWorkspaceProperties = Lens.field @"workspaceProperties"
{-# DEPRECATED wrWorkspaceProperties "Use generic-lens or generic-optics with 'workspaceProperties' instead." #-}

instance Core.FromJSON WorkspaceRequest where
  toJSON WorkspaceRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("UserName" Core..= userName),
            Core.Just ("BundleId" Core..= bundleId),
            ("RootVolumeEncryptionEnabled" Core..=)
              Core.<$> rootVolumeEncryptionEnabled,
            ("Tags" Core..=) Core.<$> tags,
            ("UserVolumeEncryptionEnabled" Core..=)
              Core.<$> userVolumeEncryptionEnabled,
            ("VolumeEncryptionKey" Core..=) Core.<$> volumeEncryptionKey,
            ("WorkspaceProperties" Core..=) Core.<$> workspaceProperties
          ]
      )

instance Core.FromJSON WorkspaceRequest where
  parseJSON =
    Core.withObject "WorkspaceRequest" Core.$
      \x ->
        WorkspaceRequest'
          Core.<$> (x Core..: "DirectoryId")
          Core.<*> (x Core..: "UserName")
          Core.<*> (x Core..: "BundleId")
          Core.<*> (x Core..:? "RootVolumeEncryptionEnabled")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "UserVolumeEncryptionEnabled")
          Core.<*> (x Core..:? "VolumeEncryptionKey")
          Core.<*> (x Core..:? "WorkspaceProperties")
