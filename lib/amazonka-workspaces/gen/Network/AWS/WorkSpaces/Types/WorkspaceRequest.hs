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
    wrWorkspaceProperties,
    wrRootVolumeEncryptionEnabled,
    wrVolumeEncryptionKey,
    wrUserVolumeEncryptionEnabled,
    wrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.WorkspaceProperties

-- | Describes the information used to create a WorkSpace.
--
-- /See:/ 'mkWorkspaceRequest' smart constructor.
data WorkspaceRequest = WorkspaceRequest'
  { -- | The identifier of the AWS Directory Service directory for the WorkSpace. You can use 'DescribeWorkspaceDirectories' to list the available directories.
    directoryId :: Lude.Text,
    -- | The user name of the user for the WorkSpace. This user name must exist in the AWS Directory Service directory for the WorkSpace.
    userName :: Lude.Text,
    -- | The identifier of the bundle for the WorkSpace. You can use 'DescribeWorkspaceBundles' to list the available bundles.
    bundleId :: Lude.Text,
    -- | The WorkSpace properties.
    workspaceProperties :: Lude.Maybe WorkspaceProperties,
    -- | Indicates whether the data stored on the root volume is encrypted.
    rootVolumeEncryptionEnabled :: Lude.Maybe Lude.Bool,
    -- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
    volumeEncryptionKey :: Lude.Maybe Lude.Text,
    -- | Indicates whether the data stored on the user volume is encrypted.
    userVolumeEncryptionEnabled :: Lude.Maybe Lude.Bool,
    -- | The tags for the WorkSpace.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceRequest' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the AWS Directory Service directory for the WorkSpace. You can use 'DescribeWorkspaceDirectories' to list the available directories.
-- * 'userName' - The user name of the user for the WorkSpace. This user name must exist in the AWS Directory Service directory for the WorkSpace.
-- * 'bundleId' - The identifier of the bundle for the WorkSpace. You can use 'DescribeWorkspaceBundles' to list the available bundles.
-- * 'workspaceProperties' - The WorkSpace properties.
-- * 'rootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
-- * 'volumeEncryptionKey' - The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
-- * 'userVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
-- * 'tags' - The tags for the WorkSpace.
mkWorkspaceRequest ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'bundleId'
  Lude.Text ->
  WorkspaceRequest
mkWorkspaceRequest pDirectoryId_ pUserName_ pBundleId_ =
  WorkspaceRequest'
    { directoryId = pDirectoryId_,
      userName = pUserName_,
      bundleId = pBundleId_,
      workspaceProperties = Lude.Nothing,
      rootVolumeEncryptionEnabled = Lude.Nothing,
      volumeEncryptionKey = Lude.Nothing,
      userVolumeEncryptionEnabled = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The identifier of the AWS Directory Service directory for the WorkSpace. You can use 'DescribeWorkspaceDirectories' to list the available directories.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrDirectoryId :: Lens.Lens' WorkspaceRequest Lude.Text
wrDirectoryId = Lens.lens (directoryId :: WorkspaceRequest -> Lude.Text) (\s a -> s {directoryId = a} :: WorkspaceRequest)
{-# DEPRECATED wrDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The user name of the user for the WorkSpace. This user name must exist in the AWS Directory Service directory for the WorkSpace.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrUserName :: Lens.Lens' WorkspaceRequest Lude.Text
wrUserName = Lens.lens (userName :: WorkspaceRequest -> Lude.Text) (\s a -> s {userName = a} :: WorkspaceRequest)
{-# DEPRECATED wrUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The identifier of the bundle for the WorkSpace. You can use 'DescribeWorkspaceBundles' to list the available bundles.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrBundleId :: Lens.Lens' WorkspaceRequest Lude.Text
wrBundleId = Lens.lens (bundleId :: WorkspaceRequest -> Lude.Text) (\s a -> s {bundleId = a} :: WorkspaceRequest)
{-# DEPRECATED wrBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The WorkSpace properties.
--
-- /Note:/ Consider using 'workspaceProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrWorkspaceProperties :: Lens.Lens' WorkspaceRequest (Lude.Maybe WorkspaceProperties)
wrWorkspaceProperties = Lens.lens (workspaceProperties :: WorkspaceRequest -> Lude.Maybe WorkspaceProperties) (\s a -> s {workspaceProperties = a} :: WorkspaceRequest)
{-# DEPRECATED wrWorkspaceProperties "Use generic-lens or generic-optics with 'workspaceProperties' instead." #-}

-- | Indicates whether the data stored on the root volume is encrypted.
--
-- /Note:/ Consider using 'rootVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrRootVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Lude.Maybe Lude.Bool)
wrRootVolumeEncryptionEnabled = Lens.lens (rootVolumeEncryptionEnabled :: WorkspaceRequest -> Lude.Maybe Lude.Bool) (\s a -> s {rootVolumeEncryptionEnabled = a} :: WorkspaceRequest)
{-# DEPRECATED wrRootVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'rootVolumeEncryptionEnabled' instead." #-}

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'volumeEncryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrVolumeEncryptionKey :: Lens.Lens' WorkspaceRequest (Lude.Maybe Lude.Text)
wrVolumeEncryptionKey = Lens.lens (volumeEncryptionKey :: WorkspaceRequest -> Lude.Maybe Lude.Text) (\s a -> s {volumeEncryptionKey = a} :: WorkspaceRequest)
{-# DEPRECATED wrVolumeEncryptionKey "Use generic-lens or generic-optics with 'volumeEncryptionKey' instead." #-}

-- | Indicates whether the data stored on the user volume is encrypted.
--
-- /Note:/ Consider using 'userVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrUserVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Lude.Maybe Lude.Bool)
wrUserVolumeEncryptionEnabled = Lens.lens (userVolumeEncryptionEnabled :: WorkspaceRequest -> Lude.Maybe Lude.Bool) (\s a -> s {userVolumeEncryptionEnabled = a} :: WorkspaceRequest)
{-# DEPRECATED wrUserVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'userVolumeEncryptionEnabled' instead." #-}

-- | The tags for the WorkSpace.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrTags :: Lens.Lens' WorkspaceRequest (Lude.Maybe [Tag])
wrTags = Lens.lens (tags :: WorkspaceRequest -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: WorkspaceRequest)
{-# DEPRECATED wrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON WorkspaceRequest where
  parseJSON =
    Lude.withObject
      "WorkspaceRequest"
      ( \x ->
          WorkspaceRequest'
            Lude.<$> (x Lude..: "DirectoryId")
            Lude.<*> (x Lude..: "UserName")
            Lude.<*> (x Lude..: "BundleId")
            Lude.<*> (x Lude..:? "WorkspaceProperties")
            Lude.<*> (x Lude..:? "RootVolumeEncryptionEnabled")
            Lude.<*> (x Lude..:? "VolumeEncryptionKey")
            Lude.<*> (x Lude..:? "UserVolumeEncryptionEnabled")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON WorkspaceRequest where
  toJSON WorkspaceRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("UserName" Lude..= userName),
            Lude.Just ("BundleId" Lude..= bundleId),
            ("WorkspaceProperties" Lude..=) Lude.<$> workspaceProperties,
            ("RootVolumeEncryptionEnabled" Lude..=)
              Lude.<$> rootVolumeEncryptionEnabled,
            ("VolumeEncryptionKey" Lude..=) Lude.<$> volumeEncryptionKey,
            ("UserVolumeEncryptionEnabled" Lude..=)
              Lude.<$> userVolumeEncryptionEnabled,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )
