-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Workspace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Workspace
  ( Workspace (..),

    -- * Smart constructor
    mkWorkspace,

    -- * Lenses
    wDirectoryId,
    wState,
    wIPAddress,
    wModificationStates,
    wUserName,
    wSubnetId,
    wBundleId,
    wWorkspaceProperties,
    wRootVolumeEncryptionEnabled,
    wErrorCode,
    wVolumeEncryptionKey,
    wComputerName,
    wWorkspaceId,
    wUserVolumeEncryptionEnabled,
    wErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceState

-- | Describes a WorkSpace.
--
-- /See:/ 'mkWorkspace' smart constructor.
data Workspace = Workspace'
  { directoryId :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe WorkspaceState,
    ipAddress :: Lude.Maybe Lude.Text,
    modificationStates :: Lude.Maybe [ModificationState],
    userName :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    bundleId :: Lude.Maybe Lude.Text,
    workspaceProperties :: Lude.Maybe WorkspaceProperties,
    rootVolumeEncryptionEnabled :: Lude.Maybe Lude.Bool,
    errorCode :: Lude.Maybe Lude.Text,
    volumeEncryptionKey :: Lude.Maybe Lude.Text,
    computerName :: Lude.Maybe Lude.Text,
    workspaceId :: Lude.Maybe Lude.Text,
    userVolumeEncryptionEnabled :: Lude.Maybe Lude.Bool,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Workspace' with the minimum fields required to make a request.
--
-- * 'bundleId' - The identifier of the bundle used to create the WorkSpace.
-- * 'computerName' - The name of the WorkSpace, as seen by the operating system. The format of this name varies. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace> .
-- * 'directoryId' - The identifier of the AWS Directory Service directory for the WorkSpace.
-- * 'errorCode' - The error code that is returned if the WorkSpace cannot be created.
-- * 'errorMessage' - The text of the error message that is returned if the WorkSpace cannot be created.
-- * 'ipAddress' - The IP address of the WorkSpace.
-- * 'modificationStates' - The modification states of the WorkSpace.
-- * 'rootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
-- * 'state' - The operational state of the WorkSpace.
-- * 'subnetId' - The identifier of the subnet for the WorkSpace.
-- * 'userName' - The user for the WorkSpace.
-- * 'userVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
-- * 'volumeEncryptionKey' - The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
-- * 'workspaceId' - The identifier of the WorkSpace.
-- * 'workspaceProperties' - The properties of the WorkSpace.
mkWorkspace ::
  Workspace
mkWorkspace =
  Workspace'
    { directoryId = Lude.Nothing,
      state = Lude.Nothing,
      ipAddress = Lude.Nothing,
      modificationStates = Lude.Nothing,
      userName = Lude.Nothing,
      subnetId = Lude.Nothing,
      bundleId = Lude.Nothing,
      workspaceProperties = Lude.Nothing,
      rootVolumeEncryptionEnabled = Lude.Nothing,
      errorCode = Lude.Nothing,
      volumeEncryptionKey = Lude.Nothing,
      computerName = Lude.Nothing,
      workspaceId = Lude.Nothing,
      userVolumeEncryptionEnabled = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The identifier of the AWS Directory Service directory for the WorkSpace.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wDirectoryId :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wDirectoryId = Lens.lens (directoryId :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: Workspace)
{-# DEPRECATED wDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The operational state of the WorkSpace.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wState :: Lens.Lens' Workspace (Lude.Maybe WorkspaceState)
wState = Lens.lens (state :: Workspace -> Lude.Maybe WorkspaceState) (\s a -> s {state = a} :: Workspace)
{-# DEPRECATED wState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The IP address of the WorkSpace.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wIPAddress :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wIPAddress = Lens.lens (ipAddress :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: Workspace)
{-# DEPRECATED wIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The modification states of the WorkSpace.
--
-- /Note:/ Consider using 'modificationStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wModificationStates :: Lens.Lens' Workspace (Lude.Maybe [ModificationState])
wModificationStates = Lens.lens (modificationStates :: Workspace -> Lude.Maybe [ModificationState]) (\s a -> s {modificationStates = a} :: Workspace)
{-# DEPRECATED wModificationStates "Use generic-lens or generic-optics with 'modificationStates' instead." #-}

-- | The user for the WorkSpace.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wUserName :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wUserName = Lens.lens (userName :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: Workspace)
{-# DEPRECATED wUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The identifier of the subnet for the WorkSpace.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSubnetId :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wSubnetId = Lens.lens (subnetId :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: Workspace)
{-# DEPRECATED wSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The identifier of the bundle used to create the WorkSpace.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wBundleId :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wBundleId = Lens.lens (bundleId :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: Workspace)
{-# DEPRECATED wBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The properties of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkspaceProperties :: Lens.Lens' Workspace (Lude.Maybe WorkspaceProperties)
wWorkspaceProperties = Lens.lens (workspaceProperties :: Workspace -> Lude.Maybe WorkspaceProperties) (\s a -> s {workspaceProperties = a} :: Workspace)
{-# DEPRECATED wWorkspaceProperties "Use generic-lens or generic-optics with 'workspaceProperties' instead." #-}

-- | Indicates whether the data stored on the root volume is encrypted.
--
-- /Note:/ Consider using 'rootVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wRootVolumeEncryptionEnabled :: Lens.Lens' Workspace (Lude.Maybe Lude.Bool)
wRootVolumeEncryptionEnabled = Lens.lens (rootVolumeEncryptionEnabled :: Workspace -> Lude.Maybe Lude.Bool) (\s a -> s {rootVolumeEncryptionEnabled = a} :: Workspace)
{-# DEPRECATED wRootVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'rootVolumeEncryptionEnabled' instead." #-}

-- | The error code that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wErrorCode :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wErrorCode = Lens.lens (errorCode :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: Workspace)
{-# DEPRECATED wErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'volumeEncryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wVolumeEncryptionKey :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wVolumeEncryptionKey = Lens.lens (volumeEncryptionKey :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {volumeEncryptionKey = a} :: Workspace)
{-# DEPRECATED wVolumeEncryptionKey "Use generic-lens or generic-optics with 'volumeEncryptionKey' instead." #-}

-- | The name of the WorkSpace, as seen by the operating system. The format of this name varies. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace> .
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wComputerName :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wComputerName = Lens.lens (computerName :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {computerName = a} :: Workspace)
{-# DEPRECATED wComputerName "Use generic-lens or generic-optics with 'computerName' instead." #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkspaceId :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wWorkspaceId = Lens.lens (workspaceId :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {workspaceId = a} :: Workspace)
{-# DEPRECATED wWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

-- | Indicates whether the data stored on the user volume is encrypted.
--
-- /Note:/ Consider using 'userVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wUserVolumeEncryptionEnabled :: Lens.Lens' Workspace (Lude.Maybe Lude.Bool)
wUserVolumeEncryptionEnabled = Lens.lens (userVolumeEncryptionEnabled :: Workspace -> Lude.Maybe Lude.Bool) (\s a -> s {userVolumeEncryptionEnabled = a} :: Workspace)
{-# DEPRECATED wUserVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'userVolumeEncryptionEnabled' instead." #-}

-- | The text of the error message that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wErrorMessage :: Lens.Lens' Workspace (Lude.Maybe Lude.Text)
wErrorMessage = Lens.lens (errorMessage :: Workspace -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: Workspace)
{-# DEPRECATED wErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON Workspace where
  parseJSON =
    Lude.withObject
      "Workspace"
      ( \x ->
          Workspace'
            Lude.<$> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "ModificationStates" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UserName")
            Lude.<*> (x Lude..:? "SubnetId")
            Lude.<*> (x Lude..:? "BundleId")
            Lude.<*> (x Lude..:? "WorkspaceProperties")
            Lude.<*> (x Lude..:? "RootVolumeEncryptionEnabled")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "VolumeEncryptionKey")
            Lude.<*> (x Lude..:? "ComputerName")
            Lude.<*> (x Lude..:? "WorkspaceId")
            Lude.<*> (x Lude..:? "UserVolumeEncryptionEnabled")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
