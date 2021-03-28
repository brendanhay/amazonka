{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Workspace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Workspace
  ( Workspace (..)
  -- * Smart constructor
  , mkWorkspace
  -- * Lenses
  , wBundleId
  , wComputerName
  , wDirectoryId
  , wErrorCode
  , wErrorMessage
  , wIpAddress
  , wModificationStates
  , wRootVolumeEncryptionEnabled
  , wState
  , wSubnetId
  , wUserName
  , wUserVolumeEncryptionEnabled
  , wVolumeEncryptionKey
  , wWorkspaceId
  , wWorkspaceProperties
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.BundleId as Types
import qualified Network.AWS.WorkSpaces.Types.ComputerName as Types
import qualified Network.AWS.WorkSpaces.Types.DirectoryId as Types
import qualified Network.AWS.WorkSpaces.Types.ErrorCode as Types
import qualified Network.AWS.WorkSpaces.Types.ErrorMessage as Types
import qualified Network.AWS.WorkSpaces.Types.IpAddress as Types
import qualified Network.AWS.WorkSpaces.Types.ModificationState as Types
import qualified Network.AWS.WorkSpaces.Types.SubnetId as Types
import qualified Network.AWS.WorkSpaces.Types.UserName as Types
import qualified Network.AWS.WorkSpaces.Types.VolumeEncryptionKey as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceProperties as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceState as Types

-- | Describes a WorkSpace.
--
-- /See:/ 'mkWorkspace' smart constructor.
data Workspace = Workspace'
  { bundleId :: Core.Maybe Types.BundleId
    -- ^ The identifier of the bundle used to create the WorkSpace.
  , computerName :: Core.Maybe Types.ComputerName
    -- ^ The name of the WorkSpace, as seen by the operating system. The format of this name varies. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace> . 
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the AWS Directory Service directory for the WorkSpace.
  , errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code that is returned if the WorkSpace cannot be created.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The text of the error message that is returned if the WorkSpace cannot be created.
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ The IP address of the WorkSpace.
  , modificationStates :: Core.Maybe [Types.ModificationState]
    -- ^ The modification states of the WorkSpace.
  , rootVolumeEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether the data stored on the root volume is encrypted.
  , state :: Core.Maybe Types.WorkspaceState
    -- ^ The operational state of the WorkSpace.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The identifier of the subnet for the WorkSpace.
  , userName :: Core.Maybe Types.UserName
    -- ^ The user for the WorkSpace.
  , userVolumeEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether the data stored on the user volume is encrypted.
  , volumeEncryptionKey :: Core.Maybe Types.VolumeEncryptionKey
    -- ^ The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
  , workspaceId :: Core.Maybe Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  , workspaceProperties :: Core.Maybe Types.WorkspaceProperties
    -- ^ The properties of the WorkSpace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Workspace' value with any optional fields omitted.
mkWorkspace
    :: Workspace
mkWorkspace
  = Workspace'{bundleId = Core.Nothing, computerName = Core.Nothing,
               directoryId = Core.Nothing, errorCode = Core.Nothing,
               errorMessage = Core.Nothing, ipAddress = Core.Nothing,
               modificationStates = Core.Nothing,
               rootVolumeEncryptionEnabled = Core.Nothing, state = Core.Nothing,
               subnetId = Core.Nothing, userName = Core.Nothing,
               userVolumeEncryptionEnabled = Core.Nothing,
               volumeEncryptionKey = Core.Nothing, workspaceId = Core.Nothing,
               workspaceProperties = Core.Nothing}

-- | The identifier of the bundle used to create the WorkSpace.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wBundleId :: Lens.Lens' Workspace (Core.Maybe Types.BundleId)
wBundleId = Lens.field @"bundleId"
{-# INLINEABLE wBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The name of the WorkSpace, as seen by the operating system. The format of this name varies. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace> . 
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wComputerName :: Lens.Lens' Workspace (Core.Maybe Types.ComputerName)
wComputerName = Lens.field @"computerName"
{-# INLINEABLE wComputerName #-}
{-# DEPRECATED computerName "Use generic-lens or generic-optics with 'computerName' instead"  #-}

-- | The identifier of the AWS Directory Service directory for the WorkSpace.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wDirectoryId :: Lens.Lens' Workspace (Core.Maybe Types.DirectoryId)
wDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE wDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The error code that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wErrorCode :: Lens.Lens' Workspace (Core.Maybe Types.ErrorCode)
wErrorCode = Lens.field @"errorCode"
{-# INLINEABLE wErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The text of the error message that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wErrorMessage :: Lens.Lens' Workspace (Core.Maybe Types.ErrorMessage)
wErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE wErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The IP address of the WorkSpace.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wIpAddress :: Lens.Lens' Workspace (Core.Maybe Types.IpAddress)
wIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE wIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | The modification states of the WorkSpace.
--
-- /Note:/ Consider using 'modificationStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wModificationStates :: Lens.Lens' Workspace (Core.Maybe [Types.ModificationState])
wModificationStates = Lens.field @"modificationStates"
{-# INLINEABLE wModificationStates #-}
{-# DEPRECATED modificationStates "Use generic-lens or generic-optics with 'modificationStates' instead"  #-}

-- | Indicates whether the data stored on the root volume is encrypted.
--
-- /Note:/ Consider using 'rootVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wRootVolumeEncryptionEnabled :: Lens.Lens' Workspace (Core.Maybe Core.Bool)
wRootVolumeEncryptionEnabled = Lens.field @"rootVolumeEncryptionEnabled"
{-# INLINEABLE wRootVolumeEncryptionEnabled #-}
{-# DEPRECATED rootVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'rootVolumeEncryptionEnabled' instead"  #-}

-- | The operational state of the WorkSpace.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wState :: Lens.Lens' Workspace (Core.Maybe Types.WorkspaceState)
wState = Lens.field @"state"
{-# INLINEABLE wState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The identifier of the subnet for the WorkSpace.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSubnetId :: Lens.Lens' Workspace (Core.Maybe Types.SubnetId)
wSubnetId = Lens.field @"subnetId"
{-# INLINEABLE wSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The user for the WorkSpace.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wUserName :: Lens.Lens' Workspace (Core.Maybe Types.UserName)
wUserName = Lens.field @"userName"
{-# INLINEABLE wUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | Indicates whether the data stored on the user volume is encrypted.
--
-- /Note:/ Consider using 'userVolumeEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wUserVolumeEncryptionEnabled :: Lens.Lens' Workspace (Core.Maybe Core.Bool)
wUserVolumeEncryptionEnabled = Lens.field @"userVolumeEncryptionEnabled"
{-# INLINEABLE wUserVolumeEncryptionEnabled #-}
{-# DEPRECATED userVolumeEncryptionEnabled "Use generic-lens or generic-optics with 'userVolumeEncryptionEnabled' instead"  #-}

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'volumeEncryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wVolumeEncryptionKey :: Lens.Lens' Workspace (Core.Maybe Types.VolumeEncryptionKey)
wVolumeEncryptionKey = Lens.field @"volumeEncryptionKey"
{-# INLINEABLE wVolumeEncryptionKey #-}
{-# DEPRECATED volumeEncryptionKey "Use generic-lens or generic-optics with 'volumeEncryptionKey' instead"  #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkspaceId :: Lens.Lens' Workspace (Core.Maybe Types.WorkspaceId)
wWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE wWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

-- | The properties of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkspaceProperties :: Lens.Lens' Workspace (Core.Maybe Types.WorkspaceProperties)
wWorkspaceProperties = Lens.field @"workspaceProperties"
{-# INLINEABLE wWorkspaceProperties #-}
{-# DEPRECATED workspaceProperties "Use generic-lens or generic-optics with 'workspaceProperties' instead"  #-}

instance Core.FromJSON Workspace where
        parseJSON
          = Core.withObject "Workspace" Core.$
              \ x ->
                Workspace' Core.<$>
                  (x Core..:? "BundleId") Core.<*> x Core..:? "ComputerName" Core.<*>
                    x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "ErrorCode"
                    Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "IpAddress"
                    Core.<*> x Core..:? "ModificationStates"
                    Core.<*> x Core..:? "RootVolumeEncryptionEnabled"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "SubnetId"
                    Core.<*> x Core..:? "UserName"
                    Core.<*> x Core..:? "UserVolumeEncryptionEnabled"
                    Core.<*> x Core..:? "VolumeEncryptionKey"
                    Core.<*> x Core..:? "WorkspaceId"
                    Core.<*> x Core..:? "WorkspaceProperties"
