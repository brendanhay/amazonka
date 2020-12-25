{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppSummary
  ( AppSummary (..),

    -- * Smart constructor
    mkAppSummary,

    -- * Lenses
    asAppId,
    asCreationTime,
    asDescription,
    asImportedAppId,
    asLastModified,
    asLatestReplicationTime,
    asLaunchConfigurationStatus,
    asLaunchDetails,
    asLaunchStatus,
    asLaunchStatusMessage,
    asName,
    asReplicationConfigurationStatus,
    asReplicationStatus,
    asReplicationStatusMessage,
    asRoleName,
    asStatus,
    asStatusMessage,
    asTotalServerGroups,
    asTotalServers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.AppDescription as Types
import qualified Network.AWS.SMS.Types.AppId as Types
import qualified Network.AWS.SMS.Types.AppLaunchConfigurationStatus as Types
import qualified Network.AWS.SMS.Types.AppLaunchStatus as Types
import qualified Network.AWS.SMS.Types.AppName as Types
import qualified Network.AWS.SMS.Types.AppReplicationConfigurationStatus as Types
import qualified Network.AWS.SMS.Types.AppReplicationStatus as Types
import qualified Network.AWS.SMS.Types.AppReplicationStatusMessage as Types
import qualified Network.AWS.SMS.Types.AppStatus as Types
import qualified Network.AWS.SMS.Types.ImportedAppId as Types
import qualified Network.AWS.SMS.Types.LaunchDetails as Types
import qualified Network.AWS.SMS.Types.LaunchStatusMessage as Types
import qualified Network.AWS.SMS.Types.RoleName as Types
import qualified Network.AWS.SMS.Types.StatusMessage as Types

-- | Information about the application.
--
-- /See:/ 'mkAppSummary' smart constructor.
data AppSummary = AppSummary'
  { -- | The unique ID of the application.
    appId :: Core.Maybe Types.AppId,
    -- | The creation time of the application.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the application.
    description :: Core.Maybe Types.AppDescription,
    -- | The ID of the application.
    importedAppId :: Core.Maybe Types.ImportedAppId,
    -- | The last modified time of the application.
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | The timestamp of the application's most recent successful replication.
    latestReplicationTime :: Core.Maybe Core.NominalDiffTime,
    -- | Status of the launch configuration.
    launchConfigurationStatus :: Core.Maybe Types.AppLaunchConfigurationStatus,
    -- | Details about the latest launch of the application.
    launchDetails :: Core.Maybe Types.LaunchDetails,
    -- | The launch status of the application.
    launchStatus :: Core.Maybe Types.AppLaunchStatus,
    -- | A message related to the launch status of the application.
    launchStatusMessage :: Core.Maybe Types.LaunchStatusMessage,
    -- | The name of the application.
    name :: Core.Maybe Types.AppName,
    -- | Status of the replication configuration.
    replicationConfigurationStatus :: Core.Maybe Types.AppReplicationConfigurationStatus,
    -- | The replication status of the application.
    replicationStatus :: Core.Maybe Types.AppReplicationStatus,
    -- | A message related to the replication status of the application.
    replicationStatusMessage :: Core.Maybe Types.AppReplicationStatusMessage,
    -- | The name of the service role in the customer's account used by AWS SMS.
    roleName :: Core.Maybe Types.RoleName,
    -- | Status of the application.
    status :: Core.Maybe Types.AppStatus,
    -- | A message related to the status of the application
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | The number of server groups present in the application.
    totalServerGroups :: Core.Maybe Core.Int,
    -- | The number of servers present in the application.
    totalServers :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AppSummary' value with any optional fields omitted.
mkAppSummary ::
  AppSummary
mkAppSummary =
  AppSummary'
    { appId = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      importedAppId = Core.Nothing,
      lastModified = Core.Nothing,
      latestReplicationTime = Core.Nothing,
      launchConfigurationStatus = Core.Nothing,
      launchDetails = Core.Nothing,
      launchStatus = Core.Nothing,
      launchStatusMessage = Core.Nothing,
      name = Core.Nothing,
      replicationConfigurationStatus = Core.Nothing,
      replicationStatus = Core.Nothing,
      replicationStatusMessage = Core.Nothing,
      roleName = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      totalServerGroups = Core.Nothing,
      totalServers = Core.Nothing
    }

-- | The unique ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAppId :: Lens.Lens' AppSummary (Core.Maybe Types.AppId)
asAppId = Lens.field @"appId"
{-# DEPRECATED asAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The creation time of the application.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCreationTime :: Lens.Lens' AppSummary (Core.Maybe Core.NominalDiffTime)
asCreationTime = Lens.field @"creationTime"
{-# DEPRECATED asCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDescription :: Lens.Lens' AppSummary (Core.Maybe Types.AppDescription)
asDescription = Lens.field @"description"
{-# DEPRECATED asDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the application.
--
-- /Note:/ Consider using 'importedAppId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asImportedAppId :: Lens.Lens' AppSummary (Core.Maybe Types.ImportedAppId)
asImportedAppId = Lens.field @"importedAppId"
{-# DEPRECATED asImportedAppId "Use generic-lens or generic-optics with 'importedAppId' instead." #-}

-- | The last modified time of the application.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLastModified :: Lens.Lens' AppSummary (Core.Maybe Core.NominalDiffTime)
asLastModified = Lens.field @"lastModified"
{-# DEPRECATED asLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The timestamp of the application's most recent successful replication.
--
-- /Note:/ Consider using 'latestReplicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLatestReplicationTime :: Lens.Lens' AppSummary (Core.Maybe Core.NominalDiffTime)
asLatestReplicationTime = Lens.field @"latestReplicationTime"
{-# DEPRECATED asLatestReplicationTime "Use generic-lens or generic-optics with 'latestReplicationTime' instead." #-}

-- | Status of the launch configuration.
--
-- /Note:/ Consider using 'launchConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchConfigurationStatus :: Lens.Lens' AppSummary (Core.Maybe Types.AppLaunchConfigurationStatus)
asLaunchConfigurationStatus = Lens.field @"launchConfigurationStatus"
{-# DEPRECATED asLaunchConfigurationStatus "Use generic-lens or generic-optics with 'launchConfigurationStatus' instead." #-}

-- | Details about the latest launch of the application.
--
-- /Note:/ Consider using 'launchDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchDetails :: Lens.Lens' AppSummary (Core.Maybe Types.LaunchDetails)
asLaunchDetails = Lens.field @"launchDetails"
{-# DEPRECATED asLaunchDetails "Use generic-lens or generic-optics with 'launchDetails' instead." #-}

-- | The launch status of the application.
--
-- /Note:/ Consider using 'launchStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchStatus :: Lens.Lens' AppSummary (Core.Maybe Types.AppLaunchStatus)
asLaunchStatus = Lens.field @"launchStatus"
{-# DEPRECATED asLaunchStatus "Use generic-lens or generic-optics with 'launchStatus' instead." #-}

-- | A message related to the launch status of the application.
--
-- /Note:/ Consider using 'launchStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchStatusMessage :: Lens.Lens' AppSummary (Core.Maybe Types.LaunchStatusMessage)
asLaunchStatusMessage = Lens.field @"launchStatusMessage"
{-# DEPRECATED asLaunchStatusMessage "Use generic-lens or generic-optics with 'launchStatusMessage' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' AppSummary (Core.Maybe Types.AppName)
asName = Lens.field @"name"
{-# DEPRECATED asName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Status of the replication configuration.
--
-- /Note:/ Consider using 'replicationConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asReplicationConfigurationStatus :: Lens.Lens' AppSummary (Core.Maybe Types.AppReplicationConfigurationStatus)
asReplicationConfigurationStatus = Lens.field @"replicationConfigurationStatus"
{-# DEPRECATED asReplicationConfigurationStatus "Use generic-lens or generic-optics with 'replicationConfigurationStatus' instead." #-}

-- | The replication status of the application.
--
-- /Note:/ Consider using 'replicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asReplicationStatus :: Lens.Lens' AppSummary (Core.Maybe Types.AppReplicationStatus)
asReplicationStatus = Lens.field @"replicationStatus"
{-# DEPRECATED asReplicationStatus "Use generic-lens or generic-optics with 'replicationStatus' instead." #-}

-- | A message related to the replication status of the application.
--
-- /Note:/ Consider using 'replicationStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asReplicationStatusMessage :: Lens.Lens' AppSummary (Core.Maybe Types.AppReplicationStatusMessage)
asReplicationStatusMessage = Lens.field @"replicationStatusMessage"
{-# DEPRECATED asReplicationStatusMessage "Use generic-lens or generic-optics with 'replicationStatusMessage' instead." #-}

-- | The name of the service role in the customer's account used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRoleName :: Lens.Lens' AppSummary (Core.Maybe Types.RoleName)
asRoleName = Lens.field @"roleName"
{-# DEPRECATED asRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Status of the application.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStatus :: Lens.Lens' AppSummary (Core.Maybe Types.AppStatus)
asStatus = Lens.field @"status"
{-# DEPRECATED asStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A message related to the status of the application
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStatusMessage :: Lens.Lens' AppSummary (Core.Maybe Types.StatusMessage)
asStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED asStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The number of server groups present in the application.
--
-- /Note:/ Consider using 'totalServerGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTotalServerGroups :: Lens.Lens' AppSummary (Core.Maybe Core.Int)
asTotalServerGroups = Lens.field @"totalServerGroups"
{-# DEPRECATED asTotalServerGroups "Use generic-lens or generic-optics with 'totalServerGroups' instead." #-}

-- | The number of servers present in the application.
--
-- /Note:/ Consider using 'totalServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTotalServers :: Lens.Lens' AppSummary (Core.Maybe Core.Int)
asTotalServers = Lens.field @"totalServers"
{-# DEPRECATED asTotalServers "Use generic-lens or generic-optics with 'totalServers' instead." #-}

instance Core.FromJSON AppSummary where
  parseJSON =
    Core.withObject "AppSummary" Core.$
      \x ->
        AppSummary'
          Core.<$> (x Core..:? "appId")
          Core.<*> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "importedAppId")
          Core.<*> (x Core..:? "lastModified")
          Core.<*> (x Core..:? "latestReplicationTime")
          Core.<*> (x Core..:? "launchConfigurationStatus")
          Core.<*> (x Core..:? "launchDetails")
          Core.<*> (x Core..:? "launchStatus")
          Core.<*> (x Core..:? "launchStatusMessage")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "replicationConfigurationStatus")
          Core.<*> (x Core..:? "replicationStatus")
          Core.<*> (x Core..:? "replicationStatusMessage")
          Core.<*> (x Core..:? "roleName")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "statusMessage")
          Core.<*> (x Core..:? "totalServerGroups")
          Core.<*> (x Core..:? "totalServers")
