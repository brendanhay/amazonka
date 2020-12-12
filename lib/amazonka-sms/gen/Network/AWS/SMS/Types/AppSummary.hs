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
    asCreationTime,
    asTotalServers,
    asStatus,
    asLaunchDetails,
    asLaunchStatusMessage,
    asReplicationConfigurationStatus,
    asReplicationStatusMessage,
    asTotalServerGroups,
    asRoleName,
    asLaunchConfigurationStatus,
    asLaunchStatus,
    asAppId,
    asName,
    asStatusMessage,
    asLatestReplicationTime,
    asImportedAppId,
    asReplicationStatus,
    asLastModified,
    asDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.AppLaunchConfigurationStatus
import Network.AWS.SMS.Types.AppLaunchStatus
import Network.AWS.SMS.Types.AppReplicationConfigurationStatus
import Network.AWS.SMS.Types.AppReplicationStatus
import Network.AWS.SMS.Types.AppStatus
import Network.AWS.SMS.Types.LaunchDetails

-- | Information about the application.
--
-- /See:/ 'mkAppSummary' smart constructor.
data AppSummary = AppSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    totalServers :: Lude.Maybe Lude.Int,
    status :: Lude.Maybe AppStatus,
    launchDetails :: Lude.Maybe LaunchDetails,
    launchStatusMessage :: Lude.Maybe Lude.Text,
    replicationConfigurationStatus ::
      Lude.Maybe AppReplicationConfigurationStatus,
    replicationStatusMessage :: Lude.Maybe Lude.Text,
    totalServerGroups :: Lude.Maybe Lude.Int,
    roleName :: Lude.Maybe Lude.Text,
    launchConfigurationStatus ::
      Lude.Maybe AppLaunchConfigurationStatus,
    launchStatus :: Lude.Maybe AppLaunchStatus,
    appId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    statusMessage :: Lude.Maybe Lude.Text,
    latestReplicationTime :: Lude.Maybe Lude.Timestamp,
    importedAppId :: Lude.Maybe Lude.Text,
    replicationStatus :: Lude.Maybe AppReplicationStatus,
    lastModified :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppSummary' with the minimum fields required to make a request.
--
-- * 'appId' - The unique ID of the application.
-- * 'creationTime' - The creation time of the application.
-- * 'description' - The description of the application.
-- * 'importedAppId' - The ID of the application.
-- * 'lastModified' - The last modified time of the application.
-- * 'latestReplicationTime' - The timestamp of the application's most recent successful replication.
-- * 'launchConfigurationStatus' - Status of the launch configuration.
-- * 'launchDetails' - Details about the latest launch of the application.
-- * 'launchStatus' - The launch status of the application.
-- * 'launchStatusMessage' - A message related to the launch status of the application.
-- * 'name' - The name of the application.
-- * 'replicationConfigurationStatus' - Status of the replication configuration.
-- * 'replicationStatus' - The replication status of the application.
-- * 'replicationStatusMessage' - A message related to the replication status of the application.
-- * 'roleName' - The name of the service role in the customer's account used by AWS SMS.
-- * 'status' - Status of the application.
-- * 'statusMessage' - A message related to the status of the application
-- * 'totalServerGroups' - The number of server groups present in the application.
-- * 'totalServers' - The number of servers present in the application.
mkAppSummary ::
  AppSummary
mkAppSummary =
  AppSummary'
    { creationTime = Lude.Nothing,
      totalServers = Lude.Nothing,
      status = Lude.Nothing,
      launchDetails = Lude.Nothing,
      launchStatusMessage = Lude.Nothing,
      replicationConfigurationStatus = Lude.Nothing,
      replicationStatusMessage = Lude.Nothing,
      totalServerGroups = Lude.Nothing,
      roleName = Lude.Nothing,
      launchConfigurationStatus = Lude.Nothing,
      launchStatus = Lude.Nothing,
      appId = Lude.Nothing,
      name = Lude.Nothing,
      statusMessage = Lude.Nothing,
      latestReplicationTime = Lude.Nothing,
      importedAppId = Lude.Nothing,
      replicationStatus = Lude.Nothing,
      lastModified = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The creation time of the application.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCreationTime :: Lens.Lens' AppSummary (Lude.Maybe Lude.Timestamp)
asCreationTime = Lens.lens (creationTime :: AppSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: AppSummary)
{-# DEPRECATED asCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The number of servers present in the application.
--
-- /Note:/ Consider using 'totalServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTotalServers :: Lens.Lens' AppSummary (Lude.Maybe Lude.Int)
asTotalServers = Lens.lens (totalServers :: AppSummary -> Lude.Maybe Lude.Int) (\s a -> s {totalServers = a} :: AppSummary)
{-# DEPRECATED asTotalServers "Use generic-lens or generic-optics with 'totalServers' instead." #-}

-- | Status of the application.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStatus :: Lens.Lens' AppSummary (Lude.Maybe AppStatus)
asStatus = Lens.lens (status :: AppSummary -> Lude.Maybe AppStatus) (\s a -> s {status = a} :: AppSummary)
{-# DEPRECATED asStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Details about the latest launch of the application.
--
-- /Note:/ Consider using 'launchDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchDetails :: Lens.Lens' AppSummary (Lude.Maybe LaunchDetails)
asLaunchDetails = Lens.lens (launchDetails :: AppSummary -> Lude.Maybe LaunchDetails) (\s a -> s {launchDetails = a} :: AppSummary)
{-# DEPRECATED asLaunchDetails "Use generic-lens or generic-optics with 'launchDetails' instead." #-}

-- | A message related to the launch status of the application.
--
-- /Note:/ Consider using 'launchStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchStatusMessage :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asLaunchStatusMessage = Lens.lens (launchStatusMessage :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {launchStatusMessage = a} :: AppSummary)
{-# DEPRECATED asLaunchStatusMessage "Use generic-lens or generic-optics with 'launchStatusMessage' instead." #-}

-- | Status of the replication configuration.
--
-- /Note:/ Consider using 'replicationConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asReplicationConfigurationStatus :: Lens.Lens' AppSummary (Lude.Maybe AppReplicationConfigurationStatus)
asReplicationConfigurationStatus = Lens.lens (replicationConfigurationStatus :: AppSummary -> Lude.Maybe AppReplicationConfigurationStatus) (\s a -> s {replicationConfigurationStatus = a} :: AppSummary)
{-# DEPRECATED asReplicationConfigurationStatus "Use generic-lens or generic-optics with 'replicationConfigurationStatus' instead." #-}

-- | A message related to the replication status of the application.
--
-- /Note:/ Consider using 'replicationStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asReplicationStatusMessage :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asReplicationStatusMessage = Lens.lens (replicationStatusMessage :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {replicationStatusMessage = a} :: AppSummary)
{-# DEPRECATED asReplicationStatusMessage "Use generic-lens or generic-optics with 'replicationStatusMessage' instead." #-}

-- | The number of server groups present in the application.
--
-- /Note:/ Consider using 'totalServerGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTotalServerGroups :: Lens.Lens' AppSummary (Lude.Maybe Lude.Int)
asTotalServerGroups = Lens.lens (totalServerGroups :: AppSummary -> Lude.Maybe Lude.Int) (\s a -> s {totalServerGroups = a} :: AppSummary)
{-# DEPRECATED asTotalServerGroups "Use generic-lens or generic-optics with 'totalServerGroups' instead." #-}

-- | The name of the service role in the customer's account used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRoleName :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asRoleName = Lens.lens (roleName :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: AppSummary)
{-# DEPRECATED asRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Status of the launch configuration.
--
-- /Note:/ Consider using 'launchConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchConfigurationStatus :: Lens.Lens' AppSummary (Lude.Maybe AppLaunchConfigurationStatus)
asLaunchConfigurationStatus = Lens.lens (launchConfigurationStatus :: AppSummary -> Lude.Maybe AppLaunchConfigurationStatus) (\s a -> s {launchConfigurationStatus = a} :: AppSummary)
{-# DEPRECATED asLaunchConfigurationStatus "Use generic-lens or generic-optics with 'launchConfigurationStatus' instead." #-}

-- | The launch status of the application.
--
-- /Note:/ Consider using 'launchStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLaunchStatus :: Lens.Lens' AppSummary (Lude.Maybe AppLaunchStatus)
asLaunchStatus = Lens.lens (launchStatus :: AppSummary -> Lude.Maybe AppLaunchStatus) (\s a -> s {launchStatus = a} :: AppSummary)
{-# DEPRECATED asLaunchStatus "Use generic-lens or generic-optics with 'launchStatus' instead." #-}

-- | The unique ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAppId :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asAppId = Lens.lens (appId :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: AppSummary)
{-# DEPRECATED asAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asName = Lens.lens (name :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AppSummary)
{-# DEPRECATED asName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A message related to the status of the application
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStatusMessage :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asStatusMessage = Lens.lens (statusMessage :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: AppSummary)
{-# DEPRECATED asStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The timestamp of the application's most recent successful replication.
--
-- /Note:/ Consider using 'latestReplicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLatestReplicationTime :: Lens.Lens' AppSummary (Lude.Maybe Lude.Timestamp)
asLatestReplicationTime = Lens.lens (latestReplicationTime :: AppSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestReplicationTime = a} :: AppSummary)
{-# DEPRECATED asLatestReplicationTime "Use generic-lens or generic-optics with 'latestReplicationTime' instead." #-}

-- | The ID of the application.
--
-- /Note:/ Consider using 'importedAppId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asImportedAppId :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asImportedAppId = Lens.lens (importedAppId :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {importedAppId = a} :: AppSummary)
{-# DEPRECATED asImportedAppId "Use generic-lens or generic-optics with 'importedAppId' instead." #-}

-- | The replication status of the application.
--
-- /Note:/ Consider using 'replicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asReplicationStatus :: Lens.Lens' AppSummary (Lude.Maybe AppReplicationStatus)
asReplicationStatus = Lens.lens (replicationStatus :: AppSummary -> Lude.Maybe AppReplicationStatus) (\s a -> s {replicationStatus = a} :: AppSummary)
{-# DEPRECATED asReplicationStatus "Use generic-lens or generic-optics with 'replicationStatus' instead." #-}

-- | The last modified time of the application.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLastModified :: Lens.Lens' AppSummary (Lude.Maybe Lude.Timestamp)
asLastModified = Lens.lens (lastModified :: AppSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: AppSummary)
{-# DEPRECATED asLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDescription :: Lens.Lens' AppSummary (Lude.Maybe Lude.Text)
asDescription = Lens.lens (description :: AppSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AppSummary)
{-# DEPRECATED asDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON AppSummary where
  parseJSON =
    Lude.withObject
      "AppSummary"
      ( \x ->
          AppSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "totalServers")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "launchDetails")
            Lude.<*> (x Lude..:? "launchStatusMessage")
            Lude.<*> (x Lude..:? "replicationConfigurationStatus")
            Lude.<*> (x Lude..:? "replicationStatusMessage")
            Lude.<*> (x Lude..:? "totalServerGroups")
            Lude.<*> (x Lude..:? "roleName")
            Lude.<*> (x Lude..:? "launchConfigurationStatus")
            Lude.<*> (x Lude..:? "launchStatus")
            Lude.<*> (x Lude..:? "appId")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "statusMessage")
            Lude.<*> (x Lude..:? "latestReplicationTime")
            Lude.<*> (x Lude..:? "importedAppId")
            Lude.<*> (x Lude..:? "replicationStatus")
            Lude.<*> (x Lude..:? "lastModified")
            Lude.<*> (x Lude..:? "description")
      )
