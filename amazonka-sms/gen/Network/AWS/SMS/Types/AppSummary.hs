{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.AppLaunchConfigurationStatus
import Network.AWS.SMS.Types.AppLaunchStatus
import Network.AWS.SMS.Types.AppReplicationConfigurationStatus
import Network.AWS.SMS.Types.AppReplicationStatus
import Network.AWS.SMS.Types.AppStatus
import Network.AWS.SMS.Types.LaunchDetails

-- | Information about the application.
--
-- /See:/ 'newAppSummary' smart constructor.
data AppSummary = AppSummary'
  { -- | A message related to the status of the application
    statusMessage :: Core.Maybe Core.Text,
    -- | The unique ID of the application.
    appId :: Core.Maybe Core.Text,
    -- | Status of the application.
    status :: Core.Maybe AppStatus,
    -- | The creation time of the application.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The number of servers present in the application.
    totalServers :: Core.Maybe Core.Int,
    -- | The launch status of the application.
    launchStatus :: Core.Maybe AppLaunchStatus,
    -- | A message related to the replication status of the application.
    replicationStatusMessage :: Core.Maybe Core.Text,
    -- | The name of the service role in the customer\'s account used by AWS SMS.
    roleName :: Core.Maybe Core.Text,
    -- | The replication status of the application.
    replicationStatus :: Core.Maybe AppReplicationStatus,
    -- | The ID of the application.
    importedAppId :: Core.Maybe Core.Text,
    -- | Status of the replication configuration.
    replicationConfigurationStatus :: Core.Maybe AppReplicationConfigurationStatus,
    -- | The timestamp of the application\'s most recent successful replication.
    latestReplicationTime :: Core.Maybe Core.POSIX,
    -- | Details about the latest launch of the application.
    launchDetails :: Core.Maybe LaunchDetails,
    -- | The name of the application.
    name :: Core.Maybe Core.Text,
    -- | Status of the launch configuration.
    launchConfigurationStatus :: Core.Maybe AppLaunchConfigurationStatus,
    -- | The description of the application.
    description :: Core.Maybe Core.Text,
    -- | The last modified time of the application.
    lastModified :: Core.Maybe Core.POSIX,
    -- | The number of server groups present in the application.
    totalServerGroups :: Core.Maybe Core.Int,
    -- | A message related to the launch status of the application.
    launchStatusMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AppSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'appSummary_statusMessage' - A message related to the status of the application
--
-- 'appId', 'appSummary_appId' - The unique ID of the application.
--
-- 'status', 'appSummary_status' - Status of the application.
--
-- 'creationTime', 'appSummary_creationTime' - The creation time of the application.
--
-- 'totalServers', 'appSummary_totalServers' - The number of servers present in the application.
--
-- 'launchStatus', 'appSummary_launchStatus' - The launch status of the application.
--
-- 'replicationStatusMessage', 'appSummary_replicationStatusMessage' - A message related to the replication status of the application.
--
-- 'roleName', 'appSummary_roleName' - The name of the service role in the customer\'s account used by AWS SMS.
--
-- 'replicationStatus', 'appSummary_replicationStatus' - The replication status of the application.
--
-- 'importedAppId', 'appSummary_importedAppId' - The ID of the application.
--
-- 'replicationConfigurationStatus', 'appSummary_replicationConfigurationStatus' - Status of the replication configuration.
--
-- 'latestReplicationTime', 'appSummary_latestReplicationTime' - The timestamp of the application\'s most recent successful replication.
--
-- 'launchDetails', 'appSummary_launchDetails' - Details about the latest launch of the application.
--
-- 'name', 'appSummary_name' - The name of the application.
--
-- 'launchConfigurationStatus', 'appSummary_launchConfigurationStatus' - Status of the launch configuration.
--
-- 'description', 'appSummary_description' - The description of the application.
--
-- 'lastModified', 'appSummary_lastModified' - The last modified time of the application.
--
-- 'totalServerGroups', 'appSummary_totalServerGroups' - The number of server groups present in the application.
--
-- 'launchStatusMessage', 'appSummary_launchStatusMessage' - A message related to the launch status of the application.
newAppSummary ::
  AppSummary
newAppSummary =
  AppSummary'
    { statusMessage = Core.Nothing,
      appId = Core.Nothing,
      status = Core.Nothing,
      creationTime = Core.Nothing,
      totalServers = Core.Nothing,
      launchStatus = Core.Nothing,
      replicationStatusMessage = Core.Nothing,
      roleName = Core.Nothing,
      replicationStatus = Core.Nothing,
      importedAppId = Core.Nothing,
      replicationConfigurationStatus = Core.Nothing,
      latestReplicationTime = Core.Nothing,
      launchDetails = Core.Nothing,
      name = Core.Nothing,
      launchConfigurationStatus = Core.Nothing,
      description = Core.Nothing,
      lastModified = Core.Nothing,
      totalServerGroups = Core.Nothing,
      launchStatusMessage = Core.Nothing
    }

-- | A message related to the status of the application
appSummary_statusMessage :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_statusMessage = Lens.lens (\AppSummary' {statusMessage} -> statusMessage) (\s@AppSummary' {} a -> s {statusMessage = a} :: AppSummary)

-- | The unique ID of the application.
appSummary_appId :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_appId = Lens.lens (\AppSummary' {appId} -> appId) (\s@AppSummary' {} a -> s {appId = a} :: AppSummary)

-- | Status of the application.
appSummary_status :: Lens.Lens' AppSummary (Core.Maybe AppStatus)
appSummary_status = Lens.lens (\AppSummary' {status} -> status) (\s@AppSummary' {} a -> s {status = a} :: AppSummary)

-- | The creation time of the application.
appSummary_creationTime :: Lens.Lens' AppSummary (Core.Maybe Core.UTCTime)
appSummary_creationTime = Lens.lens (\AppSummary' {creationTime} -> creationTime) (\s@AppSummary' {} a -> s {creationTime = a} :: AppSummary) Core.. Lens.mapping Core._Time

-- | The number of servers present in the application.
appSummary_totalServers :: Lens.Lens' AppSummary (Core.Maybe Core.Int)
appSummary_totalServers = Lens.lens (\AppSummary' {totalServers} -> totalServers) (\s@AppSummary' {} a -> s {totalServers = a} :: AppSummary)

-- | The launch status of the application.
appSummary_launchStatus :: Lens.Lens' AppSummary (Core.Maybe AppLaunchStatus)
appSummary_launchStatus = Lens.lens (\AppSummary' {launchStatus} -> launchStatus) (\s@AppSummary' {} a -> s {launchStatus = a} :: AppSummary)

-- | A message related to the replication status of the application.
appSummary_replicationStatusMessage :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_replicationStatusMessage = Lens.lens (\AppSummary' {replicationStatusMessage} -> replicationStatusMessage) (\s@AppSummary' {} a -> s {replicationStatusMessage = a} :: AppSummary)

-- | The name of the service role in the customer\'s account used by AWS SMS.
appSummary_roleName :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_roleName = Lens.lens (\AppSummary' {roleName} -> roleName) (\s@AppSummary' {} a -> s {roleName = a} :: AppSummary)

-- | The replication status of the application.
appSummary_replicationStatus :: Lens.Lens' AppSummary (Core.Maybe AppReplicationStatus)
appSummary_replicationStatus = Lens.lens (\AppSummary' {replicationStatus} -> replicationStatus) (\s@AppSummary' {} a -> s {replicationStatus = a} :: AppSummary)

-- | The ID of the application.
appSummary_importedAppId :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_importedAppId = Lens.lens (\AppSummary' {importedAppId} -> importedAppId) (\s@AppSummary' {} a -> s {importedAppId = a} :: AppSummary)

-- | Status of the replication configuration.
appSummary_replicationConfigurationStatus :: Lens.Lens' AppSummary (Core.Maybe AppReplicationConfigurationStatus)
appSummary_replicationConfigurationStatus = Lens.lens (\AppSummary' {replicationConfigurationStatus} -> replicationConfigurationStatus) (\s@AppSummary' {} a -> s {replicationConfigurationStatus = a} :: AppSummary)

-- | The timestamp of the application\'s most recent successful replication.
appSummary_latestReplicationTime :: Lens.Lens' AppSummary (Core.Maybe Core.UTCTime)
appSummary_latestReplicationTime = Lens.lens (\AppSummary' {latestReplicationTime} -> latestReplicationTime) (\s@AppSummary' {} a -> s {latestReplicationTime = a} :: AppSummary) Core.. Lens.mapping Core._Time

-- | Details about the latest launch of the application.
appSummary_launchDetails :: Lens.Lens' AppSummary (Core.Maybe LaunchDetails)
appSummary_launchDetails = Lens.lens (\AppSummary' {launchDetails} -> launchDetails) (\s@AppSummary' {} a -> s {launchDetails = a} :: AppSummary)

-- | The name of the application.
appSummary_name :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_name = Lens.lens (\AppSummary' {name} -> name) (\s@AppSummary' {} a -> s {name = a} :: AppSummary)

-- | Status of the launch configuration.
appSummary_launchConfigurationStatus :: Lens.Lens' AppSummary (Core.Maybe AppLaunchConfigurationStatus)
appSummary_launchConfigurationStatus = Lens.lens (\AppSummary' {launchConfigurationStatus} -> launchConfigurationStatus) (\s@AppSummary' {} a -> s {launchConfigurationStatus = a} :: AppSummary)

-- | The description of the application.
appSummary_description :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_description = Lens.lens (\AppSummary' {description} -> description) (\s@AppSummary' {} a -> s {description = a} :: AppSummary)

-- | The last modified time of the application.
appSummary_lastModified :: Lens.Lens' AppSummary (Core.Maybe Core.UTCTime)
appSummary_lastModified = Lens.lens (\AppSummary' {lastModified} -> lastModified) (\s@AppSummary' {} a -> s {lastModified = a} :: AppSummary) Core.. Lens.mapping Core._Time

-- | The number of server groups present in the application.
appSummary_totalServerGroups :: Lens.Lens' AppSummary (Core.Maybe Core.Int)
appSummary_totalServerGroups = Lens.lens (\AppSummary' {totalServerGroups} -> totalServerGroups) (\s@AppSummary' {} a -> s {totalServerGroups = a} :: AppSummary)

-- | A message related to the launch status of the application.
appSummary_launchStatusMessage :: Lens.Lens' AppSummary (Core.Maybe Core.Text)
appSummary_launchStatusMessage = Lens.lens (\AppSummary' {launchStatusMessage} -> launchStatusMessage) (\s@AppSummary' {} a -> s {launchStatusMessage = a} :: AppSummary)

instance Core.FromJSON AppSummary where
  parseJSON =
    Core.withObject
      "AppSummary"
      ( \x ->
          AppSummary'
            Core.<$> (x Core..:? "statusMessage")
            Core.<*> (x Core..:? "appId")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "totalServers")
            Core.<*> (x Core..:? "launchStatus")
            Core.<*> (x Core..:? "replicationStatusMessage")
            Core.<*> (x Core..:? "roleName")
            Core.<*> (x Core..:? "replicationStatus")
            Core.<*> (x Core..:? "importedAppId")
            Core.<*> (x Core..:? "replicationConfigurationStatus")
            Core.<*> (x Core..:? "latestReplicationTime")
            Core.<*> (x Core..:? "launchDetails")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "launchConfigurationStatus")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "lastModified")
            Core.<*> (x Core..:? "totalServerGroups")
            Core.<*> (x Core..:? "launchStatusMessage")
      )

instance Core.Hashable AppSummary

instance Core.NFData AppSummary
