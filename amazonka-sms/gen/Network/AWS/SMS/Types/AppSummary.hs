{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the application.
    appId :: Prelude.Maybe Prelude.Text,
    -- | Status of the application.
    status :: Prelude.Maybe AppStatus,
    -- | The creation time of the application.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The number of servers present in the application.
    totalServers :: Prelude.Maybe Prelude.Int,
    -- | The launch status of the application.
    launchStatus :: Prelude.Maybe AppLaunchStatus,
    -- | A message related to the replication status of the application.
    replicationStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the service role in the customer\'s account used by AWS SMS.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The replication status of the application.
    replicationStatus :: Prelude.Maybe AppReplicationStatus,
    -- | The ID of the application.
    importedAppId :: Prelude.Maybe Prelude.Text,
    -- | Status of the replication configuration.
    replicationConfigurationStatus :: Prelude.Maybe AppReplicationConfigurationStatus,
    -- | The timestamp of the application\'s most recent successful replication.
    latestReplicationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Details about the latest launch of the application.
    launchDetails :: Prelude.Maybe LaunchDetails,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | Status of the launch configuration.
    launchConfigurationStatus :: Prelude.Maybe AppLaunchConfigurationStatus,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last modified time of the application.
    lastModified :: Prelude.Maybe Prelude.POSIX,
    -- | The number of server groups present in the application.
    totalServerGroups :: Prelude.Maybe Prelude.Int,
    -- | A message related to the launch status of the application.
    launchStatusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { statusMessage = Prelude.Nothing,
      appId = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      totalServers = Prelude.Nothing,
      launchStatus = Prelude.Nothing,
      replicationStatusMessage = Prelude.Nothing,
      roleName = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      importedAppId = Prelude.Nothing,
      replicationConfigurationStatus = Prelude.Nothing,
      latestReplicationTime = Prelude.Nothing,
      launchDetails = Prelude.Nothing,
      name = Prelude.Nothing,
      launchConfigurationStatus = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      totalServerGroups = Prelude.Nothing,
      launchStatusMessage = Prelude.Nothing
    }

-- | A message related to the status of the application
appSummary_statusMessage :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_statusMessage = Lens.lens (\AppSummary' {statusMessage} -> statusMessage) (\s@AppSummary' {} a -> s {statusMessage = a} :: AppSummary)

-- | The unique ID of the application.
appSummary_appId :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_appId = Lens.lens (\AppSummary' {appId} -> appId) (\s@AppSummary' {} a -> s {appId = a} :: AppSummary)

-- | Status of the application.
appSummary_status :: Lens.Lens' AppSummary (Prelude.Maybe AppStatus)
appSummary_status = Lens.lens (\AppSummary' {status} -> status) (\s@AppSummary' {} a -> s {status = a} :: AppSummary)

-- | The creation time of the application.
appSummary_creationTime :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.UTCTime)
appSummary_creationTime = Lens.lens (\AppSummary' {creationTime} -> creationTime) (\s@AppSummary' {} a -> s {creationTime = a} :: AppSummary) Prelude.. Lens.mapping Prelude._Time

-- | The number of servers present in the application.
appSummary_totalServers :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Int)
appSummary_totalServers = Lens.lens (\AppSummary' {totalServers} -> totalServers) (\s@AppSummary' {} a -> s {totalServers = a} :: AppSummary)

-- | The launch status of the application.
appSummary_launchStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppLaunchStatus)
appSummary_launchStatus = Lens.lens (\AppSummary' {launchStatus} -> launchStatus) (\s@AppSummary' {} a -> s {launchStatus = a} :: AppSummary)

-- | A message related to the replication status of the application.
appSummary_replicationStatusMessage :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_replicationStatusMessage = Lens.lens (\AppSummary' {replicationStatusMessage} -> replicationStatusMessage) (\s@AppSummary' {} a -> s {replicationStatusMessage = a} :: AppSummary)

-- | The name of the service role in the customer\'s account used by AWS SMS.
appSummary_roleName :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_roleName = Lens.lens (\AppSummary' {roleName} -> roleName) (\s@AppSummary' {} a -> s {roleName = a} :: AppSummary)

-- | The replication status of the application.
appSummary_replicationStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppReplicationStatus)
appSummary_replicationStatus = Lens.lens (\AppSummary' {replicationStatus} -> replicationStatus) (\s@AppSummary' {} a -> s {replicationStatus = a} :: AppSummary)

-- | The ID of the application.
appSummary_importedAppId :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_importedAppId = Lens.lens (\AppSummary' {importedAppId} -> importedAppId) (\s@AppSummary' {} a -> s {importedAppId = a} :: AppSummary)

-- | Status of the replication configuration.
appSummary_replicationConfigurationStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppReplicationConfigurationStatus)
appSummary_replicationConfigurationStatus = Lens.lens (\AppSummary' {replicationConfigurationStatus} -> replicationConfigurationStatus) (\s@AppSummary' {} a -> s {replicationConfigurationStatus = a} :: AppSummary)

-- | The timestamp of the application\'s most recent successful replication.
appSummary_latestReplicationTime :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.UTCTime)
appSummary_latestReplicationTime = Lens.lens (\AppSummary' {latestReplicationTime} -> latestReplicationTime) (\s@AppSummary' {} a -> s {latestReplicationTime = a} :: AppSummary) Prelude.. Lens.mapping Prelude._Time

-- | Details about the latest launch of the application.
appSummary_launchDetails :: Lens.Lens' AppSummary (Prelude.Maybe LaunchDetails)
appSummary_launchDetails = Lens.lens (\AppSummary' {launchDetails} -> launchDetails) (\s@AppSummary' {} a -> s {launchDetails = a} :: AppSummary)

-- | The name of the application.
appSummary_name :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_name = Lens.lens (\AppSummary' {name} -> name) (\s@AppSummary' {} a -> s {name = a} :: AppSummary)

-- | Status of the launch configuration.
appSummary_launchConfigurationStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppLaunchConfigurationStatus)
appSummary_launchConfigurationStatus = Lens.lens (\AppSummary' {launchConfigurationStatus} -> launchConfigurationStatus) (\s@AppSummary' {} a -> s {launchConfigurationStatus = a} :: AppSummary)

-- | The description of the application.
appSummary_description :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_description = Lens.lens (\AppSummary' {description} -> description) (\s@AppSummary' {} a -> s {description = a} :: AppSummary)

-- | The last modified time of the application.
appSummary_lastModified :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.UTCTime)
appSummary_lastModified = Lens.lens (\AppSummary' {lastModified} -> lastModified) (\s@AppSummary' {} a -> s {lastModified = a} :: AppSummary) Prelude.. Lens.mapping Prelude._Time

-- | The number of server groups present in the application.
appSummary_totalServerGroups :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Int)
appSummary_totalServerGroups = Lens.lens (\AppSummary' {totalServerGroups} -> totalServerGroups) (\s@AppSummary' {} a -> s {totalServerGroups = a} :: AppSummary)

-- | A message related to the launch status of the application.
appSummary_launchStatusMessage :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_launchStatusMessage = Lens.lens (\AppSummary' {launchStatusMessage} -> launchStatusMessage) (\s@AppSummary' {} a -> s {launchStatusMessage = a} :: AppSummary)

instance Prelude.FromJSON AppSummary where
  parseJSON =
    Prelude.withObject
      "AppSummary"
      ( \x ->
          AppSummary'
            Prelude.<$> (x Prelude..:? "statusMessage")
            Prelude.<*> (x Prelude..:? "appId")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "totalServers")
            Prelude.<*> (x Prelude..:? "launchStatus")
            Prelude.<*> (x Prelude..:? "replicationStatusMessage")
            Prelude.<*> (x Prelude..:? "roleName")
            Prelude.<*> (x Prelude..:? "replicationStatus")
            Prelude.<*> (x Prelude..:? "importedAppId")
            Prelude.<*> (x Prelude..:? "replicationConfigurationStatus")
            Prelude.<*> (x Prelude..:? "latestReplicationTime")
            Prelude.<*> (x Prelude..:? "launchDetails")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "launchConfigurationStatus")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "lastModified")
            Prelude.<*> (x Prelude..:? "totalServerGroups")
            Prelude.<*> (x Prelude..:? "launchStatusMessage")
      )

instance Prelude.Hashable AppSummary

instance Prelude.NFData AppSummary
