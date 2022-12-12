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
-- Module      : Amazonka.SMS.Types.AppSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.AppSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.AppLaunchConfigurationStatus
import Amazonka.SMS.Types.AppLaunchStatus
import Amazonka.SMS.Types.AppReplicationConfigurationStatus
import Amazonka.SMS.Types.AppReplicationStatus
import Amazonka.SMS.Types.AppStatus
import Amazonka.SMS.Types.LaunchDetails

-- | Information about the application.
--
-- /See:/ 'newAppSummary' smart constructor.
data AppSummary = AppSummary'
  { -- | The unique ID of the application.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the application.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application.
    importedAppId :: Prelude.Maybe Prelude.Text,
    -- | The last modified time of the application.
    lastModified :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of the application\'s most recent successful replication.
    latestReplicationTime :: Prelude.Maybe Data.POSIX,
    -- | Status of the launch configuration.
    launchConfigurationStatus :: Prelude.Maybe AppLaunchConfigurationStatus,
    -- | Details about the latest launch of the application.
    launchDetails :: Prelude.Maybe LaunchDetails,
    -- | The launch status of the application.
    launchStatus :: Prelude.Maybe AppLaunchStatus,
    -- | A message related to the launch status of the application.
    launchStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | Status of the replication configuration.
    replicationConfigurationStatus :: Prelude.Maybe AppReplicationConfigurationStatus,
    -- | The replication status of the application.
    replicationStatus :: Prelude.Maybe AppReplicationStatus,
    -- | A message related to the replication status of the application.
    replicationStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the service role in the customer\'s account used by Server
    -- Migration Service.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Status of the application.
    status :: Prelude.Maybe AppStatus,
    -- | A message related to the status of the application
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The number of server groups present in the application.
    totalServerGroups :: Prelude.Maybe Prelude.Int,
    -- | The number of servers present in the application.
    totalServers :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'appSummary_appId' - The unique ID of the application.
--
-- 'creationTime', 'appSummary_creationTime' - The creation time of the application.
--
-- 'description', 'appSummary_description' - The description of the application.
--
-- 'importedAppId', 'appSummary_importedAppId' - The ID of the application.
--
-- 'lastModified', 'appSummary_lastModified' - The last modified time of the application.
--
-- 'latestReplicationTime', 'appSummary_latestReplicationTime' - The timestamp of the application\'s most recent successful replication.
--
-- 'launchConfigurationStatus', 'appSummary_launchConfigurationStatus' - Status of the launch configuration.
--
-- 'launchDetails', 'appSummary_launchDetails' - Details about the latest launch of the application.
--
-- 'launchStatus', 'appSummary_launchStatus' - The launch status of the application.
--
-- 'launchStatusMessage', 'appSummary_launchStatusMessage' - A message related to the launch status of the application.
--
-- 'name', 'appSummary_name' - The name of the application.
--
-- 'replicationConfigurationStatus', 'appSummary_replicationConfigurationStatus' - Status of the replication configuration.
--
-- 'replicationStatus', 'appSummary_replicationStatus' - The replication status of the application.
--
-- 'replicationStatusMessage', 'appSummary_replicationStatusMessage' - A message related to the replication status of the application.
--
-- 'roleName', 'appSummary_roleName' - The name of the service role in the customer\'s account used by Server
-- Migration Service.
--
-- 'status', 'appSummary_status' - Status of the application.
--
-- 'statusMessage', 'appSummary_statusMessage' - A message related to the status of the application
--
-- 'totalServerGroups', 'appSummary_totalServerGroups' - The number of server groups present in the application.
--
-- 'totalServers', 'appSummary_totalServers' - The number of servers present in the application.
newAppSummary ::
  AppSummary
newAppSummary =
  AppSummary'
    { appId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      importedAppId = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      latestReplicationTime = Prelude.Nothing,
      launchConfigurationStatus = Prelude.Nothing,
      launchDetails = Prelude.Nothing,
      launchStatus = Prelude.Nothing,
      launchStatusMessage = Prelude.Nothing,
      name = Prelude.Nothing,
      replicationConfigurationStatus = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      replicationStatusMessage = Prelude.Nothing,
      roleName = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      totalServerGroups = Prelude.Nothing,
      totalServers = Prelude.Nothing
    }

-- | The unique ID of the application.
appSummary_appId :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_appId = Lens.lens (\AppSummary' {appId} -> appId) (\s@AppSummary' {} a -> s {appId = a} :: AppSummary)

-- | The creation time of the application.
appSummary_creationTime :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.UTCTime)
appSummary_creationTime = Lens.lens (\AppSummary' {creationTime} -> creationTime) (\s@AppSummary' {} a -> s {creationTime = a} :: AppSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the application.
appSummary_description :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_description = Lens.lens (\AppSummary' {description} -> description) (\s@AppSummary' {} a -> s {description = a} :: AppSummary)

-- | The ID of the application.
appSummary_importedAppId :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_importedAppId = Lens.lens (\AppSummary' {importedAppId} -> importedAppId) (\s@AppSummary' {} a -> s {importedAppId = a} :: AppSummary)

-- | The last modified time of the application.
appSummary_lastModified :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.UTCTime)
appSummary_lastModified = Lens.lens (\AppSummary' {lastModified} -> lastModified) (\s@AppSummary' {} a -> s {lastModified = a} :: AppSummary) Prelude.. Lens.mapping Data._Time

-- | The timestamp of the application\'s most recent successful replication.
appSummary_latestReplicationTime :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.UTCTime)
appSummary_latestReplicationTime = Lens.lens (\AppSummary' {latestReplicationTime} -> latestReplicationTime) (\s@AppSummary' {} a -> s {latestReplicationTime = a} :: AppSummary) Prelude.. Lens.mapping Data._Time

-- | Status of the launch configuration.
appSummary_launchConfigurationStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppLaunchConfigurationStatus)
appSummary_launchConfigurationStatus = Lens.lens (\AppSummary' {launchConfigurationStatus} -> launchConfigurationStatus) (\s@AppSummary' {} a -> s {launchConfigurationStatus = a} :: AppSummary)

-- | Details about the latest launch of the application.
appSummary_launchDetails :: Lens.Lens' AppSummary (Prelude.Maybe LaunchDetails)
appSummary_launchDetails = Lens.lens (\AppSummary' {launchDetails} -> launchDetails) (\s@AppSummary' {} a -> s {launchDetails = a} :: AppSummary)

-- | The launch status of the application.
appSummary_launchStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppLaunchStatus)
appSummary_launchStatus = Lens.lens (\AppSummary' {launchStatus} -> launchStatus) (\s@AppSummary' {} a -> s {launchStatus = a} :: AppSummary)

-- | A message related to the launch status of the application.
appSummary_launchStatusMessage :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_launchStatusMessage = Lens.lens (\AppSummary' {launchStatusMessage} -> launchStatusMessage) (\s@AppSummary' {} a -> s {launchStatusMessage = a} :: AppSummary)

-- | The name of the application.
appSummary_name :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_name = Lens.lens (\AppSummary' {name} -> name) (\s@AppSummary' {} a -> s {name = a} :: AppSummary)

-- | Status of the replication configuration.
appSummary_replicationConfigurationStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppReplicationConfigurationStatus)
appSummary_replicationConfigurationStatus = Lens.lens (\AppSummary' {replicationConfigurationStatus} -> replicationConfigurationStatus) (\s@AppSummary' {} a -> s {replicationConfigurationStatus = a} :: AppSummary)

-- | The replication status of the application.
appSummary_replicationStatus :: Lens.Lens' AppSummary (Prelude.Maybe AppReplicationStatus)
appSummary_replicationStatus = Lens.lens (\AppSummary' {replicationStatus} -> replicationStatus) (\s@AppSummary' {} a -> s {replicationStatus = a} :: AppSummary)

-- | A message related to the replication status of the application.
appSummary_replicationStatusMessage :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_replicationStatusMessage = Lens.lens (\AppSummary' {replicationStatusMessage} -> replicationStatusMessage) (\s@AppSummary' {} a -> s {replicationStatusMessage = a} :: AppSummary)

-- | The name of the service role in the customer\'s account used by Server
-- Migration Service.
appSummary_roleName :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_roleName = Lens.lens (\AppSummary' {roleName} -> roleName) (\s@AppSummary' {} a -> s {roleName = a} :: AppSummary)

-- | Status of the application.
appSummary_status :: Lens.Lens' AppSummary (Prelude.Maybe AppStatus)
appSummary_status = Lens.lens (\AppSummary' {status} -> status) (\s@AppSummary' {} a -> s {status = a} :: AppSummary)

-- | A message related to the status of the application
appSummary_statusMessage :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Text)
appSummary_statusMessage = Lens.lens (\AppSummary' {statusMessage} -> statusMessage) (\s@AppSummary' {} a -> s {statusMessage = a} :: AppSummary)

-- | The number of server groups present in the application.
appSummary_totalServerGroups :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Int)
appSummary_totalServerGroups = Lens.lens (\AppSummary' {totalServerGroups} -> totalServerGroups) (\s@AppSummary' {} a -> s {totalServerGroups = a} :: AppSummary)

-- | The number of servers present in the application.
appSummary_totalServers :: Lens.Lens' AppSummary (Prelude.Maybe Prelude.Int)
appSummary_totalServers = Lens.lens (\AppSummary' {totalServers} -> totalServers) (\s@AppSummary' {} a -> s {totalServers = a} :: AppSummary)

instance Data.FromJSON AppSummary where
  parseJSON =
    Data.withObject
      "AppSummary"
      ( \x ->
          AppSummary'
            Prelude.<$> (x Data..:? "appId")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "importedAppId")
            Prelude.<*> (x Data..:? "lastModified")
            Prelude.<*> (x Data..:? "latestReplicationTime")
            Prelude.<*> (x Data..:? "launchConfigurationStatus")
            Prelude.<*> (x Data..:? "launchDetails")
            Prelude.<*> (x Data..:? "launchStatus")
            Prelude.<*> (x Data..:? "launchStatusMessage")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "replicationConfigurationStatus")
            Prelude.<*> (x Data..:? "replicationStatus")
            Prelude.<*> (x Data..:? "replicationStatusMessage")
            Prelude.<*> (x Data..:? "roleName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "totalServerGroups")
            Prelude.<*> (x Data..:? "totalServers")
      )

instance Prelude.Hashable AppSummary where
  hashWithSalt _salt AppSummary' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` importedAppId
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` latestReplicationTime
      `Prelude.hashWithSalt` launchConfigurationStatus
      `Prelude.hashWithSalt` launchDetails
      `Prelude.hashWithSalt` launchStatus
      `Prelude.hashWithSalt` launchStatusMessage
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` replicationConfigurationStatus
      `Prelude.hashWithSalt` replicationStatus
      `Prelude.hashWithSalt` replicationStatusMessage
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` totalServerGroups
      `Prelude.hashWithSalt` totalServers

instance Prelude.NFData AppSummary where
  rnf AppSummary' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf importedAppId
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf latestReplicationTime
      `Prelude.seq` Prelude.rnf launchConfigurationStatus
      `Prelude.seq` Prelude.rnf launchDetails
      `Prelude.seq` Prelude.rnf launchStatus
      `Prelude.seq` Prelude.rnf launchStatusMessage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf replicationConfigurationStatus
      `Prelude.seq` Prelude.rnf replicationStatus
      `Prelude.seq` Prelude.rnf replicationStatusMessage
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf totalServerGroups
      `Prelude.seq` Prelude.rnf totalServers
