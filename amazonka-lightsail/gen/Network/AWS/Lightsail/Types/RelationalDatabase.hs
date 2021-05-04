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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabase where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes a database.
--
-- /See:/ 'newRelationalDatabase' smart constructor.
data RelationalDatabase = RelationalDatabase'
  { -- | The bundle ID for the database. A bundle describes the performance
    -- specifications for your database.
    relationalDatabaseBundleId :: Prelude.Maybe Prelude.Text,
    -- | The master endpoint for the database.
    masterEndpoint :: Prelude.Maybe RelationalDatabaseEndpoint,
    -- | Describes the pending maintenance actions for the database.
    pendingMaintenanceActions :: Prelude.Maybe [PendingMaintenanceAction],
    -- | The daily time range during which automated backups are created for the
    -- database (for example, @16:00-16:30@).
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The certificate associated with the database.
    caCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The blueprint ID for the database. A blueprint describes the major
    -- engine version of a database.
    relationalDatabaseBlueprintId :: Prelude.Maybe Prelude.Text,
    -- | The latest point in time to which the database can be restored.
    -- Formatted in Unix time.
    latestRestorableTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the master database created when the Lightsail database
    -- resource is created.
    masterDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | Describes the hardware of the database.
    hardware :: Prelude.Maybe RelationalDatabaseHardware,
    -- | The timestamp when the database was created. Formatted in Unix time.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the database.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The master user name of the database.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type for the database (for example,
    -- @RelationalDatabase@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | A Boolean value indicating whether the database is publicly accessible.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The support code for the database. Include this code in your email to
    -- support when you have questions about a database in Lightsail. This code
    -- enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | Describes the current state of the database.
    state :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the database resource in Lightsail.
    name :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether automated backup retention is enabled
    -- for the database.
    backupRetentionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The database engine version (for example, @5.7.23@).
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur on the
    -- database.
    --
    -- In the format @ddd:hh24:mi-ddd:hh24:mi@. For example,
    -- @Tue:17:00-Tue:17:30@.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The status of parameter updates for the database.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text,
    -- | Describes pending database value modifications.
    pendingModifiedValues :: Prelude.Maybe PendingModifiedRelationalDatabaseValues,
    -- | The database software (for example, @MySQL@).
    engine :: Prelude.Maybe Prelude.Text,
    -- | Describes the secondary Availability Zone of a high availability
    -- database.
    --
    -- The secondary database is used for failover support of a high
    -- availability database.
    secondaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Region name and Availability Zone where the database is located.
    location :: Prelude.Maybe ResourceLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseBundleId', 'relationalDatabase_relationalDatabaseBundleId' - The bundle ID for the database. A bundle describes the performance
-- specifications for your database.
--
-- 'masterEndpoint', 'relationalDatabase_masterEndpoint' - The master endpoint for the database.
--
-- 'pendingMaintenanceActions', 'relationalDatabase_pendingMaintenanceActions' - Describes the pending maintenance actions for the database.
--
-- 'preferredBackupWindow', 'relationalDatabase_preferredBackupWindow' - The daily time range during which automated backups are created for the
-- database (for example, @16:00-16:30@).
--
-- 'caCertificateIdentifier', 'relationalDatabase_caCertificateIdentifier' - The certificate associated with the database.
--
-- 'relationalDatabaseBlueprintId', 'relationalDatabase_relationalDatabaseBlueprintId' - The blueprint ID for the database. A blueprint describes the major
-- engine version of a database.
--
-- 'latestRestorableTime', 'relationalDatabase_latestRestorableTime' - The latest point in time to which the database can be restored.
-- Formatted in Unix time.
--
-- 'masterDatabaseName', 'relationalDatabase_masterDatabaseName' - The name of the master database created when the Lightsail database
-- resource is created.
--
-- 'hardware', 'relationalDatabase_hardware' - Describes the hardware of the database.
--
-- 'createdAt', 'relationalDatabase_createdAt' - The timestamp when the database was created. Formatted in Unix time.
--
-- 'arn', 'relationalDatabase_arn' - The Amazon Resource Name (ARN) of the database.
--
-- 'masterUsername', 'relationalDatabase_masterUsername' - The master user name of the database.
--
-- 'resourceType', 'relationalDatabase_resourceType' - The Lightsail resource type for the database (for example,
-- @RelationalDatabase@).
--
-- 'publiclyAccessible', 'relationalDatabase_publiclyAccessible' - A Boolean value indicating whether the database is publicly accessible.
--
-- 'supportCode', 'relationalDatabase_supportCode' - The support code for the database. Include this code in your email to
-- support when you have questions about a database in Lightsail. This code
-- enables our support team to look up your Lightsail information more
-- easily.
--
-- 'state', 'relationalDatabase_state' - Describes the current state of the database.
--
-- 'name', 'relationalDatabase_name' - The unique name of the database resource in Lightsail.
--
-- 'backupRetentionEnabled', 'relationalDatabase_backupRetentionEnabled' - A Boolean value indicating whether automated backup retention is enabled
-- for the database.
--
-- 'engineVersion', 'relationalDatabase_engineVersion' - The database engine version (for example, @5.7.23@).
--
-- 'preferredMaintenanceWindow', 'relationalDatabase_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on the
-- database.
--
-- In the format @ddd:hh24:mi-ddd:hh24:mi@. For example,
-- @Tue:17:00-Tue:17:30@.
--
-- 'tags', 'relationalDatabase_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'parameterApplyStatus', 'relationalDatabase_parameterApplyStatus' - The status of parameter updates for the database.
--
-- 'pendingModifiedValues', 'relationalDatabase_pendingModifiedValues' - Describes pending database value modifications.
--
-- 'engine', 'relationalDatabase_engine' - The database software (for example, @MySQL@).
--
-- 'secondaryAvailabilityZone', 'relationalDatabase_secondaryAvailabilityZone' - Describes the secondary Availability Zone of a high availability
-- database.
--
-- The secondary database is used for failover support of a high
-- availability database.
--
-- 'location', 'relationalDatabase_location' - The Region name and Availability Zone where the database is located.
newRelationalDatabase ::
  RelationalDatabase
newRelationalDatabase =
  RelationalDatabase'
    { relationalDatabaseBundleId =
        Prelude.Nothing,
      masterEndpoint = Prelude.Nothing,
      pendingMaintenanceActions = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      caCertificateIdentifier = Prelude.Nothing,
      relationalDatabaseBlueprintId = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      masterDatabaseName = Prelude.Nothing,
      hardware = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      backupRetentionEnabled = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      tags = Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      engine = Prelude.Nothing,
      secondaryAvailabilityZone = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The bundle ID for the database. A bundle describes the performance
-- specifications for your database.
relationalDatabase_relationalDatabaseBundleId :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_relationalDatabaseBundleId = Lens.lens (\RelationalDatabase' {relationalDatabaseBundleId} -> relationalDatabaseBundleId) (\s@RelationalDatabase' {} a -> s {relationalDatabaseBundleId = a} :: RelationalDatabase)

-- | The master endpoint for the database.
relationalDatabase_masterEndpoint :: Lens.Lens' RelationalDatabase (Prelude.Maybe RelationalDatabaseEndpoint)
relationalDatabase_masterEndpoint = Lens.lens (\RelationalDatabase' {masterEndpoint} -> masterEndpoint) (\s@RelationalDatabase' {} a -> s {masterEndpoint = a} :: RelationalDatabase)

-- | Describes the pending maintenance actions for the database.
relationalDatabase_pendingMaintenanceActions :: Lens.Lens' RelationalDatabase (Prelude.Maybe [PendingMaintenanceAction])
relationalDatabase_pendingMaintenanceActions = Lens.lens (\RelationalDatabase' {pendingMaintenanceActions} -> pendingMaintenanceActions) (\s@RelationalDatabase' {} a -> s {pendingMaintenanceActions = a} :: RelationalDatabase) Prelude.. Lens.mapping Prelude._Coerce

-- | The daily time range during which automated backups are created for the
-- database (for example, @16:00-16:30@).
relationalDatabase_preferredBackupWindow :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_preferredBackupWindow = Lens.lens (\RelationalDatabase' {preferredBackupWindow} -> preferredBackupWindow) (\s@RelationalDatabase' {} a -> s {preferredBackupWindow = a} :: RelationalDatabase)

-- | The certificate associated with the database.
relationalDatabase_caCertificateIdentifier :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_caCertificateIdentifier = Lens.lens (\RelationalDatabase' {caCertificateIdentifier} -> caCertificateIdentifier) (\s@RelationalDatabase' {} a -> s {caCertificateIdentifier = a} :: RelationalDatabase)

-- | The blueprint ID for the database. A blueprint describes the major
-- engine version of a database.
relationalDatabase_relationalDatabaseBlueprintId :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_relationalDatabaseBlueprintId = Lens.lens (\RelationalDatabase' {relationalDatabaseBlueprintId} -> relationalDatabaseBlueprintId) (\s@RelationalDatabase' {} a -> s {relationalDatabaseBlueprintId = a} :: RelationalDatabase)

-- | The latest point in time to which the database can be restored.
-- Formatted in Unix time.
relationalDatabase_latestRestorableTime :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.UTCTime)
relationalDatabase_latestRestorableTime = Lens.lens (\RelationalDatabase' {latestRestorableTime} -> latestRestorableTime) (\s@RelationalDatabase' {} a -> s {latestRestorableTime = a} :: RelationalDatabase) Prelude.. Lens.mapping Prelude._Time

-- | The name of the master database created when the Lightsail database
-- resource is created.
relationalDatabase_masterDatabaseName :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_masterDatabaseName = Lens.lens (\RelationalDatabase' {masterDatabaseName} -> masterDatabaseName) (\s@RelationalDatabase' {} a -> s {masterDatabaseName = a} :: RelationalDatabase)

-- | Describes the hardware of the database.
relationalDatabase_hardware :: Lens.Lens' RelationalDatabase (Prelude.Maybe RelationalDatabaseHardware)
relationalDatabase_hardware = Lens.lens (\RelationalDatabase' {hardware} -> hardware) (\s@RelationalDatabase' {} a -> s {hardware = a} :: RelationalDatabase)

-- | The timestamp when the database was created. Formatted in Unix time.
relationalDatabase_createdAt :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.UTCTime)
relationalDatabase_createdAt = Lens.lens (\RelationalDatabase' {createdAt} -> createdAt) (\s@RelationalDatabase' {} a -> s {createdAt = a} :: RelationalDatabase) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the database.
relationalDatabase_arn :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_arn = Lens.lens (\RelationalDatabase' {arn} -> arn) (\s@RelationalDatabase' {} a -> s {arn = a} :: RelationalDatabase)

-- | The master user name of the database.
relationalDatabase_masterUsername :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_masterUsername = Lens.lens (\RelationalDatabase' {masterUsername} -> masterUsername) (\s@RelationalDatabase' {} a -> s {masterUsername = a} :: RelationalDatabase)

-- | The Lightsail resource type for the database (for example,
-- @RelationalDatabase@).
relationalDatabase_resourceType :: Lens.Lens' RelationalDatabase (Prelude.Maybe ResourceType)
relationalDatabase_resourceType = Lens.lens (\RelationalDatabase' {resourceType} -> resourceType) (\s@RelationalDatabase' {} a -> s {resourceType = a} :: RelationalDatabase)

-- | A Boolean value indicating whether the database is publicly accessible.
relationalDatabase_publiclyAccessible :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Bool)
relationalDatabase_publiclyAccessible = Lens.lens (\RelationalDatabase' {publiclyAccessible} -> publiclyAccessible) (\s@RelationalDatabase' {} a -> s {publiclyAccessible = a} :: RelationalDatabase)

-- | The support code for the database. Include this code in your email to
-- support when you have questions about a database in Lightsail. This code
-- enables our support team to look up your Lightsail information more
-- easily.
relationalDatabase_supportCode :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_supportCode = Lens.lens (\RelationalDatabase' {supportCode} -> supportCode) (\s@RelationalDatabase' {} a -> s {supportCode = a} :: RelationalDatabase)

-- | Describes the current state of the database.
relationalDatabase_state :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_state = Lens.lens (\RelationalDatabase' {state} -> state) (\s@RelationalDatabase' {} a -> s {state = a} :: RelationalDatabase)

-- | The unique name of the database resource in Lightsail.
relationalDatabase_name :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_name = Lens.lens (\RelationalDatabase' {name} -> name) (\s@RelationalDatabase' {} a -> s {name = a} :: RelationalDatabase)

-- | A Boolean value indicating whether automated backup retention is enabled
-- for the database.
relationalDatabase_backupRetentionEnabled :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Bool)
relationalDatabase_backupRetentionEnabled = Lens.lens (\RelationalDatabase' {backupRetentionEnabled} -> backupRetentionEnabled) (\s@RelationalDatabase' {} a -> s {backupRetentionEnabled = a} :: RelationalDatabase)

-- | The database engine version (for example, @5.7.23@).
relationalDatabase_engineVersion :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_engineVersion = Lens.lens (\RelationalDatabase' {engineVersion} -> engineVersion) (\s@RelationalDatabase' {} a -> s {engineVersion = a} :: RelationalDatabase)

-- | The weekly time range during which system maintenance can occur on the
-- database.
--
-- In the format @ddd:hh24:mi-ddd:hh24:mi@. For example,
-- @Tue:17:00-Tue:17:30@.
relationalDatabase_preferredMaintenanceWindow :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_preferredMaintenanceWindow = Lens.lens (\RelationalDatabase' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RelationalDatabase' {} a -> s {preferredMaintenanceWindow = a} :: RelationalDatabase)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
relationalDatabase_tags :: Lens.Lens' RelationalDatabase (Prelude.Maybe [Tag])
relationalDatabase_tags = Lens.lens (\RelationalDatabase' {tags} -> tags) (\s@RelationalDatabase' {} a -> s {tags = a} :: RelationalDatabase) Prelude.. Lens.mapping Prelude._Coerce

-- | The status of parameter updates for the database.
relationalDatabase_parameterApplyStatus :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_parameterApplyStatus = Lens.lens (\RelationalDatabase' {parameterApplyStatus} -> parameterApplyStatus) (\s@RelationalDatabase' {} a -> s {parameterApplyStatus = a} :: RelationalDatabase)

-- | Describes pending database value modifications.
relationalDatabase_pendingModifiedValues :: Lens.Lens' RelationalDatabase (Prelude.Maybe PendingModifiedRelationalDatabaseValues)
relationalDatabase_pendingModifiedValues = Lens.lens (\RelationalDatabase' {pendingModifiedValues} -> pendingModifiedValues) (\s@RelationalDatabase' {} a -> s {pendingModifiedValues = a} :: RelationalDatabase)

-- | The database software (for example, @MySQL@).
relationalDatabase_engine :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_engine = Lens.lens (\RelationalDatabase' {engine} -> engine) (\s@RelationalDatabase' {} a -> s {engine = a} :: RelationalDatabase)

-- | Describes the secondary Availability Zone of a high availability
-- database.
--
-- The secondary database is used for failover support of a high
-- availability database.
relationalDatabase_secondaryAvailabilityZone :: Lens.Lens' RelationalDatabase (Prelude.Maybe Prelude.Text)
relationalDatabase_secondaryAvailabilityZone = Lens.lens (\RelationalDatabase' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@RelationalDatabase' {} a -> s {secondaryAvailabilityZone = a} :: RelationalDatabase)

-- | The Region name and Availability Zone where the database is located.
relationalDatabase_location :: Lens.Lens' RelationalDatabase (Prelude.Maybe ResourceLocation)
relationalDatabase_location = Lens.lens (\RelationalDatabase' {location} -> location) (\s@RelationalDatabase' {} a -> s {location = a} :: RelationalDatabase)

instance Prelude.FromJSON RelationalDatabase where
  parseJSON =
    Prelude.withObject
      "RelationalDatabase"
      ( \x ->
          RelationalDatabase'
            Prelude.<$> (x Prelude..:? "relationalDatabaseBundleId")
            Prelude.<*> (x Prelude..:? "masterEndpoint")
            Prelude.<*> ( x Prelude..:? "pendingMaintenanceActions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "preferredBackupWindow")
            Prelude.<*> (x Prelude..:? "caCertificateIdentifier")
            Prelude.<*> (x Prelude..:? "relationalDatabaseBlueprintId")
            Prelude.<*> (x Prelude..:? "latestRestorableTime")
            Prelude.<*> (x Prelude..:? "masterDatabaseName")
            Prelude.<*> (x Prelude..:? "hardware")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "masterUsername")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "publiclyAccessible")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "backupRetentionEnabled")
            Prelude.<*> (x Prelude..:? "engineVersion")
            Prelude.<*> (x Prelude..:? "preferredMaintenanceWindow")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "parameterApplyStatus")
            Prelude.<*> (x Prelude..:? "pendingModifiedValues")
            Prelude.<*> (x Prelude..:? "engine")
            Prelude.<*> (x Prelude..:? "secondaryAvailabilityZone")
            Prelude.<*> (x Prelude..:? "location")
      )

instance Prelude.Hashable RelationalDatabase

instance Prelude.NFData RelationalDatabase
