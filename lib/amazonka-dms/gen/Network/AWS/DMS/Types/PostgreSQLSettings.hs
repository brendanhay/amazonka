{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.PostgreSQLSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.PostgreSQLSettings
  ( PostgreSQLSettings (..),

    -- * Smart constructor
    mkPostgreSQLSettings,

    -- * Lenses
    psqlsAfterConnectScript,
    psqlsCaptureDdls,
    psqlsDatabaseName,
    psqlsDdlArtifactsSchema,
    psqlsExecuteTimeout,
    psqlsFailTasksOnLobTruncation,
    psqlsMaxFileSize,
    psqlsPassword,
    psqlsPort,
    psqlsServerName,
    psqlsSlotName,
    psqlsUsername,
  )
where

import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines a PostgreSQL endpoint.
--
-- /See:/ 'mkPostgreSQLSettings' smart constructor.
data PostgreSQLSettings = PostgreSQLSettings'
  { -- | For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data.
    --
    -- Example: @afterConnectScript=SET session_replication_role='replica'@
    afterConnectScript :: Core.Maybe Types.String,
    -- | To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts.
    --
    -- If this value is set to @N@ , you don't have to create tables or triggers on the source database.
    captureDdls :: Core.Maybe Core.Bool,
    -- | Database name for the endpoint.
    databaseName :: Core.Maybe Types.String,
    -- | The schema in which the operational DDL database artifacts are created.
    --
    -- Example: @ddlArtifactsSchema=xyzddlschema;@
    ddlArtifactsSchema :: Core.Maybe Types.String,
    -- | Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds.
    --
    -- Example: @executeTimeout=100;@
    executeTimeout :: Core.Maybe Core.Int,
    -- | When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ .
    --
    -- If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
    failTasksOnLobTruncation :: Core.Maybe Core.Bool,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Core.Maybe Core.Int,
    -- | Endpoint connection password.
    password :: Core.Maybe Types.Password,
    -- | Endpoint TCP port.
    port :: Core.Maybe Core.Int,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Core.Maybe Types.String,
    -- | Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance.
    --
    -- When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
    slotName :: Core.Maybe Types.String,
    -- | Endpoint connection user name.
    username :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostgreSQLSettings' value with any optional fields omitted.
mkPostgreSQLSettings ::
  PostgreSQLSettings
mkPostgreSQLSettings =
  PostgreSQLSettings'
    { afterConnectScript = Core.Nothing,
      captureDdls = Core.Nothing,
      databaseName = Core.Nothing,
      ddlArtifactsSchema = Core.Nothing,
      executeTimeout = Core.Nothing,
      failTasksOnLobTruncation = Core.Nothing,
      maxFileSize = Core.Nothing,
      password = Core.Nothing,
      port = Core.Nothing,
      serverName = Core.Nothing,
      slotName = Core.Nothing,
      username = Core.Nothing
    }

-- | For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role='replica'@
--
-- /Note:/ Consider using 'afterConnectScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsAfterConnectScript :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.String)
psqlsAfterConnectScript = Lens.field @"afterConnectScript"
{-# DEPRECATED psqlsAfterConnectScript "Use generic-lens or generic-optics with 'afterConnectScript' instead." #-}

-- | To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@ , you don't have to create tables or triggers on the source database.
--
-- /Note:/ Consider using 'captureDdls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsCaptureDdls :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Bool)
psqlsCaptureDdls = Lens.field @"captureDdls"
{-# DEPRECATED psqlsCaptureDdls "Use generic-lens or generic-optics with 'captureDdls' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsDatabaseName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.String)
psqlsDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED psqlsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
--
-- /Note:/ Consider using 'ddlArtifactsSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsDdlArtifactsSchema :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.String)
psqlsDdlArtifactsSchema = Lens.field @"ddlArtifactsSchema"
{-# DEPRECATED psqlsDdlArtifactsSchema "Use generic-lens or generic-optics with 'ddlArtifactsSchema' instead." #-}

-- | Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
--
-- /Note:/ Consider using 'executeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsExecuteTimeout :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
psqlsExecuteTimeout = Lens.field @"executeTimeout"
{-# DEPRECATED psqlsExecuteTimeout "Use generic-lens or generic-optics with 'executeTimeout' instead." #-}

-- | When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ .
--
-- If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
--
-- /Note:/ Consider using 'failTasksOnLobTruncation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsFailTasksOnLobTruncation :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Bool)
psqlsFailTasksOnLobTruncation = Lens.field @"failTasksOnLobTruncation"
{-# DEPRECATED psqlsFailTasksOnLobTruncation "Use generic-lens or generic-optics with 'failTasksOnLobTruncation' instead." #-}

-- | Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL.
--
-- Example: @maxFileSize=512@
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsMaxFileSize :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
psqlsMaxFileSize = Lens.field @"maxFileSize"
{-# DEPRECATED psqlsMaxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsPassword :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.Password)
psqlsPassword = Lens.field @"password"
{-# DEPRECATED psqlsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsPort :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
psqlsPort = Lens.field @"port"
{-# DEPRECATED psqlsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsServerName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.String)
psqlsServerName = Lens.field @"serverName"
{-# DEPRECATED psqlsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
--
-- /Note:/ Consider using 'slotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsSlotName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.String)
psqlsSlotName = Lens.field @"slotName"
{-# DEPRECATED psqlsSlotName "Use generic-lens or generic-optics with 'slotName' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsUsername :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.String)
psqlsUsername = Lens.field @"username"
{-# DEPRECATED psqlsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON PostgreSQLSettings where
  toJSON PostgreSQLSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("AfterConnectScript" Core..=) Core.<$> afterConnectScript,
            ("CaptureDdls" Core..=) Core.<$> captureDdls,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            ("DdlArtifactsSchema" Core..=) Core.<$> ddlArtifactsSchema,
            ("ExecuteTimeout" Core..=) Core.<$> executeTimeout,
            ("FailTasksOnLobTruncation" Core..=)
              Core.<$> failTasksOnLobTruncation,
            ("MaxFileSize" Core..=) Core.<$> maxFileSize,
            ("Password" Core..=) Core.<$> password,
            ("Port" Core..=) Core.<$> port,
            ("ServerName" Core..=) Core.<$> serverName,
            ("SlotName" Core..=) Core.<$> slotName,
            ("Username" Core..=) Core.<$> username
          ]
      )

instance Core.FromJSON PostgreSQLSettings where
  parseJSON =
    Core.withObject "PostgreSQLSettings" Core.$
      \x ->
        PostgreSQLSettings'
          Core.<$> (x Core..:? "AfterConnectScript")
          Core.<*> (x Core..:? "CaptureDdls")
          Core.<*> (x Core..:? "DatabaseName")
          Core.<*> (x Core..:? "DdlArtifactsSchema")
          Core.<*> (x Core..:? "ExecuteTimeout")
          Core.<*> (x Core..:? "FailTasksOnLobTruncation")
          Core.<*> (x Core..:? "MaxFileSize")
          Core.<*> (x Core..:? "Password")
          Core.<*> (x Core..:? "Port")
          Core.<*> (x Core..:? "ServerName")
          Core.<*> (x Core..:? "SlotName")
          Core.<*> (x Core..:? "Username")
