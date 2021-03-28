{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.PostgreSQLSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.PostgreSQLSettings
  ( PostgreSQLSettings (..)
  -- * Smart constructor
  , mkPostgreSQLSettings
  -- * Lenses
  , psqlsAfterConnectScript
  , psqlsCaptureDdls
  , psqlsDatabaseName
  , psqlsDdlArtifactsSchema
  , psqlsExecuteTimeout
  , psqlsFailTasksOnLobTruncation
  , psqlsMaxFileSize
  , psqlsPassword
  , psqlsPort
  , psqlsServerName
  , psqlsSlotName
  , psqlsUsername
  ) where

import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines a PostgreSQL endpoint.
--
-- /See:/ 'mkPostgreSQLSettings' smart constructor.
data PostgreSQLSettings = PostgreSQLSettings'
  { afterConnectScript :: Core.Maybe Core.Text
    -- ^ For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role='replica'@ 
  , captureDdls :: Core.Maybe Core.Bool
    -- ^ To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@ , you don't have to create tables or triggers on the source database.
  , databaseName :: Core.Maybe Core.Text
    -- ^ Database name for the endpoint.
  , ddlArtifactsSchema :: Core.Maybe Core.Text
    -- ^ The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@ 
  , executeTimeout :: Core.Maybe Core.Int
    -- ^ Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@ 
  , failTasksOnLobTruncation :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ .
--
-- If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
  , maxFileSize :: Core.Maybe Core.Int
    -- ^ Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL.
--
-- Example: @maxFileSize=512@ 
  , password :: Core.Maybe Types.Password
    -- ^ Endpoint connection password.
  , port :: Core.Maybe Core.Int
    -- ^ Endpoint TCP port.
  , serverName :: Core.Maybe Core.Text
    -- ^ Fully qualified domain name of the endpoint.
  , slotName :: Core.Maybe Core.Text
    -- ^ Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
  , username :: Core.Maybe Core.Text
    -- ^ Endpoint connection user name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostgreSQLSettings' value with any optional fields omitted.
mkPostgreSQLSettings
    :: PostgreSQLSettings
mkPostgreSQLSettings
  = PostgreSQLSettings'{afterConnectScript = Core.Nothing,
                        captureDdls = Core.Nothing, databaseName = Core.Nothing,
                        ddlArtifactsSchema = Core.Nothing, executeTimeout = Core.Nothing,
                        failTasksOnLobTruncation = Core.Nothing,
                        maxFileSize = Core.Nothing, password = Core.Nothing,
                        port = Core.Nothing, serverName = Core.Nothing,
                        slotName = Core.Nothing, username = Core.Nothing}

-- | For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role='replica'@ 
--
-- /Note:/ Consider using 'afterConnectScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsAfterConnectScript :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
psqlsAfterConnectScript = Lens.field @"afterConnectScript"
{-# INLINEABLE psqlsAfterConnectScript #-}
{-# DEPRECATED afterConnectScript "Use generic-lens or generic-optics with 'afterConnectScript' instead"  #-}

-- | To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@ , you don't have to create tables or triggers on the source database.
--
-- /Note:/ Consider using 'captureDdls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsCaptureDdls :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Bool)
psqlsCaptureDdls = Lens.field @"captureDdls"
{-# INLINEABLE psqlsCaptureDdls #-}
{-# DEPRECATED captureDdls "Use generic-lens or generic-optics with 'captureDdls' instead"  #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsDatabaseName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
psqlsDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE psqlsDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@ 
--
-- /Note:/ Consider using 'ddlArtifactsSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsDdlArtifactsSchema :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
psqlsDdlArtifactsSchema = Lens.field @"ddlArtifactsSchema"
{-# INLINEABLE psqlsDdlArtifactsSchema #-}
{-# DEPRECATED ddlArtifactsSchema "Use generic-lens or generic-optics with 'ddlArtifactsSchema' instead"  #-}

-- | Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@ 
--
-- /Note:/ Consider using 'executeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsExecuteTimeout :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
psqlsExecuteTimeout = Lens.field @"executeTimeout"
{-# INLINEABLE psqlsExecuteTimeout #-}
{-# DEPRECATED executeTimeout "Use generic-lens or generic-optics with 'executeTimeout' instead"  #-}

-- | When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ .
--
-- If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
--
-- /Note:/ Consider using 'failTasksOnLobTruncation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsFailTasksOnLobTruncation :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Bool)
psqlsFailTasksOnLobTruncation = Lens.field @"failTasksOnLobTruncation"
{-# INLINEABLE psqlsFailTasksOnLobTruncation #-}
{-# DEPRECATED failTasksOnLobTruncation "Use generic-lens or generic-optics with 'failTasksOnLobTruncation' instead"  #-}

-- | Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL.
--
-- Example: @maxFileSize=512@ 
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsMaxFileSize :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
psqlsMaxFileSize = Lens.field @"maxFileSize"
{-# INLINEABLE psqlsMaxFileSize #-}
{-# DEPRECATED maxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead"  #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsPassword :: Lens.Lens' PostgreSQLSettings (Core.Maybe Types.Password)
psqlsPassword = Lens.field @"password"
{-# INLINEABLE psqlsPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsPort :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Int)
psqlsPort = Lens.field @"port"
{-# INLINEABLE psqlsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsServerName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
psqlsServerName = Lens.field @"serverName"
{-# INLINEABLE psqlsServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
--
-- /Note:/ Consider using 'slotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsSlotName :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
psqlsSlotName = Lens.field @"slotName"
{-# INLINEABLE psqlsSlotName #-}
{-# DEPRECATED slotName "Use generic-lens or generic-optics with 'slotName' instead"  #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsUsername :: Lens.Lens' PostgreSQLSettings (Core.Maybe Core.Text)
psqlsUsername = Lens.field @"username"
{-# INLINEABLE psqlsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON PostgreSQLSettings where
        toJSON PostgreSQLSettings{..}
          = Core.object
              (Core.catMaybes
                 [("AfterConnectScript" Core..=) Core.<$> afterConnectScript,
                  ("CaptureDdls" Core..=) Core.<$> captureDdls,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("DdlArtifactsSchema" Core..=) Core.<$> ddlArtifactsSchema,
                  ("ExecuteTimeout" Core..=) Core.<$> executeTimeout,
                  ("FailTasksOnLobTruncation" Core..=) Core.<$>
                    failTasksOnLobTruncation,
                  ("MaxFileSize" Core..=) Core.<$> maxFileSize,
                  ("Password" Core..=) Core.<$> password,
                  ("Port" Core..=) Core.<$> port,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("SlotName" Core..=) Core.<$> slotName,
                  ("Username" Core..=) Core.<$> username])

instance Core.FromJSON PostgreSQLSettings where
        parseJSON
          = Core.withObject "PostgreSQLSettings" Core.$
              \ x ->
                PostgreSQLSettings' Core.<$>
                  (x Core..:? "AfterConnectScript") Core.<*> x Core..:? "CaptureDdls"
                    Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "DdlArtifactsSchema"
                    Core.<*> x Core..:? "ExecuteTimeout"
                    Core.<*> x Core..:? "FailTasksOnLobTruncation"
                    Core.<*> x Core..:? "MaxFileSize"
                    Core.<*> x Core..:? "Password"
                    Core.<*> x Core..:? "Port"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "SlotName"
                    Core.<*> x Core..:? "Username"
