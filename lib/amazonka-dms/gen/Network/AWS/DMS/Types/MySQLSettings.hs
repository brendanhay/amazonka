{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MySQLSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MySQLSettings
  ( MySQLSettings (..),

    -- * Smart constructor
    mkMySQLSettings,

    -- * Lenses
    msqlsAfterConnectScript,
    msqlsDatabaseName,
    msqlsEventsPollInterval,
    msqlsMaxFileSize,
    msqlsParallelLoadThreads,
    msqlsPassword,
    msqlsPort,
    msqlsServerName,
    msqlsServerTimezone,
    msqlsTargetDbType,
    msqlsUsername,
  )
where

import qualified Network.AWS.DMS.Types.AfterConnectScript as Types
import qualified Network.AWS.DMS.Types.DatabaseName as Types
import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.DMS.Types.ServerName as Types
import qualified Network.AWS.DMS.Types.ServerTimezone as Types
import qualified Network.AWS.DMS.Types.TargetDbType as Types
import qualified Network.AWS.DMS.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines a MySQL endpoint.
--
-- /See:/ 'mkMySQLSettings' smart constructor.
data MySQLSettings = MySQLSettings'
  { -- | Specifies a script to run immediately after AWS DMS connects to the endpoint. The migration task continues running regardless if the SQL statement succeeds or fails.
    afterConnectScript :: Core.Maybe Types.AfterConnectScript,
    -- | Database name for the endpoint.
    databaseName :: Core.Maybe Types.DatabaseName,
    -- | Specifies how often to check the binary log for new changes/events when the database is idle.
    --
    -- Example: @eventsPollInterval=5;@
    -- In the example, AWS DMS checks for changes in the binary logs every five seconds.
    eventsPollInterval :: Core.Maybe Core.Int,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer data to a MySQL-compatible database.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Core.Maybe Core.Int,
    -- | Improves performance when loading data into the MySQLcompatible target database. Specifies how many threads to use to load the data into the MySQL-compatible target database. Setting a large number of threads can have an adverse effect on database performance, because a separate connection is required for each thread.
    --
    -- Example: @parallelLoadThreads=1@
    parallelLoadThreads :: Core.Maybe Core.Int,
    -- | Endpoint connection password.
    password :: Core.Maybe Types.Password,
    -- | Endpoint TCP port.
    port :: Core.Maybe Core.Int,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Core.Maybe Types.ServerName,
    -- | Specifies the time zone for the source MySQL database.
    --
    -- Example: @serverTimezone=US/Pacific;@
    -- Note: Do not enclose time zones in single quotes.
    serverTimezone :: Core.Maybe Types.ServerTimezone,
    -- | Specifies where to migrate source tables on the target, either to a single database or multiple databases.
    --
    -- Example: @targetDbType=MULTIPLE_DATABASES@
    targetDbType :: Core.Maybe Types.TargetDbType,
    -- | Endpoint connection user name.
    username :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MySQLSettings' value with any optional fields omitted.
mkMySQLSettings ::
  MySQLSettings
mkMySQLSettings =
  MySQLSettings'
    { afterConnectScript = Core.Nothing,
      databaseName = Core.Nothing,
      eventsPollInterval = Core.Nothing,
      maxFileSize = Core.Nothing,
      parallelLoadThreads = Core.Nothing,
      password = Core.Nothing,
      port = Core.Nothing,
      serverName = Core.Nothing,
      serverTimezone = Core.Nothing,
      targetDbType = Core.Nothing,
      username = Core.Nothing
    }

-- | Specifies a script to run immediately after AWS DMS connects to the endpoint. The migration task continues running regardless if the SQL statement succeeds or fails.
--
-- /Note:/ Consider using 'afterConnectScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsAfterConnectScript :: Lens.Lens' MySQLSettings (Core.Maybe Types.AfterConnectScript)
msqlsAfterConnectScript = Lens.field @"afterConnectScript"
{-# DEPRECATED msqlsAfterConnectScript "Use generic-lens or generic-optics with 'afterConnectScript' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsDatabaseName :: Lens.Lens' MySQLSettings (Core.Maybe Types.DatabaseName)
msqlsDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED msqlsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Specifies how often to check the binary log for new changes/events when the database is idle.
--
-- Example: @eventsPollInterval=5;@
-- In the example, AWS DMS checks for changes in the binary logs every five seconds.
--
-- /Note:/ Consider using 'eventsPollInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsEventsPollInterval :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
msqlsEventsPollInterval = Lens.field @"eventsPollInterval"
{-# DEPRECATED msqlsEventsPollInterval "Use generic-lens or generic-optics with 'eventsPollInterval' instead." #-}

-- | Specifies the maximum size (in KB) of any .csv file used to transfer data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsMaxFileSize :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
msqlsMaxFileSize = Lens.field @"maxFileSize"
{-# DEPRECATED msqlsMaxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead." #-}

-- | Improves performance when loading data into the MySQLcompatible target database. Specifies how many threads to use to load the data into the MySQL-compatible target database. Setting a large number of threads can have an adverse effect on database performance, because a separate connection is required for each thread.
--
-- Example: @parallelLoadThreads=1@
--
-- /Note:/ Consider using 'parallelLoadThreads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsParallelLoadThreads :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
msqlsParallelLoadThreads = Lens.field @"parallelLoadThreads"
{-# DEPRECATED msqlsParallelLoadThreads "Use generic-lens or generic-optics with 'parallelLoadThreads' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsPassword :: Lens.Lens' MySQLSettings (Core.Maybe Types.Password)
msqlsPassword = Lens.field @"password"
{-# DEPRECATED msqlsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsPort :: Lens.Lens' MySQLSettings (Core.Maybe Core.Int)
msqlsPort = Lens.field @"port"
{-# DEPRECATED msqlsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsServerName :: Lens.Lens' MySQLSettings (Core.Maybe Types.ServerName)
msqlsServerName = Lens.field @"serverName"
{-# DEPRECATED msqlsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US/Pacific;@
-- Note: Do not enclose time zones in single quotes.
--
-- /Note:/ Consider using 'serverTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsServerTimezone :: Lens.Lens' MySQLSettings (Core.Maybe Types.ServerTimezone)
msqlsServerTimezone = Lens.field @"serverTimezone"
{-# DEPRECATED msqlsServerTimezone "Use generic-lens or generic-optics with 'serverTimezone' instead." #-}

-- | Specifies where to migrate source tables on the target, either to a single database or multiple databases.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
--
-- /Note:/ Consider using 'targetDbType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsTargetDbType :: Lens.Lens' MySQLSettings (Core.Maybe Types.TargetDbType)
msqlsTargetDbType = Lens.field @"targetDbType"
{-# DEPRECATED msqlsTargetDbType "Use generic-lens or generic-optics with 'targetDbType' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsUsername :: Lens.Lens' MySQLSettings (Core.Maybe Types.Username)
msqlsUsername = Lens.field @"username"
{-# DEPRECATED msqlsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON MySQLSettings where
  toJSON MySQLSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("AfterConnectScript" Core..=) Core.<$> afterConnectScript,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            ("EventsPollInterval" Core..=) Core.<$> eventsPollInterval,
            ("MaxFileSize" Core..=) Core.<$> maxFileSize,
            ("ParallelLoadThreads" Core..=) Core.<$> parallelLoadThreads,
            ("Password" Core..=) Core.<$> password,
            ("Port" Core..=) Core.<$> port,
            ("ServerName" Core..=) Core.<$> serverName,
            ("ServerTimezone" Core..=) Core.<$> serverTimezone,
            ("TargetDbType" Core..=) Core.<$> targetDbType,
            ("Username" Core..=) Core.<$> username
          ]
      )

instance Core.FromJSON MySQLSettings where
  parseJSON =
    Core.withObject "MySQLSettings" Core.$
      \x ->
        MySQLSettings'
          Core.<$> (x Core..:? "AfterConnectScript")
          Core.<*> (x Core..:? "DatabaseName")
          Core.<*> (x Core..:? "EventsPollInterval")
          Core.<*> (x Core..:? "MaxFileSize")
          Core.<*> (x Core..:? "ParallelLoadThreads")
          Core.<*> (x Core..:? "Password")
          Core.<*> (x Core..:? "Port")
          Core.<*> (x Core..:? "ServerName")
          Core.<*> (x Core..:? "ServerTimezone")
          Core.<*> (x Core..:? "TargetDbType")
          Core.<*> (x Core..:? "Username")
