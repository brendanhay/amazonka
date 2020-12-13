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
    msqlsMaxFileSize,
    msqlsTargetDBType,
    msqlsServerName,
    msqlsParallelLoadThreads,
    msqlsUsername,
    msqlsPassword,
    msqlsEventsPollInterval,
    msqlsDatabaseName,
    msqlsAfterConnectScript,
    msqlsServerTimezone,
    msqlsPort,
  )
where

import Network.AWS.DMS.Types.TargetDBType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines a MySQL endpoint.
--
-- /See:/ 'mkMySQLSettings' smart constructor.
data MySQLSettings = MySQLSettings'
  { -- | Specifies the maximum size (in KB) of any .csv file used to transfer data to a MySQL-compatible database.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Lude.Maybe Lude.Int,
    -- | Specifies where to migrate source tables on the target, either to a single database or multiple databases.
    --
    -- Example: @targetDbType=MULTIPLE_DATABASES@
    targetDBType :: Lude.Maybe TargetDBType,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Lude.Maybe Lude.Text,
    -- | Improves performance when loading data into the MySQLcompatible target database. Specifies how many threads to use to load the data into the MySQL-compatible target database. Setting a large number of threads can have an adverse effect on database performance, because a separate connection is required for each thread.
    --
    -- Example: @parallelLoadThreads=1@
    parallelLoadThreads :: Lude.Maybe Lude.Int,
    -- | Endpoint connection user name.
    username :: Lude.Maybe Lude.Text,
    -- | Endpoint connection password.
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Specifies how often to check the binary log for new changes/events when the database is idle.
    --
    -- Example: @eventsPollInterval=5;@
    -- In the example, AWS DMS checks for changes in the binary logs every five seconds.
    eventsPollInterval :: Lude.Maybe Lude.Int,
    -- | Database name for the endpoint.
    databaseName :: Lude.Maybe Lude.Text,
    -- | Specifies a script to run immediately after AWS DMS connects to the endpoint. The migration task continues running regardless if the SQL statement succeeds or fails.
    afterConnectScript :: Lude.Maybe Lude.Text,
    -- | Specifies the time zone for the source MySQL database.
    --
    -- Example: @serverTimezone=US/Pacific;@
    -- Note: Do not enclose time zones in single quotes.
    serverTimezone :: Lude.Maybe Lude.Text,
    -- | Endpoint TCP port.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MySQLSettings' with the minimum fields required to make a request.
--
-- * 'maxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
-- * 'targetDBType' - Specifies where to migrate source tables on the target, either to a single database or multiple databases.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
-- * 'serverName' - Fully qualified domain name of the endpoint.
-- * 'parallelLoadThreads' - Improves performance when loading data into the MySQLcompatible target database. Specifies how many threads to use to load the data into the MySQL-compatible target database. Setting a large number of threads can have an adverse effect on database performance, because a separate connection is required for each thread.
--
-- Example: @parallelLoadThreads=1@
-- * 'username' - Endpoint connection user name.
-- * 'password' - Endpoint connection password.
-- * 'eventsPollInterval' - Specifies how often to check the binary log for new changes/events when the database is idle.
--
-- Example: @eventsPollInterval=5;@
-- In the example, AWS DMS checks for changes in the binary logs every five seconds.
-- * 'databaseName' - Database name for the endpoint.
-- * 'afterConnectScript' - Specifies a script to run immediately after AWS DMS connects to the endpoint. The migration task continues running regardless if the SQL statement succeeds or fails.
-- * 'serverTimezone' - Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US/Pacific;@
-- Note: Do not enclose time zones in single quotes.
-- * 'port' - Endpoint TCP port.
mkMySQLSettings ::
  MySQLSettings
mkMySQLSettings =
  MySQLSettings'
    { maxFileSize = Lude.Nothing,
      targetDBType = Lude.Nothing,
      serverName = Lude.Nothing,
      parallelLoadThreads = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing,
      eventsPollInterval = Lude.Nothing,
      databaseName = Lude.Nothing,
      afterConnectScript = Lude.Nothing,
      serverTimezone = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Specifies the maximum size (in KB) of any .csv file used to transfer data to a MySQL-compatible database.
--
-- Example: @maxFileSize=512@
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsMaxFileSize :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Int)
msqlsMaxFileSize = Lens.lens (maxFileSize :: MySQLSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxFileSize = a} :: MySQLSettings)
{-# DEPRECATED msqlsMaxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead." #-}

-- | Specifies where to migrate source tables on the target, either to a single database or multiple databases.
--
-- Example: @targetDbType=MULTIPLE_DATABASES@
--
-- /Note:/ Consider using 'targetDBType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsTargetDBType :: Lens.Lens' MySQLSettings (Lude.Maybe TargetDBType)
msqlsTargetDBType = Lens.lens (targetDBType :: MySQLSettings -> Lude.Maybe TargetDBType) (\s a -> s {targetDBType = a} :: MySQLSettings)
{-# DEPRECATED msqlsTargetDBType "Use generic-lens or generic-optics with 'targetDBType' instead." #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsServerName :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Text)
msqlsServerName = Lens.lens (serverName :: MySQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: MySQLSettings)
{-# DEPRECATED msqlsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Improves performance when loading data into the MySQLcompatible target database. Specifies how many threads to use to load the data into the MySQL-compatible target database. Setting a large number of threads can have an adverse effect on database performance, because a separate connection is required for each thread.
--
-- Example: @parallelLoadThreads=1@
--
-- /Note:/ Consider using 'parallelLoadThreads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsParallelLoadThreads :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Int)
msqlsParallelLoadThreads = Lens.lens (parallelLoadThreads :: MySQLSettings -> Lude.Maybe Lude.Int) (\s a -> s {parallelLoadThreads = a} :: MySQLSettings)
{-# DEPRECATED msqlsParallelLoadThreads "Use generic-lens or generic-optics with 'parallelLoadThreads' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsUsername :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Text)
msqlsUsername = Lens.lens (username :: MySQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: MySQLSettings)
{-# DEPRECATED msqlsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsPassword :: Lens.Lens' MySQLSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
msqlsPassword = Lens.lens (password :: MySQLSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: MySQLSettings)
{-# DEPRECATED msqlsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Specifies how often to check the binary log for new changes/events when the database is idle.
--
-- Example: @eventsPollInterval=5;@
-- In the example, AWS DMS checks for changes in the binary logs every five seconds.
--
-- /Note:/ Consider using 'eventsPollInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsEventsPollInterval :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Int)
msqlsEventsPollInterval = Lens.lens (eventsPollInterval :: MySQLSettings -> Lude.Maybe Lude.Int) (\s a -> s {eventsPollInterval = a} :: MySQLSettings)
{-# DEPRECATED msqlsEventsPollInterval "Use generic-lens or generic-optics with 'eventsPollInterval' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsDatabaseName :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Text)
msqlsDatabaseName = Lens.lens (databaseName :: MySQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: MySQLSettings)
{-# DEPRECATED msqlsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Specifies a script to run immediately after AWS DMS connects to the endpoint. The migration task continues running regardless if the SQL statement succeeds or fails.
--
-- /Note:/ Consider using 'afterConnectScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsAfterConnectScript :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Text)
msqlsAfterConnectScript = Lens.lens (afterConnectScript :: MySQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {afterConnectScript = a} :: MySQLSettings)
{-# DEPRECATED msqlsAfterConnectScript "Use generic-lens or generic-optics with 'afterConnectScript' instead." #-}

-- | Specifies the time zone for the source MySQL database.
--
-- Example: @serverTimezone=US/Pacific;@
-- Note: Do not enclose time zones in single quotes.
--
-- /Note:/ Consider using 'serverTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsServerTimezone :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Text)
msqlsServerTimezone = Lens.lens (serverTimezone :: MySQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverTimezone = a} :: MySQLSettings)
{-# DEPRECATED msqlsServerTimezone "Use generic-lens or generic-optics with 'serverTimezone' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlsPort :: Lens.Lens' MySQLSettings (Lude.Maybe Lude.Int)
msqlsPort = Lens.lens (port :: MySQLSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: MySQLSettings)
{-# DEPRECATED msqlsPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON MySQLSettings where
  parseJSON =
    Lude.withObject
      "MySQLSettings"
      ( \x ->
          MySQLSettings'
            Lude.<$> (x Lude..:? "MaxFileSize")
            Lude.<*> (x Lude..:? "TargetDbType")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "ParallelLoadThreads")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "EventsPollInterval")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "AfterConnectScript")
            Lude.<*> (x Lude..:? "ServerTimezone")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON MySQLSettings where
  toJSON MySQLSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxFileSize" Lude..=) Lude.<$> maxFileSize,
            ("TargetDbType" Lude..=) Lude.<$> targetDBType,
            ("ServerName" Lude..=) Lude.<$> serverName,
            ("ParallelLoadThreads" Lude..=) Lude.<$> parallelLoadThreads,
            ("Username" Lude..=) Lude.<$> username,
            ("Password" Lude..=) Lude.<$> password,
            ("EventsPollInterval" Lude..=) Lude.<$> eventsPollInterval,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("AfterConnectScript" Lude..=) Lude.<$> afterConnectScript,
            ("ServerTimezone" Lude..=) Lude.<$> serverTimezone,
            ("Port" Lude..=) Lude.<$> port
          ]
      )
