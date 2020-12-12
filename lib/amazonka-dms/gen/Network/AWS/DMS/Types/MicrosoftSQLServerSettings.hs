{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MicrosoftSQLServerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MicrosoftSQLServerSettings
  ( MicrosoftSQLServerSettings (..),

    -- * Smart constructor
    mkMicrosoftSQLServerSettings,

    -- * Lenses
    msqlssBcpPacketSize,
    msqlssUseBcpFullLoad,
    msqlssServerName,
    msqlssUsername,
    msqlssSafeguardPolicy,
    msqlssPassword,
    msqlssDatabaseName,
    msqlssReadBackupOnly,
    msqlssControlTablesFileGroup,
    msqlssPort,
  )
where

import Network.AWS.DMS.Types.SafeguardPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines a Microsoft SQL Server endpoint.
--
-- /See:/ 'mkMicrosoftSQLServerSettings' smart constructor.
data MicrosoftSQLServerSettings = MicrosoftSQLServerSettings'
  { bcpPacketSize ::
      Lude.Maybe Lude.Int,
    useBcpFullLoad ::
      Lude.Maybe Lude.Bool,
    serverName :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    safeguardPolicy ::
      Lude.Maybe SafeguardPolicy,
    password ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    databaseName :: Lude.Maybe Lude.Text,
    readBackupOnly ::
      Lude.Maybe Lude.Bool,
    controlTablesFileGroup ::
      Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MicrosoftSQLServerSettings' with the minimum fields required to make a request.
--
-- * 'bcpPacketSize' - The maximum size of the packets (in bytes) used to transfer data using BCP.
-- * 'controlTablesFileGroup' - Specify a filegroup for the AWS DMS internal tables. When the replication task starts, all the internal AWS DMS control tables (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created on the specified filegroup.
-- * 'databaseName' - Database name for the endpoint.
-- * 'password' - Endpoint connection password.
-- * 'port' - Endpoint TCP port.
-- * 'readBackupOnly' - When this attribute is set to @Y@ , AWS DMS only reads changes from transaction log backups and doesn't read from the active transaction log file during ongoing replication. Setting this parameter to @Y@ enables you to control active transaction log file growth during full load and ongoing replication tasks. However, it can add some source latency to ongoing replication.
-- * 'safeguardPolicy' - Use this attribute to minimize the need to access the backup log and enable AWS DMS to prevent truncation using one of the following two methods.
--
-- /Start transactions in the database:/ This is the default method. When this method is used, AWS DMS prevents TLOG truncation by mimicking a transaction in the database. As long as such a transaction is open, changes that appear after the transaction started aren't truncated. If you need Microsoft Replication to be enabled in your database, then you must choose this method.
-- /Exclusively use sp_repldone within a single task/ : When this method is used, AWS DMS reads the changes and then uses sp_repldone to mark the TLOG transactions as ready for truncation. Although this method doesn't involve any transactional activities, it can only be used when Microsoft Replication isn't running. Also, when using this method, only one AWS DMS task can access the database at any given time. Therefore, if you need to run parallel AWS DMS tasks against the same database, use the default method.
-- * 'serverName' - Fully qualified domain name of the endpoint.
-- * 'useBcpFullLoad' - Use this to attribute to transfer data for full-load operations using BCP. When the target table contains an identity column that does not exist in the source table, you must disable the use BCP for loading table option.
-- * 'username' - Endpoint connection user name.
mkMicrosoftSQLServerSettings ::
  MicrosoftSQLServerSettings
mkMicrosoftSQLServerSettings =
  MicrosoftSQLServerSettings'
    { bcpPacketSize = Lude.Nothing,
      useBcpFullLoad = Lude.Nothing,
      serverName = Lude.Nothing,
      username = Lude.Nothing,
      safeguardPolicy = Lude.Nothing,
      password = Lude.Nothing,
      databaseName = Lude.Nothing,
      readBackupOnly = Lude.Nothing,
      controlTablesFileGroup = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The maximum size of the packets (in bytes) used to transfer data using BCP.
--
-- /Note:/ Consider using 'bcpPacketSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssBcpPacketSize :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Int)
msqlssBcpPacketSize = Lens.lens (bcpPacketSize :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Int) (\s a -> s {bcpPacketSize = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssBcpPacketSize "Use generic-lens or generic-optics with 'bcpPacketSize' instead." #-}

-- | Use this to attribute to transfer data for full-load operations using BCP. When the target table contains an identity column that does not exist in the source table, you must disable the use BCP for loading table option.
--
-- /Note:/ Consider using 'useBcpFullLoad' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssUseBcpFullLoad :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Bool)
msqlssUseBcpFullLoad = Lens.lens (useBcpFullLoad :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Bool) (\s a -> s {useBcpFullLoad = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssUseBcpFullLoad "Use generic-lens or generic-optics with 'useBcpFullLoad' instead." #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssServerName :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Text)
msqlssServerName = Lens.lens (serverName :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssUsername :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Text)
msqlssUsername = Lens.lens (username :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Use this attribute to minimize the need to access the backup log and enable AWS DMS to prevent truncation using one of the following two methods.
--
-- /Start transactions in the database:/ This is the default method. When this method is used, AWS DMS prevents TLOG truncation by mimicking a transaction in the database. As long as such a transaction is open, changes that appear after the transaction started aren't truncated. If you need Microsoft Replication to be enabled in your database, then you must choose this method.
-- /Exclusively use sp_repldone within a single task/ : When this method is used, AWS DMS reads the changes and then uses sp_repldone to mark the TLOG transactions as ready for truncation. Although this method doesn't involve any transactional activities, it can only be used when Microsoft Replication isn't running. Also, when using this method, only one AWS DMS task can access the database at any given time. Therefore, if you need to run parallel AWS DMS tasks against the same database, use the default method.
--
-- /Note:/ Consider using 'safeguardPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssSafeguardPolicy :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe SafeguardPolicy)
msqlssSafeguardPolicy = Lens.lens (safeguardPolicy :: MicrosoftSQLServerSettings -> Lude.Maybe SafeguardPolicy) (\s a -> s {safeguardPolicy = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssSafeguardPolicy "Use generic-lens or generic-optics with 'safeguardPolicy' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssPassword :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
msqlssPassword = Lens.lens (password :: MicrosoftSQLServerSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssDatabaseName :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Text)
msqlssDatabaseName = Lens.lens (databaseName :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | When this attribute is set to @Y@ , AWS DMS only reads changes from transaction log backups and doesn't read from the active transaction log file during ongoing replication. Setting this parameter to @Y@ enables you to control active transaction log file growth during full load and ongoing replication tasks. However, it can add some source latency to ongoing replication.
--
-- /Note:/ Consider using 'readBackupOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssReadBackupOnly :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Bool)
msqlssReadBackupOnly = Lens.lens (readBackupOnly :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Bool) (\s a -> s {readBackupOnly = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssReadBackupOnly "Use generic-lens or generic-optics with 'readBackupOnly' instead." #-}

-- | Specify a filegroup for the AWS DMS internal tables. When the replication task starts, all the internal AWS DMS control tables (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created on the specified filegroup.
--
-- /Note:/ Consider using 'controlTablesFileGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssControlTablesFileGroup :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Text)
msqlssControlTablesFileGroup = Lens.lens (controlTablesFileGroup :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Text) (\s a -> s {controlTablesFileGroup = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssControlTablesFileGroup "Use generic-lens or generic-optics with 'controlTablesFileGroup' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssPort :: Lens.Lens' MicrosoftSQLServerSettings (Lude.Maybe Lude.Int)
msqlssPort = Lens.lens (port :: MicrosoftSQLServerSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: MicrosoftSQLServerSettings)
{-# DEPRECATED msqlssPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON MicrosoftSQLServerSettings where
  parseJSON =
    Lude.withObject
      "MicrosoftSQLServerSettings"
      ( \x ->
          MicrosoftSQLServerSettings'
            Lude.<$> (x Lude..:? "BcpPacketSize")
            Lude.<*> (x Lude..:? "UseBcpFullLoad")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "SafeguardPolicy")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "ReadBackupOnly")
            Lude.<*> (x Lude..:? "ControlTablesFileGroup")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON MicrosoftSQLServerSettings where
  toJSON MicrosoftSQLServerSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BcpPacketSize" Lude..=) Lude.<$> bcpPacketSize,
            ("UseBcpFullLoad" Lude..=) Lude.<$> useBcpFullLoad,
            ("ServerName" Lude..=) Lude.<$> serverName,
            ("Username" Lude..=) Lude.<$> username,
            ("SafeguardPolicy" Lude..=) Lude.<$> safeguardPolicy,
            ("Password" Lude..=) Lude.<$> password,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("ReadBackupOnly" Lude..=) Lude.<$> readBackupOnly,
            ("ControlTablesFileGroup" Lude..=) Lude.<$> controlTablesFileGroup,
            ("Port" Lude..=) Lude.<$> port
          ]
      )
