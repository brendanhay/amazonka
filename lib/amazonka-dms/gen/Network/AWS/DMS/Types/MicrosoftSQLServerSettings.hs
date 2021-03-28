{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MicrosoftSQLServerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.MicrosoftSQLServerSettings
  ( MicrosoftSQLServerSettings (..)
  -- * Smart constructor
  , mkMicrosoftSQLServerSettings
  -- * Lenses
  , msqlssBcpPacketSize
  , msqlssControlTablesFileGroup
  , msqlssDatabaseName
  , msqlssPassword
  , msqlssPort
  , msqlssReadBackupOnly
  , msqlssSafeguardPolicy
  , msqlssServerName
  , msqlssUseBcpFullLoad
  , msqlssUsername
  ) where

import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.DMS.Types.SafeguardPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines a Microsoft SQL Server endpoint.
--
-- /See:/ 'mkMicrosoftSQLServerSettings' smart constructor.
data MicrosoftSQLServerSettings = MicrosoftSQLServerSettings'
  { bcpPacketSize :: Core.Maybe Core.Int
    -- ^ The maximum size of the packets (in bytes) used to transfer data using BCP.
  , controlTablesFileGroup :: Core.Maybe Core.Text
    -- ^ Specify a filegroup for the AWS DMS internal tables. When the replication task starts, all the internal AWS DMS control tables (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created on the specified filegroup.
  , databaseName :: Core.Maybe Core.Text
    -- ^ Database name for the endpoint.
  , password :: Core.Maybe Types.Password
    -- ^ Endpoint connection password.
  , port :: Core.Maybe Core.Int
    -- ^ Endpoint TCP port.
  , readBackupOnly :: Core.Maybe Core.Bool
    -- ^ When this attribute is set to @Y@ , AWS DMS only reads changes from transaction log backups and doesn't read from the active transaction log file during ongoing replication. Setting this parameter to @Y@ enables you to control active transaction log file growth during full load and ongoing replication tasks. However, it can add some source latency to ongoing replication.
  , safeguardPolicy :: Core.Maybe Types.SafeguardPolicy
    -- ^ Use this attribute to minimize the need to access the backup log and enable AWS DMS to prevent truncation using one of the following two methods.
--
-- /Start transactions in the database:/ This is the default method. When this method is used, AWS DMS prevents TLOG truncation by mimicking a transaction in the database. As long as such a transaction is open, changes that appear after the transaction started aren't truncated. If you need Microsoft Replication to be enabled in your database, then you must choose this method.
-- /Exclusively use sp_repldone within a single task/ : When this method is used, AWS DMS reads the changes and then uses sp_repldone to mark the TLOG transactions as ready for truncation. Although this method doesn't involve any transactional activities, it can only be used when Microsoft Replication isn't running. Also, when using this method, only one AWS DMS task can access the database at any given time. Therefore, if you need to run parallel AWS DMS tasks against the same database, use the default method.
  , serverName :: Core.Maybe Core.Text
    -- ^ Fully qualified domain name of the endpoint.
  , useBcpFullLoad :: Core.Maybe Core.Bool
    -- ^ Use this to attribute to transfer data for full-load operations using BCP. When the target table contains an identity column that does not exist in the source table, you must disable the use BCP for loading table option.
  , username :: Core.Maybe Core.Text
    -- ^ Endpoint connection user name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MicrosoftSQLServerSettings' value with any optional fields omitted.
mkMicrosoftSQLServerSettings
    :: MicrosoftSQLServerSettings
mkMicrosoftSQLServerSettings
  = MicrosoftSQLServerSettings'{bcpPacketSize = Core.Nothing,
                                controlTablesFileGroup = Core.Nothing, databaseName = Core.Nothing,
                                password = Core.Nothing, port = Core.Nothing,
                                readBackupOnly = Core.Nothing, safeguardPolicy = Core.Nothing,
                                serverName = Core.Nothing, useBcpFullLoad = Core.Nothing,
                                username = Core.Nothing}

-- | The maximum size of the packets (in bytes) used to transfer data using BCP.
--
-- /Note:/ Consider using 'bcpPacketSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssBcpPacketSize :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Int)
msqlssBcpPacketSize = Lens.field @"bcpPacketSize"
{-# INLINEABLE msqlssBcpPacketSize #-}
{-# DEPRECATED bcpPacketSize "Use generic-lens or generic-optics with 'bcpPacketSize' instead"  #-}

-- | Specify a filegroup for the AWS DMS internal tables. When the replication task starts, all the internal AWS DMS control tables (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created on the specified filegroup.
--
-- /Note:/ Consider using 'controlTablesFileGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssControlTablesFileGroup :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Text)
msqlssControlTablesFileGroup = Lens.field @"controlTablesFileGroup"
{-# INLINEABLE msqlssControlTablesFileGroup #-}
{-# DEPRECATED controlTablesFileGroup "Use generic-lens or generic-optics with 'controlTablesFileGroup' instead"  #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssDatabaseName :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Text)
msqlssDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE msqlssDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssPassword :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Types.Password)
msqlssPassword = Lens.field @"password"
{-# INLINEABLE msqlssPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssPort :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Int)
msqlssPort = Lens.field @"port"
{-# INLINEABLE msqlssPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | When this attribute is set to @Y@ , AWS DMS only reads changes from transaction log backups and doesn't read from the active transaction log file during ongoing replication. Setting this parameter to @Y@ enables you to control active transaction log file growth during full load and ongoing replication tasks. However, it can add some source latency to ongoing replication.
--
-- /Note:/ Consider using 'readBackupOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssReadBackupOnly :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Bool)
msqlssReadBackupOnly = Lens.field @"readBackupOnly"
{-# INLINEABLE msqlssReadBackupOnly #-}
{-# DEPRECATED readBackupOnly "Use generic-lens or generic-optics with 'readBackupOnly' instead"  #-}

-- | Use this attribute to minimize the need to access the backup log and enable AWS DMS to prevent truncation using one of the following two methods.
--
-- /Start transactions in the database:/ This is the default method. When this method is used, AWS DMS prevents TLOG truncation by mimicking a transaction in the database. As long as such a transaction is open, changes that appear after the transaction started aren't truncated. If you need Microsoft Replication to be enabled in your database, then you must choose this method.
-- /Exclusively use sp_repldone within a single task/ : When this method is used, AWS DMS reads the changes and then uses sp_repldone to mark the TLOG transactions as ready for truncation. Although this method doesn't involve any transactional activities, it can only be used when Microsoft Replication isn't running. Also, when using this method, only one AWS DMS task can access the database at any given time. Therefore, if you need to run parallel AWS DMS tasks against the same database, use the default method.
--
-- /Note:/ Consider using 'safeguardPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssSafeguardPolicy :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Types.SafeguardPolicy)
msqlssSafeguardPolicy = Lens.field @"safeguardPolicy"
{-# INLINEABLE msqlssSafeguardPolicy #-}
{-# DEPRECATED safeguardPolicy "Use generic-lens or generic-optics with 'safeguardPolicy' instead"  #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssServerName :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Text)
msqlssServerName = Lens.field @"serverName"
{-# INLINEABLE msqlssServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | Use this to attribute to transfer data for full-load operations using BCP. When the target table contains an identity column that does not exist in the source table, you must disable the use BCP for loading table option.
--
-- /Note:/ Consider using 'useBcpFullLoad' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssUseBcpFullLoad :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Bool)
msqlssUseBcpFullLoad = Lens.field @"useBcpFullLoad"
{-# INLINEABLE msqlssUseBcpFullLoad #-}
{-# DEPRECATED useBcpFullLoad "Use generic-lens or generic-optics with 'useBcpFullLoad' instead"  #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msqlssUsername :: Lens.Lens' MicrosoftSQLServerSettings (Core.Maybe Core.Text)
msqlssUsername = Lens.field @"username"
{-# INLINEABLE msqlssUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON MicrosoftSQLServerSettings where
        toJSON MicrosoftSQLServerSettings{..}
          = Core.object
              (Core.catMaybes
                 [("BcpPacketSize" Core..=) Core.<$> bcpPacketSize,
                  ("ControlTablesFileGroup" Core..=) Core.<$> controlTablesFileGroup,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("Password" Core..=) Core.<$> password,
                  ("Port" Core..=) Core.<$> port,
                  ("ReadBackupOnly" Core..=) Core.<$> readBackupOnly,
                  ("SafeguardPolicy" Core..=) Core.<$> safeguardPolicy,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("UseBcpFullLoad" Core..=) Core.<$> useBcpFullLoad,
                  ("Username" Core..=) Core.<$> username])

instance Core.FromJSON MicrosoftSQLServerSettings where
        parseJSON
          = Core.withObject "MicrosoftSQLServerSettings" Core.$
              \ x ->
                MicrosoftSQLServerSettings' Core.<$>
                  (x Core..:? "BcpPacketSize") Core.<*>
                    x Core..:? "ControlTablesFileGroup"
                    Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "Password"
                    Core.<*> x Core..:? "Port"
                    Core.<*> x Core..:? "ReadBackupOnly"
                    Core.<*> x Core..:? "SafeguardPolicy"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "UseBcpFullLoad"
                    Core.<*> x Core..:? "Username"
