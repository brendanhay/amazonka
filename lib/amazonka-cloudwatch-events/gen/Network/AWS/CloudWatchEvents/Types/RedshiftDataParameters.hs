{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
  ( RedshiftDataParameters (..)
  -- * Smart constructor
  , mkRedshiftDataParameters
  -- * Lenses
  , rdpDatabase
  , rdpSql
  , rdpDbUser
  , rdpSecretManagerArn
  , rdpStatementName
  , rdpWithEvent
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.Database as Types
import qualified Network.AWS.CloudWatchEvents.Types.DbUser as Types
import qualified Network.AWS.CloudWatchEvents.Types.SecretManagerArn as Types
import qualified Network.AWS.CloudWatchEvents.Types.Sql as Types
import qualified Network.AWS.CloudWatchEvents.Types.StatementName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | These are custom parameters to be used when the target is a Redshift cluster to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
--
-- /See:/ 'mkRedshiftDataParameters' smart constructor.
data RedshiftDataParameters = RedshiftDataParameters'
  { database :: Types.Database
    -- ^ The name of the database. Required when authenticating using temporary credentials.
  , sql :: Types.Sql
    -- ^ The SQL statement text to run.
  , dbUser :: Core.Maybe Types.DbUser
    -- ^ The database user name. Required when authenticating using temporary credentials.
  , secretManagerArn :: Core.Maybe Types.SecretManagerArn
    -- ^ The name or ARN of the secret that enables access to the database. Required when authenticating using AWS Secrets Manager.
  , statementName :: Core.Maybe Types.StatementName
    -- ^ The name of the SQL statement. You can name the SQL statement when you create it to identify the query.
  , withEvent :: Core.Maybe Core.Bool
    -- ^ Indicates whether to send an event back to EventBridge after the SQL statement runs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftDataParameters' value with any optional fields omitted.
mkRedshiftDataParameters
    :: Types.Database -- ^ 'database'
    -> Types.Sql -- ^ 'sql'
    -> RedshiftDataParameters
mkRedshiftDataParameters database sql
  = RedshiftDataParameters'{database, sql, dbUser = Core.Nothing,
                            secretManagerArn = Core.Nothing, statementName = Core.Nothing,
                            withEvent = Core.Nothing}

-- | The name of the database. Required when authenticating using temporary credentials.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDatabase :: Lens.Lens' RedshiftDataParameters Types.Database
rdpDatabase = Lens.field @"database"
{-# INLINEABLE rdpDatabase #-}
{-# DEPRECATED database "Use generic-lens or generic-optics with 'database' instead"  #-}

-- | The SQL statement text to run.
--
-- /Note:/ Consider using 'sql' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpSql :: Lens.Lens' RedshiftDataParameters Types.Sql
rdpSql = Lens.field @"sql"
{-# INLINEABLE rdpSql #-}
{-# DEPRECATED sql "Use generic-lens or generic-optics with 'sql' instead"  #-}

-- | The database user name. Required when authenticating using temporary credentials.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDbUser :: Lens.Lens' RedshiftDataParameters (Core.Maybe Types.DbUser)
rdpDbUser = Lens.field @"dbUser"
{-# INLINEABLE rdpDbUser #-}
{-# DEPRECATED dbUser "Use generic-lens or generic-optics with 'dbUser' instead"  #-}

-- | The name or ARN of the secret that enables access to the database. Required when authenticating using AWS Secrets Manager.
--
-- /Note:/ Consider using 'secretManagerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpSecretManagerArn :: Lens.Lens' RedshiftDataParameters (Core.Maybe Types.SecretManagerArn)
rdpSecretManagerArn = Lens.field @"secretManagerArn"
{-# INLINEABLE rdpSecretManagerArn #-}
{-# DEPRECATED secretManagerArn "Use generic-lens or generic-optics with 'secretManagerArn' instead"  #-}

-- | The name of the SQL statement. You can name the SQL statement when you create it to identify the query.
--
-- /Note:/ Consider using 'statementName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpStatementName :: Lens.Lens' RedshiftDataParameters (Core.Maybe Types.StatementName)
rdpStatementName = Lens.field @"statementName"
{-# INLINEABLE rdpStatementName #-}
{-# DEPRECATED statementName "Use generic-lens or generic-optics with 'statementName' instead"  #-}

-- | Indicates whether to send an event back to EventBridge after the SQL statement runs.
--
-- /Note:/ Consider using 'withEvent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpWithEvent :: Lens.Lens' RedshiftDataParameters (Core.Maybe Core.Bool)
rdpWithEvent = Lens.field @"withEvent"
{-# INLINEABLE rdpWithEvent #-}
{-# DEPRECATED withEvent "Use generic-lens or generic-optics with 'withEvent' instead"  #-}

instance Core.FromJSON RedshiftDataParameters where
        toJSON RedshiftDataParameters{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Database" Core..= database),
                  Core.Just ("Sql" Core..= sql), ("DbUser" Core..=) Core.<$> dbUser,
                  ("SecretManagerArn" Core..=) Core.<$> secretManagerArn,
                  ("StatementName" Core..=) Core.<$> statementName,
                  ("WithEvent" Core..=) Core.<$> withEvent])

instance Core.FromJSON RedshiftDataParameters where
        parseJSON
          = Core.withObject "RedshiftDataParameters" Core.$
              \ x ->
                RedshiftDataParameters' Core.<$>
                  (x Core..: "Database") Core.<*> x Core..: "Sql" Core.<*>
                    x Core..:? "DbUser"
                    Core.<*> x Core..:? "SecretManagerArn"
                    Core.<*> x Core..:? "StatementName"
                    Core.<*> x Core..:? "WithEvent"
