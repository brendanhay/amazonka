{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
  ( ConnectionPoolConfigurationInfo (..),

    -- * Smart constructor
    mkConnectionPoolConfigurationInfo,

    -- * Lenses
    cpciConnectionBorrowTimeout,
    cpciInitQuery,
    cpciMaxConnectionsPercent,
    cpciMaxIdleConnectionsPercent,
    cpciSessionPinningFilters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Displays the settings that control the size and behavior of the connection pool associated with a @DBProxyTarget@ .
--
-- /See:/ 'mkConnectionPoolConfigurationInfo' smart constructor.
data ConnectionPoolConfigurationInfo = ConnectionPoolConfigurationInfo'
  { -- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
    connectionBorrowTimeout :: Core.Maybe Core.Int,
    -- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. This setting is empty by default. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
    initQuery :: Core.Maybe Types.String,
    -- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
    maxConnectionsPercent :: Core.Maybe Core.Int,
    -- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
    maxIdleConnectionsPercent :: Core.Maybe Core.Int,
    -- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@ .
    sessionPinningFilters :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionPoolConfigurationInfo' value with any optional fields omitted.
mkConnectionPoolConfigurationInfo ::
  ConnectionPoolConfigurationInfo
mkConnectionPoolConfigurationInfo =
  ConnectionPoolConfigurationInfo'
    { connectionBorrowTimeout =
        Core.Nothing,
      initQuery = Core.Nothing,
      maxConnectionsPercent = Core.Nothing,
      maxIdleConnectionsPercent = Core.Nothing,
      sessionPinningFilters = Core.Nothing
    }

-- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
--
-- /Note:/ Consider using 'connectionBorrowTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciConnectionBorrowTimeout :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Core.Int)
cpciConnectionBorrowTimeout = Lens.field @"connectionBorrowTimeout"
{-# DEPRECATED cpciConnectionBorrowTimeout "Use generic-lens or generic-optics with 'connectionBorrowTimeout' instead." #-}

-- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. This setting is empty by default. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
--
-- /Note:/ Consider using 'initQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciInitQuery :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Types.String)
cpciInitQuery = Lens.field @"initQuery"
{-# DEPRECATED cpciInitQuery "Use generic-lens or generic-optics with 'initQuery' instead." #-}

-- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- /Note:/ Consider using 'maxConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciMaxConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Core.Int)
cpciMaxConnectionsPercent = Lens.field @"maxConnectionsPercent"
{-# DEPRECATED cpciMaxConnectionsPercent "Use generic-lens or generic-optics with 'maxConnectionsPercent' instead." #-}

-- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- /Note:/ Consider using 'maxIdleConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciMaxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Core.Int)
cpciMaxIdleConnectionsPercent = Lens.field @"maxIdleConnectionsPercent"
{-# DEPRECATED cpciMaxIdleConnectionsPercent "Use generic-lens or generic-optics with 'maxIdleConnectionsPercent' instead." #-}

-- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@ .
--
-- /Note:/ Consider using 'sessionPinningFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciSessionPinningFilters :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe [Types.String])
cpciSessionPinningFilters = Lens.field @"sessionPinningFilters"
{-# DEPRECATED cpciSessionPinningFilters "Use generic-lens or generic-optics with 'sessionPinningFilters' instead." #-}

instance Core.FromXML ConnectionPoolConfigurationInfo where
  parseXML x =
    ConnectionPoolConfigurationInfo'
      Core.<$> (x Core..@? "ConnectionBorrowTimeout")
      Core.<*> (x Core..@? "InitQuery")
      Core.<*> (x Core..@? "MaxConnectionsPercent")
      Core.<*> (x Core..@? "MaxIdleConnectionsPercent")
      Core.<*> ( x Core..@? "SessionPinningFilters"
                   Core..<@> Core.parseXMLList "member"
               )
