{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ConnectionPoolConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ConnectionPoolConfiguration
  ( ConnectionPoolConfiguration (..),

    -- * Smart constructor
    mkConnectionPoolConfiguration,

    -- * Lenses
    cpcConnectionBorrowTimeout,
    cpcInitQuery,
    cpcMaxConnectionsPercent,
    cpcMaxIdleConnectionsPercent,
    cpcSessionPinningFilters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Specifies the settings that control the size and behavior of the connection pool associated with a @DBProxyTargetGroup@ .
--
-- /See:/ 'mkConnectionPoolConfiguration' smart constructor.
data ConnectionPoolConfiguration = ConnectionPoolConfiguration'
  { -- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
    --
    -- Default: 120
    -- Constraints: between 1 and 3600, or 0 representing unlimited
    connectionBorrowTimeout :: Core.Maybe Core.Int,
    -- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
    --
    -- Default: no initialization query
    initQuery :: Core.Maybe Types.String,
    -- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
    --
    -- Default: 100
    -- Constraints: between 1 and 100
    maxConnectionsPercent :: Core.Maybe Core.Int,
    -- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
    --
    -- Default: 50
    -- Constraints: between 0 and @MaxConnectionsPercent@
    maxIdleConnectionsPercent :: Core.Maybe Core.Int,
    -- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior.
    --
    -- Default: no session pinning filters
    sessionPinningFilters :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionPoolConfiguration' value with any optional fields omitted.
mkConnectionPoolConfiguration ::
  ConnectionPoolConfiguration
mkConnectionPoolConfiguration =
  ConnectionPoolConfiguration'
    { connectionBorrowTimeout =
        Core.Nothing,
      initQuery = Core.Nothing,
      maxConnectionsPercent = Core.Nothing,
      maxIdleConnectionsPercent = Core.Nothing,
      sessionPinningFilters = Core.Nothing
    }

-- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
--
-- Default: 120
-- Constraints: between 1 and 3600, or 0 representing unlimited
--
-- /Note:/ Consider using 'connectionBorrowTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcConnectionBorrowTimeout :: Lens.Lens' ConnectionPoolConfiguration (Core.Maybe Core.Int)
cpcConnectionBorrowTimeout = Lens.field @"connectionBorrowTimeout"
{-# DEPRECATED cpcConnectionBorrowTimeout "Use generic-lens or generic-optics with 'connectionBorrowTimeout' instead." #-}

-- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
--
-- Default: no initialization query
--
-- /Note:/ Consider using 'initQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcInitQuery :: Lens.Lens' ConnectionPoolConfiguration (Core.Maybe Types.String)
cpcInitQuery = Lens.field @"initQuery"
{-# DEPRECATED cpcInitQuery "Use generic-lens or generic-optics with 'initQuery' instead." #-}

-- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- Default: 100
-- Constraints: between 1 and 100
--
-- /Note:/ Consider using 'maxConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcMaxConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Core.Maybe Core.Int)
cpcMaxConnectionsPercent = Lens.field @"maxConnectionsPercent"
{-# DEPRECATED cpcMaxConnectionsPercent "Use generic-lens or generic-optics with 'maxConnectionsPercent' instead." #-}

-- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- Default: 50
-- Constraints: between 0 and @MaxConnectionsPercent@
--
-- /Note:/ Consider using 'maxIdleConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcMaxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Core.Maybe Core.Int)
cpcMaxIdleConnectionsPercent = Lens.field @"maxIdleConnectionsPercent"
{-# DEPRECATED cpcMaxIdleConnectionsPercent "Use generic-lens or generic-optics with 'maxIdleConnectionsPercent' instead." #-}

-- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior.
--
-- Default: no session pinning filters
--
-- /Note:/ Consider using 'sessionPinningFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcSessionPinningFilters :: Lens.Lens' ConnectionPoolConfiguration (Core.Maybe [Types.String])
cpcSessionPinningFilters = Lens.field @"sessionPinningFilters"
{-# DEPRECATED cpcSessionPinningFilters "Use generic-lens or generic-optics with 'sessionPinningFilters' instead." #-}
