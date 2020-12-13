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
    cpcMaxIdleConnectionsPercent,
    cpcSessionPinningFilters,
    cpcMaxConnectionsPercent,
    cpcConnectionBorrowTimeout,
    cpcInitQuery,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings that control the size and behavior of the connection pool associated with a @DBProxyTargetGroup@ .
--
-- /See:/ 'mkConnectionPoolConfiguration' smart constructor.
data ConnectionPoolConfiguration = ConnectionPoolConfiguration'
  { -- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
    --
    -- Default: 50
    -- Constraints: between 0 and @MaxConnectionsPercent@
    maxIdleConnectionsPercent :: Lude.Maybe Lude.Int,
    -- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior.
    --
    -- Default: no session pinning filters
    sessionPinningFilters :: Lude.Maybe [Lude.Text],
    -- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
    --
    -- Default: 100
    -- Constraints: between 1 and 100
    maxConnectionsPercent :: Lude.Maybe Lude.Int,
    -- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
    --
    -- Default: 120
    -- Constraints: between 1 and 3600, or 0 representing unlimited
    connectionBorrowTimeout :: Lude.Maybe Lude.Int,
    -- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
    --
    -- Default: no initialization query
    initQuery :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionPoolConfiguration' with the minimum fields required to make a request.
--
-- * 'maxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- Default: 50
-- Constraints: between 0 and @MaxConnectionsPercent@
-- * 'sessionPinningFilters' - Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior.
--
-- Default: no session pinning filters
-- * 'maxConnectionsPercent' - The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- Default: 100
-- Constraints: between 1 and 100
-- * 'connectionBorrowTimeout' - The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
--
-- Default: 120
-- Constraints: between 1 and 3600, or 0 representing unlimited
-- * 'initQuery' - One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
--
-- Default: no initialization query
mkConnectionPoolConfiguration ::
  ConnectionPoolConfiguration
mkConnectionPoolConfiguration =
  ConnectionPoolConfiguration'
    { maxIdleConnectionsPercent =
        Lude.Nothing,
      sessionPinningFilters = Lude.Nothing,
      maxConnectionsPercent = Lude.Nothing,
      connectionBorrowTimeout = Lude.Nothing,
      initQuery = Lude.Nothing
    }

-- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- Default: 50
-- Constraints: between 0 and @MaxConnectionsPercent@
--
-- /Note:/ Consider using 'maxIdleConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcMaxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Lude.Maybe Lude.Int)
cpcMaxIdleConnectionsPercent = Lens.lens (maxIdleConnectionsPercent :: ConnectionPoolConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {maxIdleConnectionsPercent = a} :: ConnectionPoolConfiguration)
{-# DEPRECATED cpcMaxIdleConnectionsPercent "Use generic-lens or generic-optics with 'maxIdleConnectionsPercent' instead." #-}

-- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior.
--
-- Default: no session pinning filters
--
-- /Note:/ Consider using 'sessionPinningFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcSessionPinningFilters :: Lens.Lens' ConnectionPoolConfiguration (Lude.Maybe [Lude.Text])
cpcSessionPinningFilters = Lens.lens (sessionPinningFilters :: ConnectionPoolConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {sessionPinningFilters = a} :: ConnectionPoolConfiguration)
{-# DEPRECATED cpcSessionPinningFilters "Use generic-lens or generic-optics with 'sessionPinningFilters' instead." #-}

-- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- Default: 100
-- Constraints: between 1 and 100
--
-- /Note:/ Consider using 'maxConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcMaxConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Lude.Maybe Lude.Int)
cpcMaxConnectionsPercent = Lens.lens (maxConnectionsPercent :: ConnectionPoolConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {maxConnectionsPercent = a} :: ConnectionPoolConfiguration)
{-# DEPRECATED cpcMaxConnectionsPercent "Use generic-lens or generic-optics with 'maxConnectionsPercent' instead." #-}

-- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
--
-- Default: 120
-- Constraints: between 1 and 3600, or 0 representing unlimited
--
-- /Note:/ Consider using 'connectionBorrowTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcConnectionBorrowTimeout :: Lens.Lens' ConnectionPoolConfiguration (Lude.Maybe Lude.Int)
cpcConnectionBorrowTimeout = Lens.lens (connectionBorrowTimeout :: ConnectionPoolConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {connectionBorrowTimeout = a} :: ConnectionPoolConfiguration)
{-# DEPRECATED cpcConnectionBorrowTimeout "Use generic-lens or generic-optics with 'connectionBorrowTimeout' instead." #-}

-- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
--
-- Default: no initialization query
--
-- /Note:/ Consider using 'initQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcInitQuery :: Lens.Lens' ConnectionPoolConfiguration (Lude.Maybe Lude.Text)
cpcInitQuery = Lens.lens (initQuery :: ConnectionPoolConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {initQuery = a} :: ConnectionPoolConfiguration)
{-# DEPRECATED cpcInitQuery "Use generic-lens or generic-optics with 'initQuery' instead." #-}

instance Lude.ToQuery ConnectionPoolConfiguration where
  toQuery ConnectionPoolConfiguration' {..} =
    Lude.mconcat
      [ "MaxIdleConnectionsPercent" Lude.=: maxIdleConnectionsPercent,
        "SessionPinningFilters"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> sessionPinningFilters),
        "MaxConnectionsPercent" Lude.=: maxConnectionsPercent,
        "ConnectionBorrowTimeout" Lude.=: connectionBorrowTimeout,
        "InitQuery" Lude.=: initQuery
      ]
