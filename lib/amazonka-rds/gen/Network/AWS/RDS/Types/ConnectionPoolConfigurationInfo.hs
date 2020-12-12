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
    cpciMaxIdleConnectionsPercent,
    cpciSessionPinningFilters,
    cpciMaxConnectionsPercent,
    cpciConnectionBorrowTimeout,
    cpciInitQuery,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Displays the settings that control the size and behavior of the connection pool associated with a @DBProxyTarget@ .
--
-- /See:/ 'mkConnectionPoolConfigurationInfo' smart constructor.
data ConnectionPoolConfigurationInfo = ConnectionPoolConfigurationInfo'
  { maxIdleConnectionsPercent ::
      Lude.Maybe Lude.Int,
    sessionPinningFilters ::
      Lude.Maybe [Lude.Text],
    maxConnectionsPercent ::
      Lude.Maybe Lude.Int,
    connectionBorrowTimeout ::
      Lude.Maybe Lude.Int,
    initQuery ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionPoolConfigurationInfo' with the minimum fields required to make a request.
--
-- * 'connectionBorrowTimeout' - The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
-- * 'initQuery' - One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. This setting is empty by default. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
-- * 'maxConnectionsPercent' - The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
-- * 'maxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
-- * 'sessionPinningFilters' - Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@ .
mkConnectionPoolConfigurationInfo ::
  ConnectionPoolConfigurationInfo
mkConnectionPoolConfigurationInfo =
  ConnectionPoolConfigurationInfo'
    { maxIdleConnectionsPercent =
        Lude.Nothing,
      sessionPinningFilters = Lude.Nothing,
      maxConnectionsPercent = Lude.Nothing,
      connectionBorrowTimeout = Lude.Nothing,
      initQuery = Lude.Nothing
    }

-- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- /Note:/ Consider using 'maxIdleConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciMaxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Lude.Maybe Lude.Int)
cpciMaxIdleConnectionsPercent = Lens.lens (maxIdleConnectionsPercent :: ConnectionPoolConfigurationInfo -> Lude.Maybe Lude.Int) (\s a -> s {maxIdleConnectionsPercent = a} :: ConnectionPoolConfigurationInfo)
{-# DEPRECATED cpciMaxIdleConnectionsPercent "Use generic-lens or generic-optics with 'maxIdleConnectionsPercent' instead." #-}

-- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@ .
--
-- /Note:/ Consider using 'sessionPinningFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciSessionPinningFilters :: Lens.Lens' ConnectionPoolConfigurationInfo (Lude.Maybe [Lude.Text])
cpciSessionPinningFilters = Lens.lens (sessionPinningFilters :: ConnectionPoolConfigurationInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {sessionPinningFilters = a} :: ConnectionPoolConfigurationInfo)
{-# DEPRECATED cpciSessionPinningFilters "Use generic-lens or generic-optics with 'sessionPinningFilters' instead." #-}

-- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- /Note:/ Consider using 'maxConnectionsPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciMaxConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Lude.Maybe Lude.Int)
cpciMaxConnectionsPercent = Lens.lens (maxConnectionsPercent :: ConnectionPoolConfigurationInfo -> Lude.Maybe Lude.Int) (\s a -> s {maxConnectionsPercent = a} :: ConnectionPoolConfigurationInfo)
{-# DEPRECATED cpciMaxConnectionsPercent "Use generic-lens or generic-optics with 'maxConnectionsPercent' instead." #-}

-- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
--
-- /Note:/ Consider using 'connectionBorrowTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciConnectionBorrowTimeout :: Lens.Lens' ConnectionPoolConfigurationInfo (Lude.Maybe Lude.Int)
cpciConnectionBorrowTimeout = Lens.lens (connectionBorrowTimeout :: ConnectionPoolConfigurationInfo -> Lude.Maybe Lude.Int) (\s a -> s {connectionBorrowTimeout = a} :: ConnectionPoolConfigurationInfo)
{-# DEPRECATED cpciConnectionBorrowTimeout "Use generic-lens or generic-optics with 'connectionBorrowTimeout' instead." #-}

-- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. This setting is empty by default. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
--
-- /Note:/ Consider using 'initQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpciInitQuery :: Lens.Lens' ConnectionPoolConfigurationInfo (Lude.Maybe Lude.Text)
cpciInitQuery = Lens.lens (initQuery :: ConnectionPoolConfigurationInfo -> Lude.Maybe Lude.Text) (\s a -> s {initQuery = a} :: ConnectionPoolConfigurationInfo)
{-# DEPRECATED cpciInitQuery "Use generic-lens or generic-optics with 'initQuery' instead." #-}

instance Lude.FromXML ConnectionPoolConfigurationInfo where
  parseXML x =
    ConnectionPoolConfigurationInfo'
      Lude.<$> (x Lude..@? "MaxIdleConnectionsPercent")
      Lude.<*> ( x Lude..@? "SessionPinningFilters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "MaxConnectionsPercent")
      Lude.<*> (x Lude..@? "ConnectionBorrowTimeout")
      Lude.<*> (x Lude..@? "InitQuery")
