{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Displays the settings that control the size and behavior of the
-- connection pool associated with a @DBProxyTarget@.
--
-- /See:/ 'newConnectionPoolConfigurationInfo' smart constructor.
data ConnectionPoolConfigurationInfo = ConnectionPoolConfigurationInfo'
  { -- | Each item in the list represents a class of SQL operations that normally
    -- cause all later statements in a session using a proxy to be pinned to
    -- the same underlying database connection. Including an item in the list
    -- exempts that class of SQL operations from the pinning behavior.
    -- Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@.
    sessionPinningFilters :: Core.Maybe [Core.Text],
    -- | Controls how actively the proxy closes idle database connections in the
    -- connection pool. A high value enables the proxy to leave a high
    -- percentage of idle connections open. A low value causes the proxy to
    -- close idle client connections and return the underlying database
    -- connections to the connection pool. For Aurora MySQL, it is expressed as
    -- a percentage of the @max_connections@ setting for the RDS DB instance or
    -- Aurora DB cluster used by the target group.
    maxIdleConnectionsPercent :: Core.Maybe Core.Int,
    -- | The number of seconds for a proxy to wait for a connection to become
    -- available in the connection pool. Only applies when the proxy has opened
    -- its maximum number of connections and all connections are busy with
    -- client sessions.
    connectionBorrowTimeout :: Core.Maybe Core.Int,
    -- | One or more SQL statements for the proxy to run when opening each new
    -- database connection. Typically used with @SET@ statements to make sure
    -- that each connection has identical settings such as time zone and
    -- character set. This setting is empty by default. For multiple
    -- statements, use semicolons as the separator. You can also include
    -- multiple variables in a single @SET@ statement, such as @SET x=1, y=2@.
    initQuery :: Core.Maybe Core.Text,
    -- | The maximum size of the connection pool for each target in a target
    -- group. For Aurora MySQL, it is expressed as a percentage of the
    -- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
    -- used by the target group.
    maxConnectionsPercent :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionPoolConfigurationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionPinningFilters', 'connectionPoolConfigurationInfo_sessionPinningFilters' - Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior.
-- Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@.
--
-- 'maxIdleConnectionsPercent', 'connectionPoolConfigurationInfo_maxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the
-- connection pool. A high value enables the proxy to leave a high
-- percentage of idle connections open. A low value causes the proxy to
-- close idle client connections and return the underlying database
-- connections to the connection pool. For Aurora MySQL, it is expressed as
-- a percentage of the @max_connections@ setting for the RDS DB instance or
-- Aurora DB cluster used by the target group.
--
-- 'connectionBorrowTimeout', 'connectionPoolConfigurationInfo_connectionBorrowTimeout' - The number of seconds for a proxy to wait for a connection to become
-- available in the connection pool. Only applies when the proxy has opened
-- its maximum number of connections and all connections are busy with
-- client sessions.
--
-- 'initQuery', 'connectionPoolConfigurationInfo_initQuery' - One or more SQL statements for the proxy to run when opening each new
-- database connection. Typically used with @SET@ statements to make sure
-- that each connection has identical settings such as time zone and
-- character set. This setting is empty by default. For multiple
-- statements, use semicolons as the separator. You can also include
-- multiple variables in a single @SET@ statement, such as @SET x=1, y=2@.
--
-- 'maxConnectionsPercent', 'connectionPoolConfigurationInfo_maxConnectionsPercent' - The maximum size of the connection pool for each target in a target
-- group. For Aurora MySQL, it is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group.
newConnectionPoolConfigurationInfo ::
  ConnectionPoolConfigurationInfo
newConnectionPoolConfigurationInfo =
  ConnectionPoolConfigurationInfo'
    { sessionPinningFilters =
        Core.Nothing,
      maxIdleConnectionsPercent = Core.Nothing,
      connectionBorrowTimeout = Core.Nothing,
      initQuery = Core.Nothing,
      maxConnectionsPercent = Core.Nothing
    }

-- | Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior.
-- Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@.
connectionPoolConfigurationInfo_sessionPinningFilters :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe [Core.Text])
connectionPoolConfigurationInfo_sessionPinningFilters = Lens.lens (\ConnectionPoolConfigurationInfo' {sessionPinningFilters} -> sessionPinningFilters) (\s@ConnectionPoolConfigurationInfo' {} a -> s {sessionPinningFilters = a} :: ConnectionPoolConfigurationInfo) Core.. Lens.mapping Lens._Coerce

-- | Controls how actively the proxy closes idle database connections in the
-- connection pool. A high value enables the proxy to leave a high
-- percentage of idle connections open. A low value causes the proxy to
-- close idle client connections and return the underlying database
-- connections to the connection pool. For Aurora MySQL, it is expressed as
-- a percentage of the @max_connections@ setting for the RDS DB instance or
-- Aurora DB cluster used by the target group.
connectionPoolConfigurationInfo_maxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Core.Int)
connectionPoolConfigurationInfo_maxIdleConnectionsPercent = Lens.lens (\ConnectionPoolConfigurationInfo' {maxIdleConnectionsPercent} -> maxIdleConnectionsPercent) (\s@ConnectionPoolConfigurationInfo' {} a -> s {maxIdleConnectionsPercent = a} :: ConnectionPoolConfigurationInfo)

-- | The number of seconds for a proxy to wait for a connection to become
-- available in the connection pool. Only applies when the proxy has opened
-- its maximum number of connections and all connections are busy with
-- client sessions.
connectionPoolConfigurationInfo_connectionBorrowTimeout :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Core.Int)
connectionPoolConfigurationInfo_connectionBorrowTimeout = Lens.lens (\ConnectionPoolConfigurationInfo' {connectionBorrowTimeout} -> connectionBorrowTimeout) (\s@ConnectionPoolConfigurationInfo' {} a -> s {connectionBorrowTimeout = a} :: ConnectionPoolConfigurationInfo)

-- | One or more SQL statements for the proxy to run when opening each new
-- database connection. Typically used with @SET@ statements to make sure
-- that each connection has identical settings such as time zone and
-- character set. This setting is empty by default. For multiple
-- statements, use semicolons as the separator. You can also include
-- multiple variables in a single @SET@ statement, such as @SET x=1, y=2@.
connectionPoolConfigurationInfo_initQuery :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Core.Text)
connectionPoolConfigurationInfo_initQuery = Lens.lens (\ConnectionPoolConfigurationInfo' {initQuery} -> initQuery) (\s@ConnectionPoolConfigurationInfo' {} a -> s {initQuery = a} :: ConnectionPoolConfigurationInfo)

-- | The maximum size of the connection pool for each target in a target
-- group. For Aurora MySQL, it is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group.
connectionPoolConfigurationInfo_maxConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Core.Maybe Core.Int)
connectionPoolConfigurationInfo_maxConnectionsPercent = Lens.lens (\ConnectionPoolConfigurationInfo' {maxConnectionsPercent} -> maxConnectionsPercent) (\s@ConnectionPoolConfigurationInfo' {} a -> s {maxConnectionsPercent = a} :: ConnectionPoolConfigurationInfo)

instance Core.FromXML ConnectionPoolConfigurationInfo where
  parseXML x =
    ConnectionPoolConfigurationInfo'
      Core.<$> ( x Core..@? "SessionPinningFilters"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "MaxIdleConnectionsPercent")
      Core.<*> (x Core..@? "ConnectionBorrowTimeout")
      Core.<*> (x Core..@? "InitQuery")
      Core.<*> (x Core..@? "MaxConnectionsPercent")

instance
  Core.Hashable
    ConnectionPoolConfigurationInfo

instance Core.NFData ConnectionPoolConfigurationInfo
