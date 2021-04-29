{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.RDS.Types.ConnectionPoolConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ConnectionPoolConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings that control the size and behavior of the
-- connection pool associated with a @DBProxyTargetGroup@.
--
-- /See:/ 'newConnectionPoolConfiguration' smart constructor.
data ConnectionPoolConfiguration = ConnectionPoolConfiguration'
  { -- | Each item in the list represents a class of SQL operations that normally
    -- cause all later statements in a session using a proxy to be pinned to
    -- the same underlying database connection. Including an item in the list
    -- exempts that class of SQL operations from the pinning behavior.
    --
    -- Default: no session pinning filters
    sessionPinningFilters :: Prelude.Maybe [Prelude.Text],
    -- | Controls how actively the proxy closes idle database connections in the
    -- connection pool. A high value enables the proxy to leave a high
    -- percentage of idle connections open. A low value causes the proxy to
    -- close idle client connections and return the underlying database
    -- connections to the connection pool. For Aurora MySQL, it is expressed as
    -- a percentage of the @max_connections@ setting for the RDS DB instance or
    -- Aurora DB cluster used by the target group.
    --
    -- Default: 50
    --
    -- Constraints: between 0 and @MaxConnectionsPercent@
    maxIdleConnectionsPercent :: Prelude.Maybe Prelude.Int,
    -- | The number of seconds for a proxy to wait for a connection to become
    -- available in the connection pool. Only applies when the proxy has opened
    -- its maximum number of connections and all connections are busy with
    -- client sessions.
    --
    -- Default: 120
    --
    -- Constraints: between 1 and 3600, or 0 representing unlimited
    connectionBorrowTimeout :: Prelude.Maybe Prelude.Int,
    -- | One or more SQL statements for the proxy to run when opening each new
    -- database connection. Typically used with @SET@ statements to make sure
    -- that each connection has identical settings such as time zone and
    -- character set. For multiple statements, use semicolons as the separator.
    -- You can also include multiple variables in a single @SET@ statement,
    -- such as @SET x=1, y=2@.
    --
    -- Default: no initialization query
    initQuery :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of the connection pool for each target in a target
    -- group. For Aurora MySQL, it is expressed as a percentage of the
    -- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
    -- used by the target group.
    --
    -- Default: 100
    --
    -- Constraints: between 1 and 100
    maxConnectionsPercent :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConnectionPoolConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionPinningFilters', 'connectionPoolConfiguration_sessionPinningFilters' - Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior.
--
-- Default: no session pinning filters
--
-- 'maxIdleConnectionsPercent', 'connectionPoolConfiguration_maxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the
-- connection pool. A high value enables the proxy to leave a high
-- percentage of idle connections open. A low value causes the proxy to
-- close idle client connections and return the underlying database
-- connections to the connection pool. For Aurora MySQL, it is expressed as
-- a percentage of the @max_connections@ setting for the RDS DB instance or
-- Aurora DB cluster used by the target group.
--
-- Default: 50
--
-- Constraints: between 0 and @MaxConnectionsPercent@
--
-- 'connectionBorrowTimeout', 'connectionPoolConfiguration_connectionBorrowTimeout' - The number of seconds for a proxy to wait for a connection to become
-- available in the connection pool. Only applies when the proxy has opened
-- its maximum number of connections and all connections are busy with
-- client sessions.
--
-- Default: 120
--
-- Constraints: between 1 and 3600, or 0 representing unlimited
--
-- 'initQuery', 'connectionPoolConfiguration_initQuery' - One or more SQL statements for the proxy to run when opening each new
-- database connection. Typically used with @SET@ statements to make sure
-- that each connection has identical settings such as time zone and
-- character set. For multiple statements, use semicolons as the separator.
-- You can also include multiple variables in a single @SET@ statement,
-- such as @SET x=1, y=2@.
--
-- Default: no initialization query
--
-- 'maxConnectionsPercent', 'connectionPoolConfiguration_maxConnectionsPercent' - The maximum size of the connection pool for each target in a target
-- group. For Aurora MySQL, it is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group.
--
-- Default: 100
--
-- Constraints: between 1 and 100
newConnectionPoolConfiguration ::
  ConnectionPoolConfiguration
newConnectionPoolConfiguration =
  ConnectionPoolConfiguration'
    { sessionPinningFilters =
        Prelude.Nothing,
      maxIdleConnectionsPercent = Prelude.Nothing,
      connectionBorrowTimeout = Prelude.Nothing,
      initQuery = Prelude.Nothing,
      maxConnectionsPercent = Prelude.Nothing
    }

-- | Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior.
--
-- Default: no session pinning filters
connectionPoolConfiguration_sessionPinningFilters :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe [Prelude.Text])
connectionPoolConfiguration_sessionPinningFilters = Lens.lens (\ConnectionPoolConfiguration' {sessionPinningFilters} -> sessionPinningFilters) (\s@ConnectionPoolConfiguration' {} a -> s {sessionPinningFilters = a} :: ConnectionPoolConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Controls how actively the proxy closes idle database connections in the
-- connection pool. A high value enables the proxy to leave a high
-- percentage of idle connections open. A low value causes the proxy to
-- close idle client connections and return the underlying database
-- connections to the connection pool. For Aurora MySQL, it is expressed as
-- a percentage of the @max_connections@ setting for the RDS DB instance or
-- Aurora DB cluster used by the target group.
--
-- Default: 50
--
-- Constraints: between 0 and @MaxConnectionsPercent@
connectionPoolConfiguration_maxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe Prelude.Int)
connectionPoolConfiguration_maxIdleConnectionsPercent = Lens.lens (\ConnectionPoolConfiguration' {maxIdleConnectionsPercent} -> maxIdleConnectionsPercent) (\s@ConnectionPoolConfiguration' {} a -> s {maxIdleConnectionsPercent = a} :: ConnectionPoolConfiguration)

-- | The number of seconds for a proxy to wait for a connection to become
-- available in the connection pool. Only applies when the proxy has opened
-- its maximum number of connections and all connections are busy with
-- client sessions.
--
-- Default: 120
--
-- Constraints: between 1 and 3600, or 0 representing unlimited
connectionPoolConfiguration_connectionBorrowTimeout :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe Prelude.Int)
connectionPoolConfiguration_connectionBorrowTimeout = Lens.lens (\ConnectionPoolConfiguration' {connectionBorrowTimeout} -> connectionBorrowTimeout) (\s@ConnectionPoolConfiguration' {} a -> s {connectionBorrowTimeout = a} :: ConnectionPoolConfiguration)

-- | One or more SQL statements for the proxy to run when opening each new
-- database connection. Typically used with @SET@ statements to make sure
-- that each connection has identical settings such as time zone and
-- character set. For multiple statements, use semicolons as the separator.
-- You can also include multiple variables in a single @SET@ statement,
-- such as @SET x=1, y=2@.
--
-- Default: no initialization query
connectionPoolConfiguration_initQuery :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe Prelude.Text)
connectionPoolConfiguration_initQuery = Lens.lens (\ConnectionPoolConfiguration' {initQuery} -> initQuery) (\s@ConnectionPoolConfiguration' {} a -> s {initQuery = a} :: ConnectionPoolConfiguration)

-- | The maximum size of the connection pool for each target in a target
-- group. For Aurora MySQL, it is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group.
--
-- Default: 100
--
-- Constraints: between 1 and 100
connectionPoolConfiguration_maxConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe Prelude.Int)
connectionPoolConfiguration_maxConnectionsPercent = Lens.lens (\ConnectionPoolConfiguration' {maxConnectionsPercent} -> maxConnectionsPercent) (\s@ConnectionPoolConfiguration' {} a -> s {maxConnectionsPercent = a} :: ConnectionPoolConfiguration)

instance Prelude.Hashable ConnectionPoolConfiguration

instance Prelude.NFData ConnectionPoolConfiguration

instance Prelude.ToQuery ConnectionPoolConfiguration where
  toQuery ConnectionPoolConfiguration' {..} =
    Prelude.mconcat
      [ "SessionPinningFilters"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> sessionPinningFilters
            ),
        "MaxIdleConnectionsPercent"
          Prelude.=: maxIdleConnectionsPercent,
        "ConnectionBorrowTimeout"
          Prelude.=: connectionBorrowTimeout,
        "InitQuery" Prelude.=: initQuery,
        "MaxConnectionsPercent"
          Prelude.=: maxConnectionsPercent
      ]
