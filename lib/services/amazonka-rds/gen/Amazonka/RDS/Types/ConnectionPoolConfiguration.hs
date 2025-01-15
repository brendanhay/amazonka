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
-- Module      : Amazonka.RDS.Types.ConnectionPoolConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ConnectionPoolConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings that control the size and behavior of the
-- connection pool associated with a @DBProxyTargetGroup@.
--
-- /See:/ 'newConnectionPoolConfiguration' smart constructor.
data ConnectionPoolConfiguration = ConnectionPoolConfiguration'
  { -- | The number of seconds for a proxy to wait for a connection to become
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
    -- group. The value is expressed as a percentage of the @max_connections@
    -- setting for the RDS DB instance or Aurora DB cluster used by the target
    -- group.
    --
    -- If you specify @MaxIdleConnectionsPercent@, then you must also include a
    -- value for this parameter.
    --
    -- Default: 10 for RDS for Microsoft SQL Server, and 100 for all other
    -- engines
    --
    -- Constraints: Must be between 1 and 100.
    maxConnectionsPercent :: Prelude.Maybe Prelude.Int,
    -- | Controls how actively the proxy closes idle database connections in the
    -- connection pool. The value is expressed as a percentage of the
    -- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
    -- used by the target group. With a high value, the proxy leaves a high
    -- percentage of idle database connections open. A low value causes the
    -- proxy to close more idle connections and return them to the database.
    --
    -- If you specify this parameter, then you must also include a value for
    -- @MaxConnectionsPercent@.
    --
    -- Default: The default value is half of the value of
    -- @MaxConnectionsPercent@. For example, if @MaxConnectionsPercent@ is 80,
    -- then the default value of @MaxIdleConnectionsPercent@ is 40. If the
    -- value of @MaxConnectionsPercent@ isn\'t specified, then for SQL Server,
    -- @MaxIdleConnectionsPercent@ is 5, and for all other engines, the default
    -- is 50.
    --
    -- Constraints: Must be between 0 and the value of @MaxConnectionsPercent@.
    maxIdleConnectionsPercent :: Prelude.Maybe Prelude.Int,
    -- | Each item in the list represents a class of SQL operations that normally
    -- cause all later statements in a session using a proxy to be pinned to
    -- the same underlying database connection. Including an item in the list
    -- exempts that class of SQL operations from the pinning behavior.
    --
    -- Default: no session pinning filters
    sessionPinningFilters :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionPoolConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- group. The value is expressed as a percentage of the @max_connections@
-- setting for the RDS DB instance or Aurora DB cluster used by the target
-- group.
--
-- If you specify @MaxIdleConnectionsPercent@, then you must also include a
-- value for this parameter.
--
-- Default: 10 for RDS for Microsoft SQL Server, and 100 for all other
-- engines
--
-- Constraints: Must be between 1 and 100.
--
-- 'maxIdleConnectionsPercent', 'connectionPoolConfiguration_maxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the
-- connection pool. The value is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group. With a high value, the proxy leaves a high
-- percentage of idle database connections open. A low value causes the
-- proxy to close more idle connections and return them to the database.
--
-- If you specify this parameter, then you must also include a value for
-- @MaxConnectionsPercent@.
--
-- Default: The default value is half of the value of
-- @MaxConnectionsPercent@. For example, if @MaxConnectionsPercent@ is 80,
-- then the default value of @MaxIdleConnectionsPercent@ is 40. If the
-- value of @MaxConnectionsPercent@ isn\'t specified, then for SQL Server,
-- @MaxIdleConnectionsPercent@ is 5, and for all other engines, the default
-- is 50.
--
-- Constraints: Must be between 0 and the value of @MaxConnectionsPercent@.
--
-- 'sessionPinningFilters', 'connectionPoolConfiguration_sessionPinningFilters' - Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior.
--
-- Default: no session pinning filters
newConnectionPoolConfiguration ::
  ConnectionPoolConfiguration
newConnectionPoolConfiguration =
  ConnectionPoolConfiguration'
    { connectionBorrowTimeout =
        Prelude.Nothing,
      initQuery = Prelude.Nothing,
      maxConnectionsPercent = Prelude.Nothing,
      maxIdleConnectionsPercent = Prelude.Nothing,
      sessionPinningFilters = Prelude.Nothing
    }

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
-- group. The value is expressed as a percentage of the @max_connections@
-- setting for the RDS DB instance or Aurora DB cluster used by the target
-- group.
--
-- If you specify @MaxIdleConnectionsPercent@, then you must also include a
-- value for this parameter.
--
-- Default: 10 for RDS for Microsoft SQL Server, and 100 for all other
-- engines
--
-- Constraints: Must be between 1 and 100.
connectionPoolConfiguration_maxConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe Prelude.Int)
connectionPoolConfiguration_maxConnectionsPercent = Lens.lens (\ConnectionPoolConfiguration' {maxConnectionsPercent} -> maxConnectionsPercent) (\s@ConnectionPoolConfiguration' {} a -> s {maxConnectionsPercent = a} :: ConnectionPoolConfiguration)

-- | Controls how actively the proxy closes idle database connections in the
-- connection pool. The value is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group. With a high value, the proxy leaves a high
-- percentage of idle database connections open. A low value causes the
-- proxy to close more idle connections and return them to the database.
--
-- If you specify this parameter, then you must also include a value for
-- @MaxConnectionsPercent@.
--
-- Default: The default value is half of the value of
-- @MaxConnectionsPercent@. For example, if @MaxConnectionsPercent@ is 80,
-- then the default value of @MaxIdleConnectionsPercent@ is 40. If the
-- value of @MaxConnectionsPercent@ isn\'t specified, then for SQL Server,
-- @MaxIdleConnectionsPercent@ is 5, and for all other engines, the default
-- is 50.
--
-- Constraints: Must be between 0 and the value of @MaxConnectionsPercent@.
connectionPoolConfiguration_maxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe Prelude.Int)
connectionPoolConfiguration_maxIdleConnectionsPercent = Lens.lens (\ConnectionPoolConfiguration' {maxIdleConnectionsPercent} -> maxIdleConnectionsPercent) (\s@ConnectionPoolConfiguration' {} a -> s {maxIdleConnectionsPercent = a} :: ConnectionPoolConfiguration)

-- | Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior.
--
-- Default: no session pinning filters
connectionPoolConfiguration_sessionPinningFilters :: Lens.Lens' ConnectionPoolConfiguration (Prelude.Maybe [Prelude.Text])
connectionPoolConfiguration_sessionPinningFilters = Lens.lens (\ConnectionPoolConfiguration' {sessionPinningFilters} -> sessionPinningFilters) (\s@ConnectionPoolConfiguration' {} a -> s {sessionPinningFilters = a} :: ConnectionPoolConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ConnectionPoolConfiguration where
  hashWithSalt _salt ConnectionPoolConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` connectionBorrowTimeout
      `Prelude.hashWithSalt` initQuery
      `Prelude.hashWithSalt` maxConnectionsPercent
      `Prelude.hashWithSalt` maxIdleConnectionsPercent
      `Prelude.hashWithSalt` sessionPinningFilters

instance Prelude.NFData ConnectionPoolConfiguration where
  rnf ConnectionPoolConfiguration' {..} =
    Prelude.rnf connectionBorrowTimeout `Prelude.seq`
      Prelude.rnf initQuery `Prelude.seq`
        Prelude.rnf maxConnectionsPercent `Prelude.seq`
          Prelude.rnf maxIdleConnectionsPercent `Prelude.seq`
            Prelude.rnf sessionPinningFilters

instance Data.ToQuery ConnectionPoolConfiguration where
  toQuery ConnectionPoolConfiguration' {..} =
    Prelude.mconcat
      [ "ConnectionBorrowTimeout"
          Data.=: connectionBorrowTimeout,
        "InitQuery" Data.=: initQuery,
        "MaxConnectionsPercent"
          Data.=: maxConnectionsPercent,
        "MaxIdleConnectionsPercent"
          Data.=: maxIdleConnectionsPercent,
        "SessionPinningFilters"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> sessionPinningFilters
            )
      ]
