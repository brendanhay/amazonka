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
-- Module      : Amazonka.RDS.Types.ConnectionPoolConfigurationInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ConnectionPoolConfigurationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Displays the settings that control the size and behavior of the
-- connection pool associated with a @DBProxyTarget@.
--
-- /See:/ 'newConnectionPoolConfigurationInfo' smart constructor.
data ConnectionPoolConfigurationInfo = ConnectionPoolConfigurationInfo'
  { -- | Controls how actively the proxy closes idle database connections in the
    -- connection pool. The value is expressed as a percentage of the
    -- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
    -- used by the target group. With a high value, the proxy leaves a high
    -- percentage of idle database connections open. A low value causes the
    -- proxy to close more idle connections and return them to the database.
    maxIdleConnectionsPercent :: Prelude.Maybe Prelude.Int,
    -- | One or more SQL statements for the proxy to run when opening each new
    -- database connection. Typically used with @SET@ statements to make sure
    -- that each connection has identical settings such as time zone and
    -- character set. This setting is empty by default. For multiple
    -- statements, use semicolons as the separator. You can also include
    -- multiple variables in a single @SET@ statement, such as @SET x=1, y=2@.
    initQuery :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds for a proxy to wait for a connection to become
    -- available in the connection pool. Only applies when the proxy has opened
    -- its maximum number of connections and all connections are busy with
    -- client sessions.
    connectionBorrowTimeout :: Prelude.Maybe Prelude.Int,
    -- | The maximum size of the connection pool for each target in a target
    -- group. The value is expressed as a percentage of the @max_connections@
    -- setting for the RDS DB instance or Aurora DB cluster used by the target
    -- group.
    maxConnectionsPercent :: Prelude.Maybe Prelude.Int,
    -- | Each item in the list represents a class of SQL operations that normally
    -- cause all later statements in a session using a proxy to be pinned to
    -- the same underlying database connection. Including an item in the list
    -- exempts that class of SQL operations from the pinning behavior. This
    -- setting is only supported for MySQL engine family databases. Currently,
    -- the only allowed value is @EXCLUDE_VARIABLE_SETS@.
    sessionPinningFilters :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionPoolConfigurationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxIdleConnectionsPercent', 'connectionPoolConfigurationInfo_maxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the
-- connection pool. The value is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group. With a high value, the proxy leaves a high
-- percentage of idle database connections open. A low value causes the
-- proxy to close more idle connections and return them to the database.
--
-- 'initQuery', 'connectionPoolConfigurationInfo_initQuery' - One or more SQL statements for the proxy to run when opening each new
-- database connection. Typically used with @SET@ statements to make sure
-- that each connection has identical settings such as time zone and
-- character set. This setting is empty by default. For multiple
-- statements, use semicolons as the separator. You can also include
-- multiple variables in a single @SET@ statement, such as @SET x=1, y=2@.
--
-- 'connectionBorrowTimeout', 'connectionPoolConfigurationInfo_connectionBorrowTimeout' - The number of seconds for a proxy to wait for a connection to become
-- available in the connection pool. Only applies when the proxy has opened
-- its maximum number of connections and all connections are busy with
-- client sessions.
--
-- 'maxConnectionsPercent', 'connectionPoolConfigurationInfo_maxConnectionsPercent' - The maximum size of the connection pool for each target in a target
-- group. The value is expressed as a percentage of the @max_connections@
-- setting for the RDS DB instance or Aurora DB cluster used by the target
-- group.
--
-- 'sessionPinningFilters', 'connectionPoolConfigurationInfo_sessionPinningFilters' - Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior. This
-- setting is only supported for MySQL engine family databases. Currently,
-- the only allowed value is @EXCLUDE_VARIABLE_SETS@.
newConnectionPoolConfigurationInfo ::
  ConnectionPoolConfigurationInfo
newConnectionPoolConfigurationInfo =
  ConnectionPoolConfigurationInfo'
    { maxIdleConnectionsPercent =
        Prelude.Nothing,
      initQuery = Prelude.Nothing,
      connectionBorrowTimeout = Prelude.Nothing,
      maxConnectionsPercent = Prelude.Nothing,
      sessionPinningFilters = Prelude.Nothing
    }

-- | Controls how actively the proxy closes idle database connections in the
-- connection pool. The value is expressed as a percentage of the
-- @max_connections@ setting for the RDS DB instance or Aurora DB cluster
-- used by the target group. With a high value, the proxy leaves a high
-- percentage of idle database connections open. A low value causes the
-- proxy to close more idle connections and return them to the database.
connectionPoolConfigurationInfo_maxIdleConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Prelude.Maybe Prelude.Int)
connectionPoolConfigurationInfo_maxIdleConnectionsPercent = Lens.lens (\ConnectionPoolConfigurationInfo' {maxIdleConnectionsPercent} -> maxIdleConnectionsPercent) (\s@ConnectionPoolConfigurationInfo' {} a -> s {maxIdleConnectionsPercent = a} :: ConnectionPoolConfigurationInfo)

-- | One or more SQL statements for the proxy to run when opening each new
-- database connection. Typically used with @SET@ statements to make sure
-- that each connection has identical settings such as time zone and
-- character set. This setting is empty by default. For multiple
-- statements, use semicolons as the separator. You can also include
-- multiple variables in a single @SET@ statement, such as @SET x=1, y=2@.
connectionPoolConfigurationInfo_initQuery :: Lens.Lens' ConnectionPoolConfigurationInfo (Prelude.Maybe Prelude.Text)
connectionPoolConfigurationInfo_initQuery = Lens.lens (\ConnectionPoolConfigurationInfo' {initQuery} -> initQuery) (\s@ConnectionPoolConfigurationInfo' {} a -> s {initQuery = a} :: ConnectionPoolConfigurationInfo)

-- | The number of seconds for a proxy to wait for a connection to become
-- available in the connection pool. Only applies when the proxy has opened
-- its maximum number of connections and all connections are busy with
-- client sessions.
connectionPoolConfigurationInfo_connectionBorrowTimeout :: Lens.Lens' ConnectionPoolConfigurationInfo (Prelude.Maybe Prelude.Int)
connectionPoolConfigurationInfo_connectionBorrowTimeout = Lens.lens (\ConnectionPoolConfigurationInfo' {connectionBorrowTimeout} -> connectionBorrowTimeout) (\s@ConnectionPoolConfigurationInfo' {} a -> s {connectionBorrowTimeout = a} :: ConnectionPoolConfigurationInfo)

-- | The maximum size of the connection pool for each target in a target
-- group. The value is expressed as a percentage of the @max_connections@
-- setting for the RDS DB instance or Aurora DB cluster used by the target
-- group.
connectionPoolConfigurationInfo_maxConnectionsPercent :: Lens.Lens' ConnectionPoolConfigurationInfo (Prelude.Maybe Prelude.Int)
connectionPoolConfigurationInfo_maxConnectionsPercent = Lens.lens (\ConnectionPoolConfigurationInfo' {maxConnectionsPercent} -> maxConnectionsPercent) (\s@ConnectionPoolConfigurationInfo' {} a -> s {maxConnectionsPercent = a} :: ConnectionPoolConfigurationInfo)

-- | Each item in the list represents a class of SQL operations that normally
-- cause all later statements in a session using a proxy to be pinned to
-- the same underlying database connection. Including an item in the list
-- exempts that class of SQL operations from the pinning behavior. This
-- setting is only supported for MySQL engine family databases. Currently,
-- the only allowed value is @EXCLUDE_VARIABLE_SETS@.
connectionPoolConfigurationInfo_sessionPinningFilters :: Lens.Lens' ConnectionPoolConfigurationInfo (Prelude.Maybe [Prelude.Text])
connectionPoolConfigurationInfo_sessionPinningFilters = Lens.lens (\ConnectionPoolConfigurationInfo' {sessionPinningFilters} -> sessionPinningFilters) (\s@ConnectionPoolConfigurationInfo' {} a -> s {sessionPinningFilters = a} :: ConnectionPoolConfigurationInfo) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML ConnectionPoolConfigurationInfo where
  parseXML x =
    ConnectionPoolConfigurationInfo'
      Prelude.<$> (x Core..@? "MaxIdleConnectionsPercent")
      Prelude.<*> (x Core..@? "InitQuery")
      Prelude.<*> (x Core..@? "ConnectionBorrowTimeout")
      Prelude.<*> (x Core..@? "MaxConnectionsPercent")
      Prelude.<*> ( x Core..@? "SessionPinningFilters"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    ConnectionPoolConfigurationInfo
  where
  hashWithSalt
    _salt
    ConnectionPoolConfigurationInfo' {..} =
      _salt
        `Prelude.hashWithSalt` maxIdleConnectionsPercent
        `Prelude.hashWithSalt` initQuery
        `Prelude.hashWithSalt` connectionBorrowTimeout
        `Prelude.hashWithSalt` maxConnectionsPercent
        `Prelude.hashWithSalt` sessionPinningFilters

instance
  Prelude.NFData
    ConnectionPoolConfigurationInfo
  where
  rnf ConnectionPoolConfigurationInfo' {..} =
    Prelude.rnf maxIdleConnectionsPercent
      `Prelude.seq` Prelude.rnf initQuery
      `Prelude.seq` Prelude.rnf connectionBorrowTimeout
      `Prelude.seq` Prelude.rnf maxConnectionsPercent
      `Prelude.seq` Prelude.rnf sessionPinningFilters
