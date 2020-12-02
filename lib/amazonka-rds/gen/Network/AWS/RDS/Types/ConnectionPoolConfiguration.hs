{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ConnectionPoolConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ConnectionPoolConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the settings that control the size and behavior of the connection pool associated with a @DBProxyTargetGroup@ .
--
--
--
-- /See:/ 'connectionPoolConfiguration' smart constructor.
data ConnectionPoolConfiguration = ConnectionPoolConfiguration'
  { _cpcMaxIdleConnectionsPercent ::
      !(Maybe Int),
    _cpcSessionPinningFilters ::
      !(Maybe [Text]),
    _cpcMaxConnectionsPercent ::
      !(Maybe Int),
    _cpcConnectionBorrowTimeout ::
      !(Maybe Int),
    _cpcInitQuery :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionPoolConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpcMaxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.  Default: 50 Constraints: between 0 and @MaxConnectionsPercent@
--
-- * 'cpcSessionPinningFilters' - Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Default: no session pinning filters
--
-- * 'cpcMaxConnectionsPercent' - The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group. Default: 100 Constraints: between 1 and 100
--
-- * 'cpcConnectionBorrowTimeout' - The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions. Default: 120 Constraints: between 1 and 3600, or 0 representing unlimited
--
-- * 'cpcInitQuery' - One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .  Default: no initialization query
connectionPoolConfiguration ::
  ConnectionPoolConfiguration
connectionPoolConfiguration =
  ConnectionPoolConfiguration'
    { _cpcMaxIdleConnectionsPercent =
        Nothing,
      _cpcSessionPinningFilters = Nothing,
      _cpcMaxConnectionsPercent = Nothing,
      _cpcConnectionBorrowTimeout = Nothing,
      _cpcInitQuery = Nothing
    }

-- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.  Default: 50 Constraints: between 0 and @MaxConnectionsPercent@
cpcMaxIdleConnectionsPercent :: Lens' ConnectionPoolConfiguration (Maybe Int)
cpcMaxIdleConnectionsPercent = lens _cpcMaxIdleConnectionsPercent (\s a -> s {_cpcMaxIdleConnectionsPercent = a})

-- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Default: no session pinning filters
cpcSessionPinningFilters :: Lens' ConnectionPoolConfiguration [Text]
cpcSessionPinningFilters = lens _cpcSessionPinningFilters (\s a -> s {_cpcSessionPinningFilters = a}) . _Default . _Coerce

-- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group. Default: 100 Constraints: between 1 and 100
cpcMaxConnectionsPercent :: Lens' ConnectionPoolConfiguration (Maybe Int)
cpcMaxConnectionsPercent = lens _cpcMaxConnectionsPercent (\s a -> s {_cpcMaxConnectionsPercent = a})

-- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions. Default: 120 Constraints: between 1 and 3600, or 0 representing unlimited
cpcConnectionBorrowTimeout :: Lens' ConnectionPoolConfiguration (Maybe Int)
cpcConnectionBorrowTimeout = lens _cpcConnectionBorrowTimeout (\s a -> s {_cpcConnectionBorrowTimeout = a})

-- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .  Default: no initialization query
cpcInitQuery :: Lens' ConnectionPoolConfiguration (Maybe Text)
cpcInitQuery = lens _cpcInitQuery (\s a -> s {_cpcInitQuery = a})

instance Hashable ConnectionPoolConfiguration

instance NFData ConnectionPoolConfiguration

instance ToQuery ConnectionPoolConfiguration where
  toQuery ConnectionPoolConfiguration' {..} =
    mconcat
      [ "MaxIdleConnectionsPercent" =: _cpcMaxIdleConnectionsPercent,
        "SessionPinningFilters"
          =: toQuery (toQueryList "member" <$> _cpcSessionPinningFilters),
        "MaxConnectionsPercent" =: _cpcMaxConnectionsPercent,
        "ConnectionBorrowTimeout" =: _cpcConnectionBorrowTimeout,
        "InitQuery" =: _cpcInitQuery
      ]
