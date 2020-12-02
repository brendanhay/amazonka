{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Displays the settings that control the size and behavior of the connection pool associated with a @DBProxyTarget@ .
--
--
--
-- /See:/ 'connectionPoolConfigurationInfo' smart constructor.
data ConnectionPoolConfigurationInfo = ConnectionPoolConfigurationInfo'
  { _cpciMaxIdleConnectionsPercent ::
      !(Maybe Int),
    _cpciSessionPinningFilters ::
      !(Maybe [Text]),
    _cpciMaxConnectionsPercent ::
      !(Maybe Int),
    _cpciConnectionBorrowTimeout ::
      !(Maybe Int),
    _cpciInitQuery ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionPoolConfigurationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpciMaxIdleConnectionsPercent' - Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- * 'cpciSessionPinningFilters' - Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@ .
--
-- * 'cpciMaxConnectionsPercent' - The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
--
-- * 'cpciConnectionBorrowTimeout' - The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
--
-- * 'cpciInitQuery' - One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. This setting is empty by default. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
connectionPoolConfigurationInfo ::
  ConnectionPoolConfigurationInfo
connectionPoolConfigurationInfo =
  ConnectionPoolConfigurationInfo'
    { _cpciMaxIdleConnectionsPercent =
        Nothing,
      _cpciSessionPinningFilters = Nothing,
      _cpciMaxConnectionsPercent = Nothing,
      _cpciConnectionBorrowTimeout = Nothing,
      _cpciInitQuery = Nothing
    }

-- | Controls how actively the proxy closes idle database connections in the connection pool. A high value enables the proxy to leave a high percentage of idle connections open. A low value causes the proxy to close idle client connections and return the underlying database connections to the connection pool. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
cpciMaxIdleConnectionsPercent :: Lens' ConnectionPoolConfigurationInfo (Maybe Int)
cpciMaxIdleConnectionsPercent = lens _cpciMaxIdleConnectionsPercent (\s a -> s {_cpciMaxIdleConnectionsPercent = a})

-- | Each item in the list represents a class of SQL operations that normally cause all later statements in a session using a proxy to be pinned to the same underlying database connection. Including an item in the list exempts that class of SQL operations from the pinning behavior. Currently, the only allowed value is @EXCLUDE_VARIABLE_SETS@ .
cpciSessionPinningFilters :: Lens' ConnectionPoolConfigurationInfo [Text]
cpciSessionPinningFilters = lens _cpciSessionPinningFilters (\s a -> s {_cpciSessionPinningFilters = a}) . _Default . _Coerce

-- | The maximum size of the connection pool for each target in a target group. For Aurora MySQL, it is expressed as a percentage of the @max_connections@ setting for the RDS DB instance or Aurora DB cluster used by the target group.
cpciMaxConnectionsPercent :: Lens' ConnectionPoolConfigurationInfo (Maybe Int)
cpciMaxConnectionsPercent = lens _cpciMaxConnectionsPercent (\s a -> s {_cpciMaxConnectionsPercent = a})

-- | The number of seconds for a proxy to wait for a connection to become available in the connection pool. Only applies when the proxy has opened its maximum number of connections and all connections are busy with client sessions.
cpciConnectionBorrowTimeout :: Lens' ConnectionPoolConfigurationInfo (Maybe Int)
cpciConnectionBorrowTimeout = lens _cpciConnectionBorrowTimeout (\s a -> s {_cpciConnectionBorrowTimeout = a})

-- | One or more SQL statements for the proxy to run when opening each new database connection. Typically used with @SET@ statements to make sure that each connection has identical settings such as time zone and character set. This setting is empty by default. For multiple statements, use semicolons as the separator. You can also include multiple variables in a single @SET@ statement, such as @SET x=1, y=2@ .
cpciInitQuery :: Lens' ConnectionPoolConfigurationInfo (Maybe Text)
cpciInitQuery = lens _cpciInitQuery (\s a -> s {_cpciInitQuery = a})

instance FromXML ConnectionPoolConfigurationInfo where
  parseXML x =
    ConnectionPoolConfigurationInfo'
      <$> (x .@? "MaxIdleConnectionsPercent")
      <*> ( x .@? "SessionPinningFilters" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "MaxConnectionsPercent")
      <*> (x .@? "ConnectionBorrowTimeout")
      <*> (x .@? "InitQuery")

instance Hashable ConnectionPoolConfigurationInfo

instance NFData ConnectionPoolConfigurationInfo
