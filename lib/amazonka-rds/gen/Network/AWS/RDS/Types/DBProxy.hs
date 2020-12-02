{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxy where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.DBProxyStatus
import Network.AWS.RDS.Types.UserAuthConfigInfo

-- | The data structure representing a proxy managed by the RDS Proxy.
--
--
-- This data type is used as a response element in the @DescribeDBProxies@ action.
--
--
-- /See:/ 'dbProxy' smart constructor.
data DBProxy = DBProxy'
  { _dpStatus :: !(Maybe DBProxyStatus),
    _dpDBProxyARN :: !(Maybe Text),
    _dpDebugLogging :: !(Maybe Bool),
    _dpVPCSubnetIds :: !(Maybe [Text]),
    _dpEngineFamily :: !(Maybe Text),
    _dpAuth :: !(Maybe [UserAuthConfigInfo]),
    _dpRequireTLS :: !(Maybe Bool),
    _dpIdleClientTimeout :: !(Maybe Int),
    _dpUpdatedDate :: !(Maybe ISO8601),
    _dpCreatedDate :: !(Maybe ISO8601),
    _dpVPCSecurityGroupIds :: !(Maybe [Text]),
    _dpDBProxyName :: !(Maybe Text),
    _dpEndpoint :: !(Maybe Text),
    _dpRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBProxy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpStatus' - The current status of this proxy. A status of @available@ means the proxy is ready to handle requests. Other values indicate that you must wait for the proxy to be ready, or take some action to resolve an issue.
--
-- * 'dpDBProxyARN' - The Amazon Resource Name (ARN) for the proxy.
--
-- * 'dpDebugLogging' - Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- * 'dpVPCSubnetIds' - The EC2 subnet IDs for the proxy.
--
-- * 'dpEngineFamily' - The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
--
-- * 'dpAuth' - One or more data structures specifying the authorization mechanism to connect to the associated RDS DB instance or Aurora DB cluster.
--
-- * 'dpRequireTLS' - Indicates whether Transport Layer Security (TLS) encryption is required for connections to the proxy.
--
-- * 'dpIdleClientTimeout' - The number of seconds a connection to the proxy can have no activity before the proxy drops the client connection. The proxy keeps the underlying database connection open and puts it back into the connection pool for reuse by later connection requests. Default: 1800 (30 minutes) Constraints: 1 to 28,800
--
-- * 'dpUpdatedDate' - The date and time when the proxy was last updated.
--
-- * 'dpCreatedDate' - The date and time when the proxy was first created.
--
-- * 'dpVPCSecurityGroupIds' - Provides a list of VPC security groups that the proxy belongs to.
--
-- * 'dpDBProxyName' - The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region.
--
-- * 'dpEndpoint' - The endpoint that you can use to connect to the proxy. You include the endpoint value in the connection string for a database client application.
--
-- * 'dpRoleARN' - The Amazon Resource Name (ARN) for the IAM role that the proxy uses to access Amazon Secrets Manager.
dbProxy ::
  DBProxy
dbProxy =
  DBProxy'
    { _dpStatus = Nothing,
      _dpDBProxyARN = Nothing,
      _dpDebugLogging = Nothing,
      _dpVPCSubnetIds = Nothing,
      _dpEngineFamily = Nothing,
      _dpAuth = Nothing,
      _dpRequireTLS = Nothing,
      _dpIdleClientTimeout = Nothing,
      _dpUpdatedDate = Nothing,
      _dpCreatedDate = Nothing,
      _dpVPCSecurityGroupIds = Nothing,
      _dpDBProxyName = Nothing,
      _dpEndpoint = Nothing,
      _dpRoleARN = Nothing
    }

-- | The current status of this proxy. A status of @available@ means the proxy is ready to handle requests. Other values indicate that you must wait for the proxy to be ready, or take some action to resolve an issue.
dpStatus :: Lens' DBProxy (Maybe DBProxyStatus)
dpStatus = lens _dpStatus (\s a -> s {_dpStatus = a})

-- | The Amazon Resource Name (ARN) for the proxy.
dpDBProxyARN :: Lens' DBProxy (Maybe Text)
dpDBProxyARN = lens _dpDBProxyARN (\s a -> s {_dpDBProxyARN = a})

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
dpDebugLogging :: Lens' DBProxy (Maybe Bool)
dpDebugLogging = lens _dpDebugLogging (\s a -> s {_dpDebugLogging = a})

-- | The EC2 subnet IDs for the proxy.
dpVPCSubnetIds :: Lens' DBProxy [Text]
dpVPCSubnetIds = lens _dpVPCSubnetIds (\s a -> s {_dpVPCSubnetIds = a}) . _Default . _Coerce

-- | The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
dpEngineFamily :: Lens' DBProxy (Maybe Text)
dpEngineFamily = lens _dpEngineFamily (\s a -> s {_dpEngineFamily = a})

-- | One or more data structures specifying the authorization mechanism to connect to the associated RDS DB instance or Aurora DB cluster.
dpAuth :: Lens' DBProxy [UserAuthConfigInfo]
dpAuth = lens _dpAuth (\s a -> s {_dpAuth = a}) . _Default . _Coerce

-- | Indicates whether Transport Layer Security (TLS) encryption is required for connections to the proxy.
dpRequireTLS :: Lens' DBProxy (Maybe Bool)
dpRequireTLS = lens _dpRequireTLS (\s a -> s {_dpRequireTLS = a})

-- | The number of seconds a connection to the proxy can have no activity before the proxy drops the client connection. The proxy keeps the underlying database connection open and puts it back into the connection pool for reuse by later connection requests. Default: 1800 (30 minutes) Constraints: 1 to 28,800
dpIdleClientTimeout :: Lens' DBProxy (Maybe Int)
dpIdleClientTimeout = lens _dpIdleClientTimeout (\s a -> s {_dpIdleClientTimeout = a})

-- | The date and time when the proxy was last updated.
dpUpdatedDate :: Lens' DBProxy (Maybe UTCTime)
dpUpdatedDate = lens _dpUpdatedDate (\s a -> s {_dpUpdatedDate = a}) . mapping _Time

-- | The date and time when the proxy was first created.
dpCreatedDate :: Lens' DBProxy (Maybe UTCTime)
dpCreatedDate = lens _dpCreatedDate (\s a -> s {_dpCreatedDate = a}) . mapping _Time

-- | Provides a list of VPC security groups that the proxy belongs to.
dpVPCSecurityGroupIds :: Lens' DBProxy [Text]
dpVPCSecurityGroupIds = lens _dpVPCSecurityGroupIds (\s a -> s {_dpVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region.
dpDBProxyName :: Lens' DBProxy (Maybe Text)
dpDBProxyName = lens _dpDBProxyName (\s a -> s {_dpDBProxyName = a})

-- | The endpoint that you can use to connect to the proxy. You include the endpoint value in the connection string for a database client application.
dpEndpoint :: Lens' DBProxy (Maybe Text)
dpEndpoint = lens _dpEndpoint (\s a -> s {_dpEndpoint = a})

-- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to access Amazon Secrets Manager.
dpRoleARN :: Lens' DBProxy (Maybe Text)
dpRoleARN = lens _dpRoleARN (\s a -> s {_dpRoleARN = a})

instance FromXML DBProxy where
  parseXML x =
    DBProxy'
      <$> (x .@? "Status")
      <*> (x .@? "DBProxyArn")
      <*> (x .@? "DebugLogging")
      <*> (x .@? "VpcSubnetIds" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "EngineFamily")
      <*> (x .@? "Auth" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "RequireTLS")
      <*> (x .@? "IdleClientTimeout")
      <*> (x .@? "UpdatedDate")
      <*> (x .@? "CreatedDate")
      <*> ( x .@? "VpcSecurityGroupIds" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "DBProxyName")
      <*> (x .@? "Endpoint")
      <*> (x .@? "RoleArn")

instance Hashable DBProxy

instance NFData DBProxy
