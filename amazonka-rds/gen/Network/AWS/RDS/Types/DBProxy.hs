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
-- Module      : Network.AWS.RDS.Types.DBProxy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.DBProxyStatus
import Network.AWS.RDS.Types.UserAuthConfigInfo

-- | The data structure representing a proxy managed by the RDS Proxy.
--
-- This data type is used as a response element in the @DescribeDBProxies@
-- action.
--
-- /See:/ 'newDBProxy' smart constructor.
data DBProxy = DBProxy'
  { -- | The Amazon Resource Name (ARN) for the proxy.
    dbProxyArn :: Core.Maybe Core.Text,
    -- | The current status of this proxy. A status of @available@ means the
    -- proxy is ready to handle requests. Other values indicate that you must
    -- wait for the proxy to be ready, or take some action to resolve an issue.
    status :: Core.Maybe DBProxyStatus,
    -- | The date and time when the proxy was first created.
    createdDate :: Core.Maybe Core.ISO8601,
    -- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
    -- access Amazon Secrets Manager.
    roleArn :: Core.Maybe Core.Text,
    -- | The number of seconds a connection to the proxy can have no activity
    -- before the proxy drops the client connection. The proxy keeps the
    -- underlying database connection open and puts it back into the connection
    -- pool for reuse by later connection requests.
    --
    -- Default: 1800 (30 minutes)
    --
    -- Constraints: 1 to 28,800
    idleClientTimeout :: Core.Maybe Core.Int,
    -- | The EC2 subnet IDs for the proxy.
    vpcSubnetIds :: Core.Maybe [Core.Text],
    -- | One or more data structures specifying the authorization mechanism to
    -- connect to the associated RDS DB instance or Aurora DB cluster.
    auth :: Core.Maybe [UserAuthConfigInfo],
    -- | The engine family applies to MySQL and PostgreSQL for both RDS and
    -- Aurora.
    engineFamily :: Core.Maybe Core.Text,
    -- | Provides a list of VPC security groups that the proxy belongs to.
    vpcSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | Whether the proxy includes detailed information about SQL statements in
    -- its logs. This information helps you to debug issues involving SQL
    -- behavior or the performance and scalability of the proxy connections.
    -- The debug information includes the text of SQL statements that you
    -- submit through the proxy. Thus, only enable this setting when needed for
    -- debugging, and only when you have security measures in place to
    -- safeguard any sensitive information that appears in the logs.
    debugLogging :: Core.Maybe Core.Bool,
    -- | The date and time when the proxy was last updated.
    updatedDate :: Core.Maybe Core.ISO8601,
    -- | Indicates whether Transport Layer Security (TLS) encryption is required
    -- for connections to the proxy.
    requireTLS :: Core.Maybe Core.Bool,
    -- | The endpoint that you can use to connect to the proxy. You include the
    -- endpoint value in the connection string for a database client
    -- application.
    endpoint :: Core.Maybe Core.Text,
    -- | The identifier for the proxy. This name must be unique for all proxies
    -- owned by your AWS account in the specified AWS Region.
    dbProxyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyArn', 'dbProxy_dbProxyArn' - The Amazon Resource Name (ARN) for the proxy.
--
-- 'status', 'dbProxy_status' - The current status of this proxy. A status of @available@ means the
-- proxy is ready to handle requests. Other values indicate that you must
-- wait for the proxy to be ready, or take some action to resolve an issue.
--
-- 'createdDate', 'dbProxy_createdDate' - The date and time when the proxy was first created.
--
-- 'roleArn', 'dbProxy_roleArn' - The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
-- access Amazon Secrets Manager.
--
-- 'idleClientTimeout', 'dbProxy_idleClientTimeout' - The number of seconds a connection to the proxy can have no activity
-- before the proxy drops the client connection. The proxy keeps the
-- underlying database connection open and puts it back into the connection
-- pool for reuse by later connection requests.
--
-- Default: 1800 (30 minutes)
--
-- Constraints: 1 to 28,800
--
-- 'vpcSubnetIds', 'dbProxy_vpcSubnetIds' - The EC2 subnet IDs for the proxy.
--
-- 'auth', 'dbProxy_auth' - One or more data structures specifying the authorization mechanism to
-- connect to the associated RDS DB instance or Aurora DB cluster.
--
-- 'engineFamily', 'dbProxy_engineFamily' - The engine family applies to MySQL and PostgreSQL for both RDS and
-- Aurora.
--
-- 'vpcSecurityGroupIds', 'dbProxy_vpcSecurityGroupIds' - Provides a list of VPC security groups that the proxy belongs to.
--
-- 'debugLogging', 'dbProxy_debugLogging' - Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
--
-- 'updatedDate', 'dbProxy_updatedDate' - The date and time when the proxy was last updated.
--
-- 'requireTLS', 'dbProxy_requireTLS' - Indicates whether Transport Layer Security (TLS) encryption is required
-- for connections to the proxy.
--
-- 'endpoint', 'dbProxy_endpoint' - The endpoint that you can use to connect to the proxy. You include the
-- endpoint value in the connection string for a database client
-- application.
--
-- 'dbProxyName', 'dbProxy_dbProxyName' - The identifier for the proxy. This name must be unique for all proxies
-- owned by your AWS account in the specified AWS Region.
newDBProxy ::
  DBProxy
newDBProxy =
  DBProxy'
    { dbProxyArn = Core.Nothing,
      status = Core.Nothing,
      createdDate = Core.Nothing,
      roleArn = Core.Nothing,
      idleClientTimeout = Core.Nothing,
      vpcSubnetIds = Core.Nothing,
      auth = Core.Nothing,
      engineFamily = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing,
      debugLogging = Core.Nothing,
      updatedDate = Core.Nothing,
      requireTLS = Core.Nothing,
      endpoint = Core.Nothing,
      dbProxyName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the proxy.
dbProxy_dbProxyArn :: Lens.Lens' DBProxy (Core.Maybe Core.Text)
dbProxy_dbProxyArn = Lens.lens (\DBProxy' {dbProxyArn} -> dbProxyArn) (\s@DBProxy' {} a -> s {dbProxyArn = a} :: DBProxy)

-- | The current status of this proxy. A status of @available@ means the
-- proxy is ready to handle requests. Other values indicate that you must
-- wait for the proxy to be ready, or take some action to resolve an issue.
dbProxy_status :: Lens.Lens' DBProxy (Core.Maybe DBProxyStatus)
dbProxy_status = Lens.lens (\DBProxy' {status} -> status) (\s@DBProxy' {} a -> s {status = a} :: DBProxy)

-- | The date and time when the proxy was first created.
dbProxy_createdDate :: Lens.Lens' DBProxy (Core.Maybe Core.UTCTime)
dbProxy_createdDate = Lens.lens (\DBProxy' {createdDate} -> createdDate) (\s@DBProxy' {} a -> s {createdDate = a} :: DBProxy) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
-- access Amazon Secrets Manager.
dbProxy_roleArn :: Lens.Lens' DBProxy (Core.Maybe Core.Text)
dbProxy_roleArn = Lens.lens (\DBProxy' {roleArn} -> roleArn) (\s@DBProxy' {} a -> s {roleArn = a} :: DBProxy)

-- | The number of seconds a connection to the proxy can have no activity
-- before the proxy drops the client connection. The proxy keeps the
-- underlying database connection open and puts it back into the connection
-- pool for reuse by later connection requests.
--
-- Default: 1800 (30 minutes)
--
-- Constraints: 1 to 28,800
dbProxy_idleClientTimeout :: Lens.Lens' DBProxy (Core.Maybe Core.Int)
dbProxy_idleClientTimeout = Lens.lens (\DBProxy' {idleClientTimeout} -> idleClientTimeout) (\s@DBProxy' {} a -> s {idleClientTimeout = a} :: DBProxy)

-- | The EC2 subnet IDs for the proxy.
dbProxy_vpcSubnetIds :: Lens.Lens' DBProxy (Core.Maybe [Core.Text])
dbProxy_vpcSubnetIds = Lens.lens (\DBProxy' {vpcSubnetIds} -> vpcSubnetIds) (\s@DBProxy' {} a -> s {vpcSubnetIds = a} :: DBProxy) Core.. Lens.mapping Lens._Coerce

-- | One or more data structures specifying the authorization mechanism to
-- connect to the associated RDS DB instance or Aurora DB cluster.
dbProxy_auth :: Lens.Lens' DBProxy (Core.Maybe [UserAuthConfigInfo])
dbProxy_auth = Lens.lens (\DBProxy' {auth} -> auth) (\s@DBProxy' {} a -> s {auth = a} :: DBProxy) Core.. Lens.mapping Lens._Coerce

-- | The engine family applies to MySQL and PostgreSQL for both RDS and
-- Aurora.
dbProxy_engineFamily :: Lens.Lens' DBProxy (Core.Maybe Core.Text)
dbProxy_engineFamily = Lens.lens (\DBProxy' {engineFamily} -> engineFamily) (\s@DBProxy' {} a -> s {engineFamily = a} :: DBProxy)

-- | Provides a list of VPC security groups that the proxy belongs to.
dbProxy_vpcSecurityGroupIds :: Lens.Lens' DBProxy (Core.Maybe [Core.Text])
dbProxy_vpcSecurityGroupIds = Lens.lens (\DBProxy' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@DBProxy' {} a -> s {vpcSecurityGroupIds = a} :: DBProxy) Core.. Lens.mapping Lens._Coerce

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
dbProxy_debugLogging :: Lens.Lens' DBProxy (Core.Maybe Core.Bool)
dbProxy_debugLogging = Lens.lens (\DBProxy' {debugLogging} -> debugLogging) (\s@DBProxy' {} a -> s {debugLogging = a} :: DBProxy)

-- | The date and time when the proxy was last updated.
dbProxy_updatedDate :: Lens.Lens' DBProxy (Core.Maybe Core.UTCTime)
dbProxy_updatedDate = Lens.lens (\DBProxy' {updatedDate} -> updatedDate) (\s@DBProxy' {} a -> s {updatedDate = a} :: DBProxy) Core.. Lens.mapping Core._Time

-- | Indicates whether Transport Layer Security (TLS) encryption is required
-- for connections to the proxy.
dbProxy_requireTLS :: Lens.Lens' DBProxy (Core.Maybe Core.Bool)
dbProxy_requireTLS = Lens.lens (\DBProxy' {requireTLS} -> requireTLS) (\s@DBProxy' {} a -> s {requireTLS = a} :: DBProxy)

-- | The endpoint that you can use to connect to the proxy. You include the
-- endpoint value in the connection string for a database client
-- application.
dbProxy_endpoint :: Lens.Lens' DBProxy (Core.Maybe Core.Text)
dbProxy_endpoint = Lens.lens (\DBProxy' {endpoint} -> endpoint) (\s@DBProxy' {} a -> s {endpoint = a} :: DBProxy)

-- | The identifier for the proxy. This name must be unique for all proxies
-- owned by your AWS account in the specified AWS Region.
dbProxy_dbProxyName :: Lens.Lens' DBProxy (Core.Maybe Core.Text)
dbProxy_dbProxyName = Lens.lens (\DBProxy' {dbProxyName} -> dbProxyName) (\s@DBProxy' {} a -> s {dbProxyName = a} :: DBProxy)

instance Core.FromXML DBProxy where
  parseXML x =
    DBProxy'
      Core.<$> (x Core..@? "DBProxyArn")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "CreatedDate")
      Core.<*> (x Core..@? "RoleArn")
      Core.<*> (x Core..@? "IdleClientTimeout")
      Core.<*> ( x Core..@? "VpcSubnetIds" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "Auth" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "EngineFamily")
      Core.<*> ( x Core..@? "VpcSecurityGroupIds"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "DebugLogging")
      Core.<*> (x Core..@? "UpdatedDate")
      Core.<*> (x Core..@? "RequireTLS")
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "DBProxyName")

instance Core.Hashable DBProxy

instance Core.NFData DBProxy
