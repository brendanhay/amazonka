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
-- Module      : Amazonka.RDS.Types.DBProxy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBProxy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.DBProxyStatus
import Amazonka.RDS.Types.UserAuthConfigInfo

-- | The data structure representing a proxy managed by the RDS Proxy.
--
-- This data type is used as a response element in the @DescribeDBProxies@
-- action.
--
-- /See:/ 'newDBProxy' smart constructor.
data DBProxy = DBProxy'
  { -- | One or more data structures specifying the authorization mechanism to
    -- connect to the associated RDS DB instance or Aurora DB cluster.
    auth :: Prelude.Maybe [UserAuthConfigInfo],
    -- | The date and time when the proxy was first created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) for the proxy.
    dbProxyArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the proxy. This name must be unique for all proxies
    -- owned by your Amazon Web Services account in the specified Amazon Web
    -- Services Region.
    dbProxyName :: Prelude.Maybe Prelude.Text,
    -- | Whether the proxy includes detailed information about SQL statements in
    -- its logs. This information helps you to debug issues involving SQL
    -- behavior or the performance and scalability of the proxy connections.
    -- The debug information includes the text of SQL statements that you
    -- submit through the proxy. Thus, only enable this setting when needed for
    -- debugging, and only when you have security measures in place to
    -- safeguard any sensitive information that appears in the logs.
    debugLogging :: Prelude.Maybe Prelude.Bool,
    -- | The endpoint that you can use to connect to the DB proxy. You include
    -- the endpoint value in the connection string for a database client
    -- application.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The kinds of databases that the proxy can connect to. This value
    -- determines which database network protocol the proxy recognizes when it
    -- interprets network traffic to and from the database. @MYSQL@ supports
    -- Aurora MySQL, RDS for MariaDB, and RDS for MySQL databases. @POSTGRESQL@
    -- supports Aurora PostgreSQL and RDS for PostgreSQL databases. @SQLSERVER@
    -- supports RDS for Microsoft SQL Server databases.
    engineFamily :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds a connection to the proxy can have no activity
    -- before the proxy drops the client connection. The proxy keeps the
    -- underlying database connection open and puts it back into the connection
    -- pool for reuse by later connection requests.
    --
    -- Default: 1800 (30 minutes)
    --
    -- Constraints: 1 to 28,800
    idleClientTimeout :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether Transport Layer Security (TLS) encryption is required
    -- for connections to the proxy.
    requireTLS :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
    -- access Amazon Secrets Manager.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of this proxy. A status of @available@ means the
    -- proxy is ready to handle requests. Other values indicate that you must
    -- wait for the proxy to be ready, or take some action to resolve an issue.
    status :: Prelude.Maybe DBProxyStatus,
    -- | The date and time when the proxy was last updated.
    updatedDate :: Prelude.Maybe Data.ISO8601,
    -- | Provides the VPC ID of the DB proxy.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of VPC security groups that the proxy belongs to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The EC2 subnet IDs for the proxy.
    vpcSubnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auth', 'dbProxy_auth' - One or more data structures specifying the authorization mechanism to
-- connect to the associated RDS DB instance or Aurora DB cluster.
--
-- 'createdDate', 'dbProxy_createdDate' - The date and time when the proxy was first created.
--
-- 'dbProxyArn', 'dbProxy_dbProxyArn' - The Amazon Resource Name (ARN) for the proxy.
--
-- 'dbProxyName', 'dbProxy_dbProxyName' - The identifier for the proxy. This name must be unique for all proxies
-- owned by your Amazon Web Services account in the specified Amazon Web
-- Services Region.
--
-- 'debugLogging', 'dbProxy_debugLogging' - Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
--
-- 'endpoint', 'dbProxy_endpoint' - The endpoint that you can use to connect to the DB proxy. You include
-- the endpoint value in the connection string for a database client
-- application.
--
-- 'engineFamily', 'dbProxy_engineFamily' - The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. @MYSQL@ supports
-- Aurora MySQL, RDS for MariaDB, and RDS for MySQL databases. @POSTGRESQL@
-- supports Aurora PostgreSQL and RDS for PostgreSQL databases. @SQLSERVER@
-- supports RDS for Microsoft SQL Server databases.
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
-- 'requireTLS', 'dbProxy_requireTLS' - Indicates whether Transport Layer Security (TLS) encryption is required
-- for connections to the proxy.
--
-- 'roleArn', 'dbProxy_roleArn' - The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
-- access Amazon Secrets Manager.
--
-- 'status', 'dbProxy_status' - The current status of this proxy. A status of @available@ means the
-- proxy is ready to handle requests. Other values indicate that you must
-- wait for the proxy to be ready, or take some action to resolve an issue.
--
-- 'updatedDate', 'dbProxy_updatedDate' - The date and time when the proxy was last updated.
--
-- 'vpcId', 'dbProxy_vpcId' - Provides the VPC ID of the DB proxy.
--
-- 'vpcSecurityGroupIds', 'dbProxy_vpcSecurityGroupIds' - Provides a list of VPC security groups that the proxy belongs to.
--
-- 'vpcSubnetIds', 'dbProxy_vpcSubnetIds' - The EC2 subnet IDs for the proxy.
newDBProxy ::
  DBProxy
newDBProxy =
  DBProxy'
    { auth = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      dbProxyArn = Prelude.Nothing,
      dbProxyName = Prelude.Nothing,
      debugLogging = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      engineFamily = Prelude.Nothing,
      idleClientTimeout = Prelude.Nothing,
      requireTLS = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedDate = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      vpcSubnetIds = Prelude.Nothing
    }

-- | One or more data structures specifying the authorization mechanism to
-- connect to the associated RDS DB instance or Aurora DB cluster.
dbProxy_auth :: Lens.Lens' DBProxy (Prelude.Maybe [UserAuthConfigInfo])
dbProxy_auth = Lens.lens (\DBProxy' {auth} -> auth) (\s@DBProxy' {} a -> s {auth = a} :: DBProxy) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the proxy was first created.
dbProxy_createdDate :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.UTCTime)
dbProxy_createdDate = Lens.lens (\DBProxy' {createdDate} -> createdDate) (\s@DBProxy' {} a -> s {createdDate = a} :: DBProxy) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for the proxy.
dbProxy_dbProxyArn :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_dbProxyArn = Lens.lens (\DBProxy' {dbProxyArn} -> dbProxyArn) (\s@DBProxy' {} a -> s {dbProxyArn = a} :: DBProxy)

-- | The identifier for the proxy. This name must be unique for all proxies
-- owned by your Amazon Web Services account in the specified Amazon Web
-- Services Region.
dbProxy_dbProxyName :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_dbProxyName = Lens.lens (\DBProxy' {dbProxyName} -> dbProxyName) (\s@DBProxy' {} a -> s {dbProxyName = a} :: DBProxy)

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
dbProxy_debugLogging :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Bool)
dbProxy_debugLogging = Lens.lens (\DBProxy' {debugLogging} -> debugLogging) (\s@DBProxy' {} a -> s {debugLogging = a} :: DBProxy)

-- | The endpoint that you can use to connect to the DB proxy. You include
-- the endpoint value in the connection string for a database client
-- application.
dbProxy_endpoint :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_endpoint = Lens.lens (\DBProxy' {endpoint} -> endpoint) (\s@DBProxy' {} a -> s {endpoint = a} :: DBProxy)

-- | The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. @MYSQL@ supports
-- Aurora MySQL, RDS for MariaDB, and RDS for MySQL databases. @POSTGRESQL@
-- supports Aurora PostgreSQL and RDS for PostgreSQL databases. @SQLSERVER@
-- supports RDS for Microsoft SQL Server databases.
dbProxy_engineFamily :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_engineFamily = Lens.lens (\DBProxy' {engineFamily} -> engineFamily) (\s@DBProxy' {} a -> s {engineFamily = a} :: DBProxy)

-- | The number of seconds a connection to the proxy can have no activity
-- before the proxy drops the client connection. The proxy keeps the
-- underlying database connection open and puts it back into the connection
-- pool for reuse by later connection requests.
--
-- Default: 1800 (30 minutes)
--
-- Constraints: 1 to 28,800
dbProxy_idleClientTimeout :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Int)
dbProxy_idleClientTimeout = Lens.lens (\DBProxy' {idleClientTimeout} -> idleClientTimeout) (\s@DBProxy' {} a -> s {idleClientTimeout = a} :: DBProxy)

-- | Indicates whether Transport Layer Security (TLS) encryption is required
-- for connections to the proxy.
dbProxy_requireTLS :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Bool)
dbProxy_requireTLS = Lens.lens (\DBProxy' {requireTLS} -> requireTLS) (\s@DBProxy' {} a -> s {requireTLS = a} :: DBProxy)

-- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
-- access Amazon Secrets Manager.
dbProxy_roleArn :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_roleArn = Lens.lens (\DBProxy' {roleArn} -> roleArn) (\s@DBProxy' {} a -> s {roleArn = a} :: DBProxy)

-- | The current status of this proxy. A status of @available@ means the
-- proxy is ready to handle requests. Other values indicate that you must
-- wait for the proxy to be ready, or take some action to resolve an issue.
dbProxy_status :: Lens.Lens' DBProxy (Prelude.Maybe DBProxyStatus)
dbProxy_status = Lens.lens (\DBProxy' {status} -> status) (\s@DBProxy' {} a -> s {status = a} :: DBProxy)

-- | The date and time when the proxy was last updated.
dbProxy_updatedDate :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.UTCTime)
dbProxy_updatedDate = Lens.lens (\DBProxy' {updatedDate} -> updatedDate) (\s@DBProxy' {} a -> s {updatedDate = a} :: DBProxy) Prelude.. Lens.mapping Data._Time

-- | Provides the VPC ID of the DB proxy.
dbProxy_vpcId :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_vpcId = Lens.lens (\DBProxy' {vpcId} -> vpcId) (\s@DBProxy' {} a -> s {vpcId = a} :: DBProxy)

-- | Provides a list of VPC security groups that the proxy belongs to.
dbProxy_vpcSecurityGroupIds :: Lens.Lens' DBProxy (Prelude.Maybe [Prelude.Text])
dbProxy_vpcSecurityGroupIds = Lens.lens (\DBProxy' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@DBProxy' {} a -> s {vpcSecurityGroupIds = a} :: DBProxy) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 subnet IDs for the proxy.
dbProxy_vpcSubnetIds :: Lens.Lens' DBProxy (Prelude.Maybe [Prelude.Text])
dbProxy_vpcSubnetIds = Lens.lens (\DBProxy' {vpcSubnetIds} -> vpcSubnetIds) (\s@DBProxy' {} a -> s {vpcSubnetIds = a} :: DBProxy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DBProxy where
  parseXML x =
    DBProxy'
      Prelude.<$> ( x
                      Data..@? "Auth"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "CreatedDate")
      Prelude.<*> (x Data..@? "DBProxyArn")
      Prelude.<*> (x Data..@? "DBProxyName")
      Prelude.<*> (x Data..@? "DebugLogging")
      Prelude.<*> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "EngineFamily")
      Prelude.<*> (x Data..@? "IdleClientTimeout")
      Prelude.<*> (x Data..@? "RequireTLS")
      Prelude.<*> (x Data..@? "RoleArn")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "UpdatedDate")
      Prelude.<*> (x Data..@? "VpcId")
      Prelude.<*> ( x
                      Data..@? "VpcSecurityGroupIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "VpcSubnetIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable DBProxy where
  hashWithSalt _salt DBProxy' {..} =
    _salt
      `Prelude.hashWithSalt` auth
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` dbProxyArn
      `Prelude.hashWithSalt` dbProxyName
      `Prelude.hashWithSalt` debugLogging
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` engineFamily
      `Prelude.hashWithSalt` idleClientTimeout
      `Prelude.hashWithSalt` requireTLS
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedDate
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` vpcSubnetIds

instance Prelude.NFData DBProxy where
  rnf DBProxy' {..} =
    Prelude.rnf auth
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf dbProxyArn
      `Prelude.seq` Prelude.rnf dbProxyName
      `Prelude.seq` Prelude.rnf debugLogging
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf engineFamily
      `Prelude.seq` Prelude.rnf idleClientTimeout
      `Prelude.seq` Prelude.rnf requireTLS
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedDate
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf vpcSubnetIds
