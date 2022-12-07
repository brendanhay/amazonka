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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The Amazon Resource Name (ARN) for the proxy.
    dbProxyArn :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of VPC security groups that the proxy belongs to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The kinds of databases that the proxy can connect to. This value
    -- determines which database network protocol the proxy recognizes when it
    -- interprets network traffic to and from the database. @MYSQL@ supports
    -- Aurora MySQL, RDS for MariaDB, and RDS for MySQL databases. @POSTGRESQL@
    -- supports Aurora PostgreSQL and RDS for PostgreSQL databases. @SQLSERVER@
    -- supports RDS for Microsoft SQL Server databases.
    engineFamily :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
    -- access Amazon Secrets Manager.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Transport Layer Security (TLS) encryption is required
    -- for connections to the proxy.
    requireTLS :: Prelude.Maybe Prelude.Bool,
    -- | The current status of this proxy. A status of @available@ means the
    -- proxy is ready to handle requests. Other values indicate that you must
    -- wait for the proxy to be ready, or take some action to resolve an issue.
    status :: Prelude.Maybe DBProxyStatus,
    -- | Whether the proxy includes detailed information about SQL statements in
    -- its logs. This information helps you to debug issues involving SQL
    -- behavior or the performance and scalability of the proxy connections.
    -- The debug information includes the text of SQL statements that you
    -- submit through the proxy. Thus, only enable this setting when needed for
    -- debugging, and only when you have security measures in place to
    -- safeguard any sensitive information that appears in the logs.
    debugLogging :: Prelude.Maybe Prelude.Bool,
    -- | The EC2 subnet IDs for the proxy.
    vpcSubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The date and time when the proxy was last updated.
    updatedDate :: Prelude.Maybe Data.ISO8601,
    -- | The date and time when the proxy was first created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The endpoint that you can use to connect to the DB proxy. You include
    -- the endpoint value in the connection string for a database client
    -- application.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | Provides the VPC ID of the DB proxy.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | One or more data structures specifying the authorization mechanism to
    -- connect to the associated RDS DB instance or Aurora DB cluster.
    auth :: Prelude.Maybe [UserAuthConfigInfo],
    -- | The identifier for the proxy. This name must be unique for all proxies
    -- owned by your Amazon Web Services account in the specified Amazon Web
    -- Services Region.
    dbProxyName :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds a connection to the proxy can have no activity
    -- before the proxy drops the client connection. The proxy keeps the
    -- underlying database connection open and puts it back into the connection
    -- pool for reuse by later connection requests.
    --
    -- Default: 1800 (30 minutes)
    --
    -- Constraints: 1 to 28,800
    idleClientTimeout :: Prelude.Maybe Prelude.Int
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
-- 'dbProxyArn', 'dbProxy_dbProxyArn' - The Amazon Resource Name (ARN) for the proxy.
--
-- 'vpcSecurityGroupIds', 'dbProxy_vpcSecurityGroupIds' - Provides a list of VPC security groups that the proxy belongs to.
--
-- 'engineFamily', 'dbProxy_engineFamily' - The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. @MYSQL@ supports
-- Aurora MySQL, RDS for MariaDB, and RDS for MySQL databases. @POSTGRESQL@
-- supports Aurora PostgreSQL and RDS for PostgreSQL databases. @SQLSERVER@
-- supports RDS for Microsoft SQL Server databases.
--
-- 'roleArn', 'dbProxy_roleArn' - The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
-- access Amazon Secrets Manager.
--
-- 'requireTLS', 'dbProxy_requireTLS' - Indicates whether Transport Layer Security (TLS) encryption is required
-- for connections to the proxy.
--
-- 'status', 'dbProxy_status' - The current status of this proxy. A status of @available@ means the
-- proxy is ready to handle requests. Other values indicate that you must
-- wait for the proxy to be ready, or take some action to resolve an issue.
--
-- 'debugLogging', 'dbProxy_debugLogging' - Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
--
-- 'vpcSubnetIds', 'dbProxy_vpcSubnetIds' - The EC2 subnet IDs for the proxy.
--
-- 'updatedDate', 'dbProxy_updatedDate' - The date and time when the proxy was last updated.
--
-- 'createdDate', 'dbProxy_createdDate' - The date and time when the proxy was first created.
--
-- 'endpoint', 'dbProxy_endpoint' - The endpoint that you can use to connect to the DB proxy. You include
-- the endpoint value in the connection string for a database client
-- application.
--
-- 'vpcId', 'dbProxy_vpcId' - Provides the VPC ID of the DB proxy.
--
-- 'auth', 'dbProxy_auth' - One or more data structures specifying the authorization mechanism to
-- connect to the associated RDS DB instance or Aurora DB cluster.
--
-- 'dbProxyName', 'dbProxy_dbProxyName' - The identifier for the proxy. This name must be unique for all proxies
-- owned by your Amazon Web Services account in the specified Amazon Web
-- Services Region.
--
-- 'idleClientTimeout', 'dbProxy_idleClientTimeout' - The number of seconds a connection to the proxy can have no activity
-- before the proxy drops the client connection. The proxy keeps the
-- underlying database connection open and puts it back into the connection
-- pool for reuse by later connection requests.
--
-- Default: 1800 (30 minutes)
--
-- Constraints: 1 to 28,800
newDBProxy ::
  DBProxy
newDBProxy =
  DBProxy'
    { dbProxyArn = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      engineFamily = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      requireTLS = Prelude.Nothing,
      status = Prelude.Nothing,
      debugLogging = Prelude.Nothing,
      vpcSubnetIds = Prelude.Nothing,
      updatedDate = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      auth = Prelude.Nothing,
      dbProxyName = Prelude.Nothing,
      idleClientTimeout = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the proxy.
dbProxy_dbProxyArn :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_dbProxyArn = Lens.lens (\DBProxy' {dbProxyArn} -> dbProxyArn) (\s@DBProxy' {} a -> s {dbProxyArn = a} :: DBProxy)

-- | Provides a list of VPC security groups that the proxy belongs to.
dbProxy_vpcSecurityGroupIds :: Lens.Lens' DBProxy (Prelude.Maybe [Prelude.Text])
dbProxy_vpcSecurityGroupIds = Lens.lens (\DBProxy' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@DBProxy' {} a -> s {vpcSecurityGroupIds = a} :: DBProxy) Prelude.. Lens.mapping Lens.coerced

-- | The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. @MYSQL@ supports
-- Aurora MySQL, RDS for MariaDB, and RDS for MySQL databases. @POSTGRESQL@
-- supports Aurora PostgreSQL and RDS for PostgreSQL databases. @SQLSERVER@
-- supports RDS for Microsoft SQL Server databases.
dbProxy_engineFamily :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_engineFamily = Lens.lens (\DBProxy' {engineFamily} -> engineFamily) (\s@DBProxy' {} a -> s {engineFamily = a} :: DBProxy)

-- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to
-- access Amazon Secrets Manager.
dbProxy_roleArn :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_roleArn = Lens.lens (\DBProxy' {roleArn} -> roleArn) (\s@DBProxy' {} a -> s {roleArn = a} :: DBProxy)

-- | Indicates whether Transport Layer Security (TLS) encryption is required
-- for connections to the proxy.
dbProxy_requireTLS :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Bool)
dbProxy_requireTLS = Lens.lens (\DBProxy' {requireTLS} -> requireTLS) (\s@DBProxy' {} a -> s {requireTLS = a} :: DBProxy)

-- | The current status of this proxy. A status of @available@ means the
-- proxy is ready to handle requests. Other values indicate that you must
-- wait for the proxy to be ready, or take some action to resolve an issue.
dbProxy_status :: Lens.Lens' DBProxy (Prelude.Maybe DBProxyStatus)
dbProxy_status = Lens.lens (\DBProxy' {status} -> status) (\s@DBProxy' {} a -> s {status = a} :: DBProxy)

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
dbProxy_debugLogging :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Bool)
dbProxy_debugLogging = Lens.lens (\DBProxy' {debugLogging} -> debugLogging) (\s@DBProxy' {} a -> s {debugLogging = a} :: DBProxy)

-- | The EC2 subnet IDs for the proxy.
dbProxy_vpcSubnetIds :: Lens.Lens' DBProxy (Prelude.Maybe [Prelude.Text])
dbProxy_vpcSubnetIds = Lens.lens (\DBProxy' {vpcSubnetIds} -> vpcSubnetIds) (\s@DBProxy' {} a -> s {vpcSubnetIds = a} :: DBProxy) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the proxy was last updated.
dbProxy_updatedDate :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.UTCTime)
dbProxy_updatedDate = Lens.lens (\DBProxy' {updatedDate} -> updatedDate) (\s@DBProxy' {} a -> s {updatedDate = a} :: DBProxy) Prelude.. Lens.mapping Data._Time

-- | The date and time when the proxy was first created.
dbProxy_createdDate :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.UTCTime)
dbProxy_createdDate = Lens.lens (\DBProxy' {createdDate} -> createdDate) (\s@DBProxy' {} a -> s {createdDate = a} :: DBProxy) Prelude.. Lens.mapping Data._Time

-- | The endpoint that you can use to connect to the DB proxy. You include
-- the endpoint value in the connection string for a database client
-- application.
dbProxy_endpoint :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_endpoint = Lens.lens (\DBProxy' {endpoint} -> endpoint) (\s@DBProxy' {} a -> s {endpoint = a} :: DBProxy)

-- | Provides the VPC ID of the DB proxy.
dbProxy_vpcId :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_vpcId = Lens.lens (\DBProxy' {vpcId} -> vpcId) (\s@DBProxy' {} a -> s {vpcId = a} :: DBProxy)

-- | One or more data structures specifying the authorization mechanism to
-- connect to the associated RDS DB instance or Aurora DB cluster.
dbProxy_auth :: Lens.Lens' DBProxy (Prelude.Maybe [UserAuthConfigInfo])
dbProxy_auth = Lens.lens (\DBProxy' {auth} -> auth) (\s@DBProxy' {} a -> s {auth = a} :: DBProxy) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the proxy. This name must be unique for all proxies
-- owned by your Amazon Web Services account in the specified Amazon Web
-- Services Region.
dbProxy_dbProxyName :: Lens.Lens' DBProxy (Prelude.Maybe Prelude.Text)
dbProxy_dbProxyName = Lens.lens (\DBProxy' {dbProxyName} -> dbProxyName) (\s@DBProxy' {} a -> s {dbProxyName = a} :: DBProxy)

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

instance Data.FromXML DBProxy where
  parseXML x =
    DBProxy'
      Prelude.<$> (x Data..@? "DBProxyArn")
      Prelude.<*> ( x Data..@? "VpcSecurityGroupIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "EngineFamily")
      Prelude.<*> (x Data..@? "RoleArn")
      Prelude.<*> (x Data..@? "RequireTLS")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "DebugLogging")
      Prelude.<*> ( x Data..@? "VpcSubnetIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "UpdatedDate")
      Prelude.<*> (x Data..@? "CreatedDate")
      Prelude.<*> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "VpcId")
      Prelude.<*> ( x Data..@? "Auth" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "DBProxyName")
      Prelude.<*> (x Data..@? "IdleClientTimeout")

instance Prelude.Hashable DBProxy where
  hashWithSalt _salt DBProxy' {..} =
    _salt `Prelude.hashWithSalt` dbProxyArn
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` engineFamily
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` requireTLS
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` debugLogging
      `Prelude.hashWithSalt` vpcSubnetIds
      `Prelude.hashWithSalt` updatedDate
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` auth
      `Prelude.hashWithSalt` dbProxyName
      `Prelude.hashWithSalt` idleClientTimeout

instance Prelude.NFData DBProxy where
  rnf DBProxy' {..} =
    Prelude.rnf dbProxyArn
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf engineFamily
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf requireTLS
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf debugLogging
      `Prelude.seq` Prelude.rnf vpcSubnetIds
      `Prelude.seq` Prelude.rnf updatedDate
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf auth
      `Prelude.seq` Prelude.rnf dbProxyName
      `Prelude.seq` Prelude.rnf idleClientTimeout
