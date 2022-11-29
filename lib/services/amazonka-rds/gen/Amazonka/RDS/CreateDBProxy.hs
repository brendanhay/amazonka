{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.CreateDBProxy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB proxy.
module Amazonka.RDS.CreateDBProxy
  ( -- * Creating a Request
    CreateDBProxy (..),
    newCreateDBProxy,

    -- * Request Lenses
    createDBProxy_tags,
    createDBProxy_vpcSecurityGroupIds,
    createDBProxy_requireTLS,
    createDBProxy_debugLogging,
    createDBProxy_idleClientTimeout,
    createDBProxy_dbProxyName,
    createDBProxy_engineFamily,
    createDBProxy_auth,
    createDBProxy_roleArn,
    createDBProxy_vpcSubnetIds,

    -- * Destructuring the Response
    CreateDBProxyResponse (..),
    newCreateDBProxyResponse,

    -- * Response Lenses
    createDBProxyResponse_dbProxy,
    createDBProxyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBProxy' smart constructor.
data CreateDBProxy = CreateDBProxy'
  { -- | An optional set of key-value pairs to associate arbitrary data of your
    -- choosing with the proxy.
    tags :: Prelude.Maybe [Tag],
    -- | One or more VPC security group IDs to associate with the new proxy.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A Boolean parameter that specifies whether Transport Layer Security
    -- (TLS) encryption is required for connections to the proxy. By enabling
    -- this setting, you can enforce encrypted TLS connections to the proxy.
    requireTLS :: Prelude.Maybe Prelude.Bool,
    -- | Whether the proxy includes detailed information about SQL statements in
    -- its logs. This information helps you to debug issues involving SQL
    -- behavior or the performance and scalability of the proxy connections.
    -- The debug information includes the text of SQL statements that you
    -- submit through the proxy. Thus, only enable this setting when needed for
    -- debugging, and only when you have security measures in place to
    -- safeguard any sensitive information that appears in the logs.
    debugLogging :: Prelude.Maybe Prelude.Bool,
    -- | The number of seconds that a connection to the proxy can be inactive
    -- before the proxy disconnects it. You can set this value higher or lower
    -- than the connection timeout limit for the associated database.
    idleClientTimeout :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the proxy. This name must be unique for all proxies
    -- owned by your Amazon Web Services account in the specified Amazon Web
    -- Services Region. An identifier must begin with a letter and must contain
    -- only ASCII letters, digits, and hyphens; it can\'t end with a hyphen or
    -- contain two consecutive hyphens.
    dbProxyName :: Prelude.Text,
    -- | The kinds of databases that the proxy can connect to. This value
    -- determines which database network protocol the proxy recognizes when it
    -- interprets network traffic to and from the database. For Aurora MySQL,
    -- RDS for MariaDB, and RDS for MySQL databases, specify @MYSQL@. For
    -- Aurora PostgreSQL and RDS for PostgreSQL databases, specify
    -- @POSTGRESQL@. For RDS for Microsoft SQL Server, specify @SQLSERVER@.
    engineFamily :: EngineFamily,
    -- | The authorization mechanism that the proxy uses.
    auth :: [UserAuthConfig],
    -- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
    -- access secrets in Amazon Web Services Secrets Manager.
    roleArn :: Prelude.Text,
    -- | One or more VPC subnet IDs to associate with the new proxy.
    vpcSubnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDBProxy_tags' - An optional set of key-value pairs to associate arbitrary data of your
-- choosing with the proxy.
--
-- 'vpcSecurityGroupIds', 'createDBProxy_vpcSecurityGroupIds' - One or more VPC security group IDs to associate with the new proxy.
--
-- 'requireTLS', 'createDBProxy_requireTLS' - A Boolean parameter that specifies whether Transport Layer Security
-- (TLS) encryption is required for connections to the proxy. By enabling
-- this setting, you can enforce encrypted TLS connections to the proxy.
--
-- 'debugLogging', 'createDBProxy_debugLogging' - Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
--
-- 'idleClientTimeout', 'createDBProxy_idleClientTimeout' - The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
--
-- 'dbProxyName', 'createDBProxy_dbProxyName' - The identifier for the proxy. This name must be unique for all proxies
-- owned by your Amazon Web Services account in the specified Amazon Web
-- Services Region. An identifier must begin with a letter and must contain
-- only ASCII letters, digits, and hyphens; it can\'t end with a hyphen or
-- contain two consecutive hyphens.
--
-- 'engineFamily', 'createDBProxy_engineFamily' - The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. For Aurora MySQL,
-- RDS for MariaDB, and RDS for MySQL databases, specify @MYSQL@. For
-- Aurora PostgreSQL and RDS for PostgreSQL databases, specify
-- @POSTGRESQL@. For RDS for Microsoft SQL Server, specify @SQLSERVER@.
--
-- 'auth', 'createDBProxy_auth' - The authorization mechanism that the proxy uses.
--
-- 'roleArn', 'createDBProxy_roleArn' - The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in Amazon Web Services Secrets Manager.
--
-- 'vpcSubnetIds', 'createDBProxy_vpcSubnetIds' - One or more VPC subnet IDs to associate with the new proxy.
newCreateDBProxy ::
  -- | 'dbProxyName'
  Prelude.Text ->
  -- | 'engineFamily'
  EngineFamily ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateDBProxy
newCreateDBProxy
  pDBProxyName_
  pEngineFamily_
  pRoleArn_ =
    CreateDBProxy'
      { tags = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        requireTLS = Prelude.Nothing,
        debugLogging = Prelude.Nothing,
        idleClientTimeout = Prelude.Nothing,
        dbProxyName = pDBProxyName_,
        engineFamily = pEngineFamily_,
        auth = Prelude.mempty,
        roleArn = pRoleArn_,
        vpcSubnetIds = Prelude.mempty
      }

-- | An optional set of key-value pairs to associate arbitrary data of your
-- choosing with the proxy.
createDBProxy_tags :: Lens.Lens' CreateDBProxy (Prelude.Maybe [Tag])
createDBProxy_tags = Lens.lens (\CreateDBProxy' {tags} -> tags) (\s@CreateDBProxy' {} a -> s {tags = a} :: CreateDBProxy) Prelude.. Lens.mapping Lens.coerced

-- | One or more VPC security group IDs to associate with the new proxy.
createDBProxy_vpcSecurityGroupIds :: Lens.Lens' CreateDBProxy (Prelude.Maybe [Prelude.Text])
createDBProxy_vpcSecurityGroupIds = Lens.lens (\CreateDBProxy' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBProxy' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBProxy) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean parameter that specifies whether Transport Layer Security
-- (TLS) encryption is required for connections to the proxy. By enabling
-- this setting, you can enforce encrypted TLS connections to the proxy.
createDBProxy_requireTLS :: Lens.Lens' CreateDBProxy (Prelude.Maybe Prelude.Bool)
createDBProxy_requireTLS = Lens.lens (\CreateDBProxy' {requireTLS} -> requireTLS) (\s@CreateDBProxy' {} a -> s {requireTLS = a} :: CreateDBProxy)

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
createDBProxy_debugLogging :: Lens.Lens' CreateDBProxy (Prelude.Maybe Prelude.Bool)
createDBProxy_debugLogging = Lens.lens (\CreateDBProxy' {debugLogging} -> debugLogging) (\s@CreateDBProxy' {} a -> s {debugLogging = a} :: CreateDBProxy)

-- | The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
createDBProxy_idleClientTimeout :: Lens.Lens' CreateDBProxy (Prelude.Maybe Prelude.Int)
createDBProxy_idleClientTimeout = Lens.lens (\CreateDBProxy' {idleClientTimeout} -> idleClientTimeout) (\s@CreateDBProxy' {} a -> s {idleClientTimeout = a} :: CreateDBProxy)

-- | The identifier for the proxy. This name must be unique for all proxies
-- owned by your Amazon Web Services account in the specified Amazon Web
-- Services Region. An identifier must begin with a letter and must contain
-- only ASCII letters, digits, and hyphens; it can\'t end with a hyphen or
-- contain two consecutive hyphens.
createDBProxy_dbProxyName :: Lens.Lens' CreateDBProxy Prelude.Text
createDBProxy_dbProxyName = Lens.lens (\CreateDBProxy' {dbProxyName} -> dbProxyName) (\s@CreateDBProxy' {} a -> s {dbProxyName = a} :: CreateDBProxy)

-- | The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. For Aurora MySQL,
-- RDS for MariaDB, and RDS for MySQL databases, specify @MYSQL@. For
-- Aurora PostgreSQL and RDS for PostgreSQL databases, specify
-- @POSTGRESQL@. For RDS for Microsoft SQL Server, specify @SQLSERVER@.
createDBProxy_engineFamily :: Lens.Lens' CreateDBProxy EngineFamily
createDBProxy_engineFamily = Lens.lens (\CreateDBProxy' {engineFamily} -> engineFamily) (\s@CreateDBProxy' {} a -> s {engineFamily = a} :: CreateDBProxy)

-- | The authorization mechanism that the proxy uses.
createDBProxy_auth :: Lens.Lens' CreateDBProxy [UserAuthConfig]
createDBProxy_auth = Lens.lens (\CreateDBProxy' {auth} -> auth) (\s@CreateDBProxy' {} a -> s {auth = a} :: CreateDBProxy) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in Amazon Web Services Secrets Manager.
createDBProxy_roleArn :: Lens.Lens' CreateDBProxy Prelude.Text
createDBProxy_roleArn = Lens.lens (\CreateDBProxy' {roleArn} -> roleArn) (\s@CreateDBProxy' {} a -> s {roleArn = a} :: CreateDBProxy)

-- | One or more VPC subnet IDs to associate with the new proxy.
createDBProxy_vpcSubnetIds :: Lens.Lens' CreateDBProxy [Prelude.Text]
createDBProxy_vpcSubnetIds = Lens.lens (\CreateDBProxy' {vpcSubnetIds} -> vpcSubnetIds) (\s@CreateDBProxy' {} a -> s {vpcSubnetIds = a} :: CreateDBProxy) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDBProxy where
  type
    AWSResponse CreateDBProxy =
      CreateDBProxyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBProxyResult"
      ( \s h x ->
          CreateDBProxyResponse'
            Prelude.<$> (x Core..@? "DBProxy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBProxy where
  hashWithSalt _salt CreateDBProxy' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` requireTLS
      `Prelude.hashWithSalt` debugLogging
      `Prelude.hashWithSalt` idleClientTimeout
      `Prelude.hashWithSalt` dbProxyName
      `Prelude.hashWithSalt` engineFamily
      `Prelude.hashWithSalt` auth
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` vpcSubnetIds

instance Prelude.NFData CreateDBProxy where
  rnf CreateDBProxy' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf requireTLS
      `Prelude.seq` Prelude.rnf debugLogging
      `Prelude.seq` Prelude.rnf idleClientTimeout
      `Prelude.seq` Prelude.rnf dbProxyName
      `Prelude.seq` Prelude.rnf engineFamily
      `Prelude.seq` Prelude.rnf auth
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf vpcSubnetIds

instance Core.ToHeaders CreateDBProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDBProxy where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDBProxy where
  toQuery CreateDBProxy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateDBProxy" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "RequireTLS" Core.=: requireTLS,
        "DebugLogging" Core.=: debugLogging,
        "IdleClientTimeout" Core.=: idleClientTimeout,
        "DBProxyName" Core.=: dbProxyName,
        "EngineFamily" Core.=: engineFamily,
        "Auth" Core.=: Core.toQueryList "member" auth,
        "RoleArn" Core.=: roleArn,
        "VpcSubnetIds"
          Core.=: Core.toQueryList "member" vpcSubnetIds
      ]

-- | /See:/ 'newCreateDBProxyResponse' smart constructor.
data CreateDBProxyResponse = CreateDBProxyResponse'
  { -- | The @DBProxy@ structure corresponding to the new proxy.
    dbProxy :: Prelude.Maybe DBProxy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBProxyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxy', 'createDBProxyResponse_dbProxy' - The @DBProxy@ structure corresponding to the new proxy.
--
-- 'httpStatus', 'createDBProxyResponse_httpStatus' - The response's http status code.
newCreateDBProxyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBProxyResponse
newCreateDBProxyResponse pHttpStatus_ =
  CreateDBProxyResponse'
    { dbProxy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DBProxy@ structure corresponding to the new proxy.
createDBProxyResponse_dbProxy :: Lens.Lens' CreateDBProxyResponse (Prelude.Maybe DBProxy)
createDBProxyResponse_dbProxy = Lens.lens (\CreateDBProxyResponse' {dbProxy} -> dbProxy) (\s@CreateDBProxyResponse' {} a -> s {dbProxy = a} :: CreateDBProxyResponse)

-- | The response's http status code.
createDBProxyResponse_httpStatus :: Lens.Lens' CreateDBProxyResponse Prelude.Int
createDBProxyResponse_httpStatus = Lens.lens (\CreateDBProxyResponse' {httpStatus} -> httpStatus) (\s@CreateDBProxyResponse' {} a -> s {httpStatus = a} :: CreateDBProxyResponse)

instance Prelude.NFData CreateDBProxyResponse where
  rnf CreateDBProxyResponse' {..} =
    Prelude.rnf dbProxy
      `Prelude.seq` Prelude.rnf httpStatus
