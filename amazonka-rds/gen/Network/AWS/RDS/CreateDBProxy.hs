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
-- Module      : Network.AWS.RDS.CreateDBProxy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB proxy.
module Network.AWS.RDS.CreateDBProxy
  ( -- * Creating a Request
    CreateDBProxy (..),
    newCreateDBProxy,

    -- * Request Lenses
    createDBProxy_idleClientTimeout,
    createDBProxy_vpcSecurityGroupIds,
    createDBProxy_debugLogging,
    createDBProxy_tags,
    createDBProxy_requireTLS,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDBProxy' smart constructor.
data CreateDBProxy = CreateDBProxy'
  { -- | The number of seconds that a connection to the proxy can be inactive
    -- before the proxy disconnects it. You can set this value higher or lower
    -- than the connection timeout limit for the associated database.
    idleClientTimeout :: Core.Maybe Core.Int,
    -- | One or more VPC security group IDs to associate with the new proxy.
    vpcSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | Whether the proxy includes detailed information about SQL statements in
    -- its logs. This information helps you to debug issues involving SQL
    -- behavior or the performance and scalability of the proxy connections.
    -- The debug information includes the text of SQL statements that you
    -- submit through the proxy. Thus, only enable this setting when needed for
    -- debugging, and only when you have security measures in place to
    -- safeguard any sensitive information that appears in the logs.
    debugLogging :: Core.Maybe Core.Bool,
    -- | An optional set of key-value pairs to associate arbitrary data of your
    -- choosing with the proxy.
    tags :: Core.Maybe [Tag],
    -- | A Boolean parameter that specifies whether Transport Layer Security
    -- (TLS) encryption is required for connections to the proxy. By enabling
    -- this setting, you can enforce encrypted TLS connections to the proxy.
    requireTLS :: Core.Maybe Core.Bool,
    -- | The identifier for the proxy. This name must be unique for all proxies
    -- owned by your AWS account in the specified AWS Region. An identifier
    -- must begin with a letter and must contain only ASCII letters, digits,
    -- and hyphens; it can\'t end with a hyphen or contain two consecutive
    -- hyphens.
    dbProxyName :: Core.Text,
    -- | The kinds of databases that the proxy can connect to. This value
    -- determines which database network protocol the proxy recognizes when it
    -- interprets network traffic to and from the database. The engine family
    -- applies to MySQL and PostgreSQL for both RDS and Aurora.
    engineFamily :: EngineFamily,
    -- | The authorization mechanism that the proxy uses.
    auth :: [UserAuthConfig],
    -- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
    -- access secrets in AWS Secrets Manager.
    roleArn :: Core.Text,
    -- | One or more VPC subnet IDs to associate with the new proxy.
    vpcSubnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDBProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idleClientTimeout', 'createDBProxy_idleClientTimeout' - The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
--
-- 'vpcSecurityGroupIds', 'createDBProxy_vpcSecurityGroupIds' - One or more VPC security group IDs to associate with the new proxy.
--
-- 'debugLogging', 'createDBProxy_debugLogging' - Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
--
-- 'tags', 'createDBProxy_tags' - An optional set of key-value pairs to associate arbitrary data of your
-- choosing with the proxy.
--
-- 'requireTLS', 'createDBProxy_requireTLS' - A Boolean parameter that specifies whether Transport Layer Security
-- (TLS) encryption is required for connections to the proxy. By enabling
-- this setting, you can enforce encrypted TLS connections to the proxy.
--
-- 'dbProxyName', 'createDBProxy_dbProxyName' - The identifier for the proxy. This name must be unique for all proxies
-- owned by your AWS account in the specified AWS Region. An identifier
-- must begin with a letter and must contain only ASCII letters, digits,
-- and hyphens; it can\'t end with a hyphen or contain two consecutive
-- hyphens.
--
-- 'engineFamily', 'createDBProxy_engineFamily' - The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. The engine family
-- applies to MySQL and PostgreSQL for both RDS and Aurora.
--
-- 'auth', 'createDBProxy_auth' - The authorization mechanism that the proxy uses.
--
-- 'roleArn', 'createDBProxy_roleArn' - The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in AWS Secrets Manager.
--
-- 'vpcSubnetIds', 'createDBProxy_vpcSubnetIds' - One or more VPC subnet IDs to associate with the new proxy.
newCreateDBProxy ::
  -- | 'dbProxyName'
  Core.Text ->
  -- | 'engineFamily'
  EngineFamily ->
  -- | 'roleArn'
  Core.Text ->
  CreateDBProxy
newCreateDBProxy
  pDBProxyName_
  pEngineFamily_
  pRoleArn_ =
    CreateDBProxy'
      { idleClientTimeout = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing,
        debugLogging = Core.Nothing,
        tags = Core.Nothing,
        requireTLS = Core.Nothing,
        dbProxyName = pDBProxyName_,
        engineFamily = pEngineFamily_,
        auth = Core.mempty,
        roleArn = pRoleArn_,
        vpcSubnetIds = Core.mempty
      }

-- | The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
createDBProxy_idleClientTimeout :: Lens.Lens' CreateDBProxy (Core.Maybe Core.Int)
createDBProxy_idleClientTimeout = Lens.lens (\CreateDBProxy' {idleClientTimeout} -> idleClientTimeout) (\s@CreateDBProxy' {} a -> s {idleClientTimeout = a} :: CreateDBProxy)

-- | One or more VPC security group IDs to associate with the new proxy.
createDBProxy_vpcSecurityGroupIds :: Lens.Lens' CreateDBProxy (Core.Maybe [Core.Text])
createDBProxy_vpcSecurityGroupIds = Lens.lens (\CreateDBProxy' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBProxy' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBProxy) Core.. Lens.mapping Lens._Coerce

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
createDBProxy_debugLogging :: Lens.Lens' CreateDBProxy (Core.Maybe Core.Bool)
createDBProxy_debugLogging = Lens.lens (\CreateDBProxy' {debugLogging} -> debugLogging) (\s@CreateDBProxy' {} a -> s {debugLogging = a} :: CreateDBProxy)

-- | An optional set of key-value pairs to associate arbitrary data of your
-- choosing with the proxy.
createDBProxy_tags :: Lens.Lens' CreateDBProxy (Core.Maybe [Tag])
createDBProxy_tags = Lens.lens (\CreateDBProxy' {tags} -> tags) (\s@CreateDBProxy' {} a -> s {tags = a} :: CreateDBProxy) Core.. Lens.mapping Lens._Coerce

-- | A Boolean parameter that specifies whether Transport Layer Security
-- (TLS) encryption is required for connections to the proxy. By enabling
-- this setting, you can enforce encrypted TLS connections to the proxy.
createDBProxy_requireTLS :: Lens.Lens' CreateDBProxy (Core.Maybe Core.Bool)
createDBProxy_requireTLS = Lens.lens (\CreateDBProxy' {requireTLS} -> requireTLS) (\s@CreateDBProxy' {} a -> s {requireTLS = a} :: CreateDBProxy)

-- | The identifier for the proxy. This name must be unique for all proxies
-- owned by your AWS account in the specified AWS Region. An identifier
-- must begin with a letter and must contain only ASCII letters, digits,
-- and hyphens; it can\'t end with a hyphen or contain two consecutive
-- hyphens.
createDBProxy_dbProxyName :: Lens.Lens' CreateDBProxy Core.Text
createDBProxy_dbProxyName = Lens.lens (\CreateDBProxy' {dbProxyName} -> dbProxyName) (\s@CreateDBProxy' {} a -> s {dbProxyName = a} :: CreateDBProxy)

-- | The kinds of databases that the proxy can connect to. This value
-- determines which database network protocol the proxy recognizes when it
-- interprets network traffic to and from the database. The engine family
-- applies to MySQL and PostgreSQL for both RDS and Aurora.
createDBProxy_engineFamily :: Lens.Lens' CreateDBProxy EngineFamily
createDBProxy_engineFamily = Lens.lens (\CreateDBProxy' {engineFamily} -> engineFamily) (\s@CreateDBProxy' {} a -> s {engineFamily = a} :: CreateDBProxy)

-- | The authorization mechanism that the proxy uses.
createDBProxy_auth :: Lens.Lens' CreateDBProxy [UserAuthConfig]
createDBProxy_auth = Lens.lens (\CreateDBProxy' {auth} -> auth) (\s@CreateDBProxy' {} a -> s {auth = a} :: CreateDBProxy) Core.. Lens._Coerce

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in AWS Secrets Manager.
createDBProxy_roleArn :: Lens.Lens' CreateDBProxy Core.Text
createDBProxy_roleArn = Lens.lens (\CreateDBProxy' {roleArn} -> roleArn) (\s@CreateDBProxy' {} a -> s {roleArn = a} :: CreateDBProxy)

-- | One or more VPC subnet IDs to associate with the new proxy.
createDBProxy_vpcSubnetIds :: Lens.Lens' CreateDBProxy [Core.Text]
createDBProxy_vpcSubnetIds = Lens.lens (\CreateDBProxy' {vpcSubnetIds} -> vpcSubnetIds) (\s@CreateDBProxy' {} a -> s {vpcSubnetIds = a} :: CreateDBProxy) Core.. Lens._Coerce

instance Core.AWSRequest CreateDBProxy where
  type
    AWSResponse CreateDBProxy =
      CreateDBProxyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBProxyResult"
      ( \s h x ->
          CreateDBProxyResponse'
            Core.<$> (x Core..@? "DBProxy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDBProxy

instance Core.NFData CreateDBProxy

instance Core.ToHeaders CreateDBProxy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateDBProxy where
  toPath = Core.const "/"

instance Core.ToQuery CreateDBProxy where
  toQuery CreateDBProxy' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateDBProxy" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "IdleClientTimeout" Core.=: idleClientTimeout,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> vpcSecurityGroupIds
            ),
        "DebugLogging" Core.=: debugLogging,
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "RequireTLS" Core.=: requireTLS,
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
    dbProxy :: Core.Maybe DBProxy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateDBProxyResponse
newCreateDBProxyResponse pHttpStatus_ =
  CreateDBProxyResponse'
    { dbProxy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DBProxy@ structure corresponding to the new proxy.
createDBProxyResponse_dbProxy :: Lens.Lens' CreateDBProxyResponse (Core.Maybe DBProxy)
createDBProxyResponse_dbProxy = Lens.lens (\CreateDBProxyResponse' {dbProxy} -> dbProxy) (\s@CreateDBProxyResponse' {} a -> s {dbProxy = a} :: CreateDBProxyResponse)

-- | The response's http status code.
createDBProxyResponse_httpStatus :: Lens.Lens' CreateDBProxyResponse Core.Int
createDBProxyResponse_httpStatus = Lens.lens (\CreateDBProxyResponse' {httpStatus} -> httpStatus) (\s@CreateDBProxyResponse' {} a -> s {httpStatus = a} :: CreateDBProxyResponse)

instance Core.NFData CreateDBProxyResponse
