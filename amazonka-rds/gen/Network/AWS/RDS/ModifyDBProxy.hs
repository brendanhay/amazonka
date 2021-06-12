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
-- Module      : Network.AWS.RDS.ModifyDBProxy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings for an existing DB proxy.
module Network.AWS.RDS.ModifyDBProxy
  ( -- * Creating a Request
    ModifyDBProxy (..),
    newModifyDBProxy,

    -- * Request Lenses
    modifyDBProxy_roleArn,
    modifyDBProxy_newDBProxyName,
    modifyDBProxy_idleClientTimeout,
    modifyDBProxy_auth,
    modifyDBProxy_securityGroups,
    modifyDBProxy_debugLogging,
    modifyDBProxy_requireTLS,
    modifyDBProxy_dbProxyName,

    -- * Destructuring the Response
    ModifyDBProxyResponse (..),
    newModifyDBProxyResponse,

    -- * Response Lenses
    modifyDBProxyResponse_dbProxy,
    modifyDBProxyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyDBProxy' smart constructor.
data ModifyDBProxy = ModifyDBProxy'
  { -- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
    -- access secrets in AWS Secrets Manager.
    roleArn :: Core.Maybe Core.Text,
    -- | The new identifier for the @DBProxy@. An identifier must begin with a
    -- letter and must contain only ASCII letters, digits, and hyphens; it
    -- can\'t end with a hyphen or contain two consecutive hyphens.
    newDBProxyName' :: Core.Maybe Core.Text,
    -- | The number of seconds that a connection to the proxy can be inactive
    -- before the proxy disconnects it. You can set this value higher or lower
    -- than the connection timeout limit for the associated database.
    idleClientTimeout :: Core.Maybe Core.Int,
    -- | The new authentication settings for the @DBProxy@.
    auth :: Core.Maybe [UserAuthConfig],
    -- | The new list of security groups for the @DBProxy@.
    securityGroups :: Core.Maybe [Core.Text],
    -- | Whether the proxy includes detailed information about SQL statements in
    -- its logs. This information helps you to debug issues involving SQL
    -- behavior or the performance and scalability of the proxy connections.
    -- The debug information includes the text of SQL statements that you
    -- submit through the proxy. Thus, only enable this setting when needed for
    -- debugging, and only when you have security measures in place to
    -- safeguard any sensitive information that appears in the logs.
    debugLogging :: Core.Maybe Core.Bool,
    -- | Whether Transport Layer Security (TLS) encryption is required for
    -- connections to the proxy. By enabling this setting, you can enforce
    -- encrypted TLS connections to the proxy, even if the associated database
    -- doesn\'t use TLS.
    requireTLS :: Core.Maybe Core.Bool,
    -- | The identifier for the @DBProxy@ to modify.
    dbProxyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'modifyDBProxy_roleArn' - The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in AWS Secrets Manager.
--
-- 'newDBProxyName'', 'modifyDBProxy_newDBProxyName' - The new identifier for the @DBProxy@. An identifier must begin with a
-- letter and must contain only ASCII letters, digits, and hyphens; it
-- can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'idleClientTimeout', 'modifyDBProxy_idleClientTimeout' - The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
--
-- 'auth', 'modifyDBProxy_auth' - The new authentication settings for the @DBProxy@.
--
-- 'securityGroups', 'modifyDBProxy_securityGroups' - The new list of security groups for the @DBProxy@.
--
-- 'debugLogging', 'modifyDBProxy_debugLogging' - Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
--
-- 'requireTLS', 'modifyDBProxy_requireTLS' - Whether Transport Layer Security (TLS) encryption is required for
-- connections to the proxy. By enabling this setting, you can enforce
-- encrypted TLS connections to the proxy, even if the associated database
-- doesn\'t use TLS.
--
-- 'dbProxyName', 'modifyDBProxy_dbProxyName' - The identifier for the @DBProxy@ to modify.
newModifyDBProxy ::
  -- | 'dbProxyName'
  Core.Text ->
  ModifyDBProxy
newModifyDBProxy pDBProxyName_ =
  ModifyDBProxy'
    { roleArn = Core.Nothing,
      newDBProxyName' = Core.Nothing,
      idleClientTimeout = Core.Nothing,
      auth = Core.Nothing,
      securityGroups = Core.Nothing,
      debugLogging = Core.Nothing,
      requireTLS = Core.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in AWS Secrets Manager.
modifyDBProxy_roleArn :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Text)
modifyDBProxy_roleArn = Lens.lens (\ModifyDBProxy' {roleArn} -> roleArn) (\s@ModifyDBProxy' {} a -> s {roleArn = a} :: ModifyDBProxy)

-- | The new identifier for the @DBProxy@. An identifier must begin with a
-- letter and must contain only ASCII letters, digits, and hyphens; it
-- can\'t end with a hyphen or contain two consecutive hyphens.
modifyDBProxy_newDBProxyName :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Text)
modifyDBProxy_newDBProxyName = Lens.lens (\ModifyDBProxy' {newDBProxyName'} -> newDBProxyName') (\s@ModifyDBProxy' {} a -> s {newDBProxyName' = a} :: ModifyDBProxy)

-- | The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
modifyDBProxy_idleClientTimeout :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Int)
modifyDBProxy_idleClientTimeout = Lens.lens (\ModifyDBProxy' {idleClientTimeout} -> idleClientTimeout) (\s@ModifyDBProxy' {} a -> s {idleClientTimeout = a} :: ModifyDBProxy)

-- | The new authentication settings for the @DBProxy@.
modifyDBProxy_auth :: Lens.Lens' ModifyDBProxy (Core.Maybe [UserAuthConfig])
modifyDBProxy_auth = Lens.lens (\ModifyDBProxy' {auth} -> auth) (\s@ModifyDBProxy' {} a -> s {auth = a} :: ModifyDBProxy) Core.. Lens.mapping Lens._Coerce

-- | The new list of security groups for the @DBProxy@.
modifyDBProxy_securityGroups :: Lens.Lens' ModifyDBProxy (Core.Maybe [Core.Text])
modifyDBProxy_securityGroups = Lens.lens (\ModifyDBProxy' {securityGroups} -> securityGroups) (\s@ModifyDBProxy' {} a -> s {securityGroups = a} :: ModifyDBProxy) Core.. Lens.mapping Lens._Coerce

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
modifyDBProxy_debugLogging :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Bool)
modifyDBProxy_debugLogging = Lens.lens (\ModifyDBProxy' {debugLogging} -> debugLogging) (\s@ModifyDBProxy' {} a -> s {debugLogging = a} :: ModifyDBProxy)

-- | Whether Transport Layer Security (TLS) encryption is required for
-- connections to the proxy. By enabling this setting, you can enforce
-- encrypted TLS connections to the proxy, even if the associated database
-- doesn\'t use TLS.
modifyDBProxy_requireTLS :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Bool)
modifyDBProxy_requireTLS = Lens.lens (\ModifyDBProxy' {requireTLS} -> requireTLS) (\s@ModifyDBProxy' {} a -> s {requireTLS = a} :: ModifyDBProxy)

-- | The identifier for the @DBProxy@ to modify.
modifyDBProxy_dbProxyName :: Lens.Lens' ModifyDBProxy Core.Text
modifyDBProxy_dbProxyName = Lens.lens (\ModifyDBProxy' {dbProxyName} -> dbProxyName) (\s@ModifyDBProxy' {} a -> s {dbProxyName = a} :: ModifyDBProxy)

instance Core.AWSRequest ModifyDBProxy where
  type
    AWSResponse ModifyDBProxy =
      ModifyDBProxyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBProxyResult"
      ( \s h x ->
          ModifyDBProxyResponse'
            Core.<$> (x Core..@? "DBProxy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyDBProxy

instance Core.NFData ModifyDBProxy

instance Core.ToHeaders ModifyDBProxy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyDBProxy where
  toPath = Core.const "/"

instance Core.ToQuery ModifyDBProxy where
  toQuery ModifyDBProxy' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyDBProxy" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "RoleArn" Core.=: roleArn,
        "NewDBProxyName" Core.=: newDBProxyName',
        "IdleClientTimeout" Core.=: idleClientTimeout,
        "Auth"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> auth),
        "SecurityGroups"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> securityGroups),
        "DebugLogging" Core.=: debugLogging,
        "RequireTLS" Core.=: requireTLS,
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newModifyDBProxyResponse' smart constructor.
data ModifyDBProxyResponse = ModifyDBProxyResponse'
  { -- | The @DBProxy@ object representing the new settings for the proxy.
    dbProxy :: Core.Maybe DBProxy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBProxyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxy', 'modifyDBProxyResponse_dbProxy' - The @DBProxy@ object representing the new settings for the proxy.
--
-- 'httpStatus', 'modifyDBProxyResponse_httpStatus' - The response's http status code.
newModifyDBProxyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyDBProxyResponse
newModifyDBProxyResponse pHttpStatus_ =
  ModifyDBProxyResponse'
    { dbProxy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DBProxy@ object representing the new settings for the proxy.
modifyDBProxyResponse_dbProxy :: Lens.Lens' ModifyDBProxyResponse (Core.Maybe DBProxy)
modifyDBProxyResponse_dbProxy = Lens.lens (\ModifyDBProxyResponse' {dbProxy} -> dbProxy) (\s@ModifyDBProxyResponse' {} a -> s {dbProxy = a} :: ModifyDBProxyResponse)

-- | The response's http status code.
modifyDBProxyResponse_httpStatus :: Lens.Lens' ModifyDBProxyResponse Core.Int
modifyDBProxyResponse_httpStatus = Lens.lens (\ModifyDBProxyResponse' {httpStatus} -> httpStatus) (\s@ModifyDBProxyResponse' {} a -> s {httpStatus = a} :: ModifyDBProxyResponse)

instance Core.NFData ModifyDBProxyResponse
