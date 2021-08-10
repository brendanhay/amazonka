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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyDBProxy' smart constructor.
data ModifyDBProxy = ModifyDBProxy'
  { -- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
    -- access secrets in AWS Secrets Manager.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The new identifier for the @DBProxy@. An identifier must begin with a
    -- letter and must contain only ASCII letters, digits, and hyphens; it
    -- can\'t end with a hyphen or contain two consecutive hyphens.
    newDBProxyName' :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds that a connection to the proxy can be inactive
    -- before the proxy disconnects it. You can set this value higher or lower
    -- than the connection timeout limit for the associated database.
    idleClientTimeout :: Prelude.Maybe Prelude.Int,
    -- | The new authentication settings for the @DBProxy@.
    auth :: Prelude.Maybe [UserAuthConfig],
    -- | The new list of security groups for the @DBProxy@.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Whether the proxy includes detailed information about SQL statements in
    -- its logs. This information helps you to debug issues involving SQL
    -- behavior or the performance and scalability of the proxy connections.
    -- The debug information includes the text of SQL statements that you
    -- submit through the proxy. Thus, only enable this setting when needed for
    -- debugging, and only when you have security measures in place to
    -- safeguard any sensitive information that appears in the logs.
    debugLogging :: Prelude.Maybe Prelude.Bool,
    -- | Whether Transport Layer Security (TLS) encryption is required for
    -- connections to the proxy. By enabling this setting, you can enforce
    -- encrypted TLS connections to the proxy, even if the associated database
    -- doesn\'t use TLS.
    requireTLS :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the @DBProxy@ to modify.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ModifyDBProxy
newModifyDBProxy pDBProxyName_ =
  ModifyDBProxy'
    { roleArn = Prelude.Nothing,
      newDBProxyName' = Prelude.Nothing,
      idleClientTimeout = Prelude.Nothing,
      auth = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      debugLogging = Prelude.Nothing,
      requireTLS = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in AWS Secrets Manager.
modifyDBProxy_roleArn :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Text)
modifyDBProxy_roleArn = Lens.lens (\ModifyDBProxy' {roleArn} -> roleArn) (\s@ModifyDBProxy' {} a -> s {roleArn = a} :: ModifyDBProxy)

-- | The new identifier for the @DBProxy@. An identifier must begin with a
-- letter and must contain only ASCII letters, digits, and hyphens; it
-- can\'t end with a hyphen or contain two consecutive hyphens.
modifyDBProxy_newDBProxyName :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Text)
modifyDBProxy_newDBProxyName = Lens.lens (\ModifyDBProxy' {newDBProxyName'} -> newDBProxyName') (\s@ModifyDBProxy' {} a -> s {newDBProxyName' = a} :: ModifyDBProxy)

-- | The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
modifyDBProxy_idleClientTimeout :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Int)
modifyDBProxy_idleClientTimeout = Lens.lens (\ModifyDBProxy' {idleClientTimeout} -> idleClientTimeout) (\s@ModifyDBProxy' {} a -> s {idleClientTimeout = a} :: ModifyDBProxy)

-- | The new authentication settings for the @DBProxy@.
modifyDBProxy_auth :: Lens.Lens' ModifyDBProxy (Prelude.Maybe [UserAuthConfig])
modifyDBProxy_auth = Lens.lens (\ModifyDBProxy' {auth} -> auth) (\s@ModifyDBProxy' {} a -> s {auth = a} :: ModifyDBProxy) Prelude.. Lens.mapping Lens._Coerce

-- | The new list of security groups for the @DBProxy@.
modifyDBProxy_securityGroups :: Lens.Lens' ModifyDBProxy (Prelude.Maybe [Prelude.Text])
modifyDBProxy_securityGroups = Lens.lens (\ModifyDBProxy' {securityGroups} -> securityGroups) (\s@ModifyDBProxy' {} a -> s {securityGroups = a} :: ModifyDBProxy) Prelude.. Lens.mapping Lens._Coerce

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
modifyDBProxy_debugLogging :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Bool)
modifyDBProxy_debugLogging = Lens.lens (\ModifyDBProxy' {debugLogging} -> debugLogging) (\s@ModifyDBProxy' {} a -> s {debugLogging = a} :: ModifyDBProxy)

-- | Whether Transport Layer Security (TLS) encryption is required for
-- connections to the proxy. By enabling this setting, you can enforce
-- encrypted TLS connections to the proxy, even if the associated database
-- doesn\'t use TLS.
modifyDBProxy_requireTLS :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Bool)
modifyDBProxy_requireTLS = Lens.lens (\ModifyDBProxy' {requireTLS} -> requireTLS) (\s@ModifyDBProxy' {} a -> s {requireTLS = a} :: ModifyDBProxy)

-- | The identifier for the @DBProxy@ to modify.
modifyDBProxy_dbProxyName :: Lens.Lens' ModifyDBProxy Prelude.Text
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
            Prelude.<$> (x Core..@? "DBProxy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBProxy

instance Prelude.NFData ModifyDBProxy

instance Core.ToHeaders ModifyDBProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBProxy where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBProxy where
  toQuery ModifyDBProxy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyDBProxy" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "RoleArn" Core.=: roleArn,
        "NewDBProxyName" Core.=: newDBProxyName',
        "IdleClientTimeout" Core.=: idleClientTimeout,
        "Auth"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> auth),
        "SecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> securityGroups
            ),
        "DebugLogging" Core.=: debugLogging,
        "RequireTLS" Core.=: requireTLS,
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newModifyDBProxyResponse' smart constructor.
data ModifyDBProxyResponse = ModifyDBProxyResponse'
  { -- | The @DBProxy@ object representing the new settings for the proxy.
    dbProxy :: Prelude.Maybe DBProxy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyDBProxyResponse
newModifyDBProxyResponse pHttpStatus_ =
  ModifyDBProxyResponse'
    { dbProxy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DBProxy@ object representing the new settings for the proxy.
modifyDBProxyResponse_dbProxy :: Lens.Lens' ModifyDBProxyResponse (Prelude.Maybe DBProxy)
modifyDBProxyResponse_dbProxy = Lens.lens (\ModifyDBProxyResponse' {dbProxy} -> dbProxy) (\s@ModifyDBProxyResponse' {} a -> s {dbProxy = a} :: ModifyDBProxyResponse)

-- | The response's http status code.
modifyDBProxyResponse_httpStatus :: Lens.Lens' ModifyDBProxyResponse Prelude.Int
modifyDBProxyResponse_httpStatus = Lens.lens (\ModifyDBProxyResponse' {httpStatus} -> httpStatus) (\s@ModifyDBProxyResponse' {} a -> s {httpStatus = a} :: ModifyDBProxyResponse)

instance Prelude.NFData ModifyDBProxyResponse
