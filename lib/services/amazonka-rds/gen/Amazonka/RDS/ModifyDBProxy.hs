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
-- Module      : Amazonka.RDS.ModifyDBProxy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings for an existing DB proxy.
module Amazonka.RDS.ModifyDBProxy
  ( -- * Creating a Request
    ModifyDBProxy (..),
    newModifyDBProxy,

    -- * Request Lenses
    modifyDBProxy_auth,
    modifyDBProxy_debugLogging,
    modifyDBProxy_idleClientTimeout,
    modifyDBProxy_newDBProxyName,
    modifyDBProxy_requireTLS,
    modifyDBProxy_roleArn,
    modifyDBProxy_securityGroups,
    modifyDBProxy_dbProxyName,

    -- * Destructuring the Response
    ModifyDBProxyResponse (..),
    newModifyDBProxyResponse,

    -- * Response Lenses
    modifyDBProxyResponse_dbProxy,
    modifyDBProxyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBProxy' smart constructor.
data ModifyDBProxy = ModifyDBProxy'
  { -- | The new authentication settings for the @DBProxy@.
    auth :: Prelude.Maybe [UserAuthConfig],
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
    -- | The new identifier for the @DBProxy@. An identifier must begin with a
    -- letter and must contain only ASCII letters, digits, and hyphens; it
    -- can\'t end with a hyphen or contain two consecutive hyphens.
    newDBProxyName' :: Prelude.Maybe Prelude.Text,
    -- | Whether Transport Layer Security (TLS) encryption is required for
    -- connections to the proxy. By enabling this setting, you can enforce
    -- encrypted TLS connections to the proxy, even if the associated database
    -- doesn\'t use TLS.
    requireTLS :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
    -- access secrets in Amazon Web Services Secrets Manager.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The new list of security groups for the @DBProxy@.
    securityGroups :: Prelude.Maybe [Prelude.Text],
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
-- 'auth', 'modifyDBProxy_auth' - The new authentication settings for the @DBProxy@.
--
-- 'debugLogging', 'modifyDBProxy_debugLogging' - Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
--
-- 'idleClientTimeout', 'modifyDBProxy_idleClientTimeout' - The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
--
-- 'newDBProxyName'', 'modifyDBProxy_newDBProxyName' - The new identifier for the @DBProxy@. An identifier must begin with a
-- letter and must contain only ASCII letters, digits, and hyphens; it
-- can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'requireTLS', 'modifyDBProxy_requireTLS' - Whether Transport Layer Security (TLS) encryption is required for
-- connections to the proxy. By enabling this setting, you can enforce
-- encrypted TLS connections to the proxy, even if the associated database
-- doesn\'t use TLS.
--
-- 'roleArn', 'modifyDBProxy_roleArn' - The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in Amazon Web Services Secrets Manager.
--
-- 'securityGroups', 'modifyDBProxy_securityGroups' - The new list of security groups for the @DBProxy@.
--
-- 'dbProxyName', 'modifyDBProxy_dbProxyName' - The identifier for the @DBProxy@ to modify.
newModifyDBProxy ::
  -- | 'dbProxyName'
  Prelude.Text ->
  ModifyDBProxy
newModifyDBProxy pDBProxyName_ =
  ModifyDBProxy'
    { auth = Prelude.Nothing,
      debugLogging = Prelude.Nothing,
      idleClientTimeout = Prelude.Nothing,
      newDBProxyName' = Prelude.Nothing,
      requireTLS = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | The new authentication settings for the @DBProxy@.
modifyDBProxy_auth :: Lens.Lens' ModifyDBProxy (Prelude.Maybe [UserAuthConfig])
modifyDBProxy_auth = Lens.lens (\ModifyDBProxy' {auth} -> auth) (\s@ModifyDBProxy' {} a -> s {auth = a} :: ModifyDBProxy) Prelude.. Lens.mapping Lens.coerced

-- | Whether the proxy includes detailed information about SQL statements in
-- its logs. This information helps you to debug issues involving SQL
-- behavior or the performance and scalability of the proxy connections.
-- The debug information includes the text of SQL statements that you
-- submit through the proxy. Thus, only enable this setting when needed for
-- debugging, and only when you have security measures in place to
-- safeguard any sensitive information that appears in the logs.
modifyDBProxy_debugLogging :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Bool)
modifyDBProxy_debugLogging = Lens.lens (\ModifyDBProxy' {debugLogging} -> debugLogging) (\s@ModifyDBProxy' {} a -> s {debugLogging = a} :: ModifyDBProxy)

-- | The number of seconds that a connection to the proxy can be inactive
-- before the proxy disconnects it. You can set this value higher or lower
-- than the connection timeout limit for the associated database.
modifyDBProxy_idleClientTimeout :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Int)
modifyDBProxy_idleClientTimeout = Lens.lens (\ModifyDBProxy' {idleClientTimeout} -> idleClientTimeout) (\s@ModifyDBProxy' {} a -> s {idleClientTimeout = a} :: ModifyDBProxy)

-- | The new identifier for the @DBProxy@. An identifier must begin with a
-- letter and must contain only ASCII letters, digits, and hyphens; it
-- can\'t end with a hyphen or contain two consecutive hyphens.
modifyDBProxy_newDBProxyName :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Text)
modifyDBProxy_newDBProxyName = Lens.lens (\ModifyDBProxy' {newDBProxyName'} -> newDBProxyName') (\s@ModifyDBProxy' {} a -> s {newDBProxyName' = a} :: ModifyDBProxy)

-- | Whether Transport Layer Security (TLS) encryption is required for
-- connections to the proxy. By enabling this setting, you can enforce
-- encrypted TLS connections to the proxy, even if the associated database
-- doesn\'t use TLS.
modifyDBProxy_requireTLS :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Bool)
modifyDBProxy_requireTLS = Lens.lens (\ModifyDBProxy' {requireTLS} -> requireTLS) (\s@ModifyDBProxy' {} a -> s {requireTLS = a} :: ModifyDBProxy)

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to
-- access secrets in Amazon Web Services Secrets Manager.
modifyDBProxy_roleArn :: Lens.Lens' ModifyDBProxy (Prelude.Maybe Prelude.Text)
modifyDBProxy_roleArn = Lens.lens (\ModifyDBProxy' {roleArn} -> roleArn) (\s@ModifyDBProxy' {} a -> s {roleArn = a} :: ModifyDBProxy)

-- | The new list of security groups for the @DBProxy@.
modifyDBProxy_securityGroups :: Lens.Lens' ModifyDBProxy (Prelude.Maybe [Prelude.Text])
modifyDBProxy_securityGroups = Lens.lens (\ModifyDBProxy' {securityGroups} -> securityGroups) (\s@ModifyDBProxy' {} a -> s {securityGroups = a} :: ModifyDBProxy) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the @DBProxy@ to modify.
modifyDBProxy_dbProxyName :: Lens.Lens' ModifyDBProxy Prelude.Text
modifyDBProxy_dbProxyName = Lens.lens (\ModifyDBProxy' {dbProxyName} -> dbProxyName) (\s@ModifyDBProxy' {} a -> s {dbProxyName = a} :: ModifyDBProxy)

instance Core.AWSRequest ModifyDBProxy where
  type
    AWSResponse ModifyDBProxy =
      ModifyDBProxyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBProxyResult"
      ( \s h x ->
          ModifyDBProxyResponse'
            Prelude.<$> (x Data..@? "DBProxy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBProxy where
  hashWithSalt _salt ModifyDBProxy' {..} =
    _salt
      `Prelude.hashWithSalt` auth
      `Prelude.hashWithSalt` debugLogging
      `Prelude.hashWithSalt` idleClientTimeout
      `Prelude.hashWithSalt` newDBProxyName'
      `Prelude.hashWithSalt` requireTLS
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` dbProxyName

instance Prelude.NFData ModifyDBProxy where
  rnf ModifyDBProxy' {..} =
    Prelude.rnf auth `Prelude.seq`
      Prelude.rnf debugLogging `Prelude.seq`
        Prelude.rnf idleClientTimeout `Prelude.seq`
          Prelude.rnf newDBProxyName' `Prelude.seq`
            Prelude.rnf requireTLS `Prelude.seq`
              Prelude.rnf roleArn `Prelude.seq`
                Prelude.rnf securityGroups `Prelude.seq`
                  Prelude.rnf dbProxyName

instance Data.ToHeaders ModifyDBProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBProxy where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBProxy where
  toQuery ModifyDBProxy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBProxy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Auth"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> auth),
        "DebugLogging" Data.=: debugLogging,
        "IdleClientTimeout" Data.=: idleClientTimeout,
        "NewDBProxyName" Data.=: newDBProxyName',
        "RequireTLS" Data.=: requireTLS,
        "RoleArn" Data.=: roleArn,
        "SecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> securityGroups
            ),
        "DBProxyName" Data.=: dbProxyName
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

instance Prelude.NFData ModifyDBProxyResponse where
  rnf ModifyDBProxyResponse' {..} =
    Prelude.rnf dbProxy `Prelude.seq`
      Prelude.rnf httpStatus
