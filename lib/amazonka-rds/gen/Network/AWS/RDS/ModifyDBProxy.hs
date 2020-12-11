{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings for an existing DB proxy.
module Network.AWS.RDS.ModifyDBProxy
  ( -- * Creating a request
    ModifyDBProxy (..),
    mkModifyDBProxy,

    -- ** Request lenses
    mdpDebugLogging,
    mdpSecurityGroups,
    mdpAuth,
    mdpRequireTLS,
    mdpIdleClientTimeout,
    mdpNewDBProxyName,
    mdpRoleARN,
    mdpDBProxyName,

    -- * Destructuring the response
    ModifyDBProxyResponse (..),
    mkModifyDBProxyResponse,

    -- ** Response lenses
    mdprsDBProxy,
    mdprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyDBProxy' smart constructor.
data ModifyDBProxy = ModifyDBProxy'
  { debugLogging ::
      Lude.Maybe Lude.Bool,
    securityGroups :: Lude.Maybe [Lude.Text],
    auth :: Lude.Maybe [UserAuthConfig],
    requireTLS :: Lude.Maybe Lude.Bool,
    idleClientTimeout :: Lude.Maybe Lude.Int,
    newDBProxyName :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    dbProxyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBProxy' with the minimum fields required to make a request.
--
-- * 'auth' - The new authentication settings for the @DBProxy@ .
-- * 'dbProxyName' - The identifier for the @DBProxy@ to modify.
-- * 'debugLogging' - Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
-- * 'idleClientTimeout' - The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
-- * 'newDBProxyName' - The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
-- * 'requireTLS' - Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
-- * 'securityGroups' - The new list of security groups for the @DBProxy@ .
mkModifyDBProxy ::
  -- | 'dbProxyName'
  Lude.Text ->
  ModifyDBProxy
mkModifyDBProxy pDBProxyName_ =
  ModifyDBProxy'
    { debugLogging = Lude.Nothing,
      securityGroups = Lude.Nothing,
      auth = Lude.Nothing,
      requireTLS = Lude.Nothing,
      idleClientTimeout = Lude.Nothing,
      newDBProxyName = Lude.Nothing,
      roleARN = Lude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- /Note:/ Consider using 'debugLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpDebugLogging :: Lens.Lens' ModifyDBProxy (Lude.Maybe Lude.Bool)
mdpDebugLogging = Lens.lens (debugLogging :: ModifyDBProxy -> Lude.Maybe Lude.Bool) (\s a -> s {debugLogging = a} :: ModifyDBProxy)
{-# DEPRECATED mdpDebugLogging "Use generic-lens or generic-optics with 'debugLogging' instead." #-}

-- | The new list of security groups for the @DBProxy@ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpSecurityGroups :: Lens.Lens' ModifyDBProxy (Lude.Maybe [Lude.Text])
mdpSecurityGroups = Lens.lens (securityGroups :: ModifyDBProxy -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: ModifyDBProxy)
{-# DEPRECATED mdpSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The new authentication settings for the @DBProxy@ .
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpAuth :: Lens.Lens' ModifyDBProxy (Lude.Maybe [UserAuthConfig])
mdpAuth = Lens.lens (auth :: ModifyDBProxy -> Lude.Maybe [UserAuthConfig]) (\s a -> s {auth = a} :: ModifyDBProxy)
{-# DEPRECATED mdpAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
--
-- /Note:/ Consider using 'requireTLS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpRequireTLS :: Lens.Lens' ModifyDBProxy (Lude.Maybe Lude.Bool)
mdpRequireTLS = Lens.lens (requireTLS :: ModifyDBProxy -> Lude.Maybe Lude.Bool) (\s a -> s {requireTLS = a} :: ModifyDBProxy)
{-# DEPRECATED mdpRequireTLS "Use generic-lens or generic-optics with 'requireTLS' instead." #-}

-- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
--
-- /Note:/ Consider using 'idleClientTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpIdleClientTimeout :: Lens.Lens' ModifyDBProxy (Lude.Maybe Lude.Int)
mdpIdleClientTimeout = Lens.lens (idleClientTimeout :: ModifyDBProxy -> Lude.Maybe Lude.Int) (\s a -> s {idleClientTimeout = a} :: ModifyDBProxy)
{-# DEPRECATED mdpIdleClientTimeout "Use generic-lens or generic-optics with 'idleClientTimeout' instead." #-}

-- | The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'newDBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpNewDBProxyName :: Lens.Lens' ModifyDBProxy (Lude.Maybe Lude.Text)
mdpNewDBProxyName = Lens.lens (newDBProxyName :: ModifyDBProxy -> Lude.Maybe Lude.Text) (\s a -> s {newDBProxyName = a} :: ModifyDBProxy)
{-# DEPRECATED mdpNewDBProxyName "Use generic-lens or generic-optics with 'newDBProxyName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpRoleARN :: Lens.Lens' ModifyDBProxy (Lude.Maybe Lude.Text)
mdpRoleARN = Lens.lens (roleARN :: ModifyDBProxy -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ModifyDBProxy)
{-# DEPRECATED mdpRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The identifier for the @DBProxy@ to modify.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpDBProxyName :: Lens.Lens' ModifyDBProxy Lude.Text
mdpDBProxyName = Lens.lens (dbProxyName :: ModifyDBProxy -> Lude.Text) (\s a -> s {dbProxyName = a} :: ModifyDBProxy)
{-# DEPRECATED mdpDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

instance Lude.AWSRequest ModifyDBProxy where
  type Rs ModifyDBProxy = ModifyDBProxyResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBProxyResult"
      ( \s h x ->
          ModifyDBProxyResponse'
            Lude.<$> (x Lude..@? "DBProxy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBProxy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBProxy where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBProxy where
  toQuery ModifyDBProxy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBProxy" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DebugLogging" Lude.=: debugLogging,
        "SecurityGroups"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> securityGroups),
        "Auth"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> auth),
        "RequireTLS" Lude.=: requireTLS,
        "IdleClientTimeout" Lude.=: idleClientTimeout,
        "NewDBProxyName" Lude.=: newDBProxyName,
        "RoleArn" Lude.=: roleARN,
        "DBProxyName" Lude.=: dbProxyName
      ]

-- | /See:/ 'mkModifyDBProxyResponse' smart constructor.
data ModifyDBProxyResponse = ModifyDBProxyResponse'
  { dbProxy ::
      Lude.Maybe DBProxy,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBProxyResponse' with the minimum fields required to make a request.
--
-- * 'dbProxy' - The @DBProxy@ object representing the new settings for the proxy.
-- * 'responseStatus' - The response status code.
mkModifyDBProxyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBProxyResponse
mkModifyDBProxyResponse pResponseStatus_ =
  ModifyDBProxyResponse'
    { dbProxy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @DBProxy@ object representing the new settings for the proxy.
--
-- /Note:/ Consider using 'dbProxy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdprsDBProxy :: Lens.Lens' ModifyDBProxyResponse (Lude.Maybe DBProxy)
mdprsDBProxy = Lens.lens (dbProxy :: ModifyDBProxyResponse -> Lude.Maybe DBProxy) (\s a -> s {dbProxy = a} :: ModifyDBProxyResponse)
{-# DEPRECATED mdprsDBProxy "Use generic-lens or generic-optics with 'dbProxy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdprsResponseStatus :: Lens.Lens' ModifyDBProxyResponse Lude.Int
mdprsResponseStatus = Lens.lens (responseStatus :: ModifyDBProxyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBProxyResponse)
{-# DEPRECATED mdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
