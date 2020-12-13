{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB proxy.
module Network.AWS.RDS.CreateDBProxy
  ( -- * Creating a request
    CreateDBProxy (..),
    mkCreateDBProxy,

    -- ** Request lenses
    cdpDebugLogging,
    cdpVPCSubnetIds,
    cdpEngineFamily,
    cdpAuth,
    cdpRequireTLS,
    cdpIdleClientTimeout,
    cdpVPCSecurityGroupIds,
    cdpDBProxyName,
    cdpTags,
    cdpRoleARN,

    -- * Destructuring the response
    CreateDBProxyResponse (..),
    mkCreateDBProxyResponse,

    -- ** Response lenses
    cdprsDBProxy,
    cdprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDBProxy' smart constructor.
data CreateDBProxy = CreateDBProxy'
  { -- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
    debugLogging :: Lude.Maybe Lude.Bool,
    -- | One or more VPC subnet IDs to associate with the new proxy.
    vpcSubnetIds :: [Lude.Text],
    -- | The kinds of databases that the proxy can connect to. This value determines which database network protocol the proxy recognizes when it interprets network traffic to and from the database. The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
    engineFamily :: EngineFamily,
    -- | The authorization mechanism that the proxy uses.
    auth :: [UserAuthConfig],
    -- | A Boolean parameter that specifies whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy.
    requireTLS :: Lude.Maybe Lude.Bool,
    -- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
    idleClientTimeout :: Lude.Maybe Lude.Int,
    -- | One or more VPC security group IDs to associate with the new proxy.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
    dbProxyName :: Lude.Text,
    -- | An optional set of key-value pairs to associate arbitrary data of your choosing with the proxy.
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBProxy' with the minimum fields required to make a request.
--
-- * 'debugLogging' - Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
-- * 'vpcSubnetIds' - One or more VPC subnet IDs to associate with the new proxy.
-- * 'engineFamily' - The kinds of databases that the proxy can connect to. This value determines which database network protocol the proxy recognizes when it interprets network traffic to and from the database. The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
-- * 'auth' - The authorization mechanism that the proxy uses.
-- * 'requireTLS' - A Boolean parameter that specifies whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy.
-- * 'idleClientTimeout' - The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
-- * 'vpcSecurityGroupIds' - One or more VPC security group IDs to associate with the new proxy.
-- * 'dbProxyName' - The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
-- * 'tags' - An optional set of key-value pairs to associate arbitrary data of your choosing with the proxy.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
mkCreateDBProxy ::
  -- | 'engineFamily'
  EngineFamily ->
  -- | 'dbProxyName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateDBProxy
mkCreateDBProxy pEngineFamily_ pDBProxyName_ pRoleARN_ =
  CreateDBProxy'
    { debugLogging = Lude.Nothing,
      vpcSubnetIds = Lude.mempty,
      engineFamily = pEngineFamily_,
      auth = Lude.mempty,
      requireTLS = Lude.Nothing,
      idleClientTimeout = Lude.Nothing,
      vpcSecurityGroupIds = Lude.Nothing,
      dbProxyName = pDBProxyName_,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- /Note:/ Consider using 'debugLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpDebugLogging :: Lens.Lens' CreateDBProxy (Lude.Maybe Lude.Bool)
cdpDebugLogging = Lens.lens (debugLogging :: CreateDBProxy -> Lude.Maybe Lude.Bool) (\s a -> s {debugLogging = a} :: CreateDBProxy)
{-# DEPRECATED cdpDebugLogging "Use generic-lens or generic-optics with 'debugLogging' instead." #-}

-- | One or more VPC subnet IDs to associate with the new proxy.
--
-- /Note:/ Consider using 'vpcSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpVPCSubnetIds :: Lens.Lens' CreateDBProxy [Lude.Text]
cdpVPCSubnetIds = Lens.lens (vpcSubnetIds :: CreateDBProxy -> [Lude.Text]) (\s a -> s {vpcSubnetIds = a} :: CreateDBProxy)
{-# DEPRECATED cdpVPCSubnetIds "Use generic-lens or generic-optics with 'vpcSubnetIds' instead." #-}

-- | The kinds of databases that the proxy can connect to. This value determines which database network protocol the proxy recognizes when it interprets network traffic to and from the database. The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
--
-- /Note:/ Consider using 'engineFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpEngineFamily :: Lens.Lens' CreateDBProxy EngineFamily
cdpEngineFamily = Lens.lens (engineFamily :: CreateDBProxy -> EngineFamily) (\s a -> s {engineFamily = a} :: CreateDBProxy)
{-# DEPRECATED cdpEngineFamily "Use generic-lens or generic-optics with 'engineFamily' instead." #-}

-- | The authorization mechanism that the proxy uses.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpAuth :: Lens.Lens' CreateDBProxy [UserAuthConfig]
cdpAuth = Lens.lens (auth :: CreateDBProxy -> [UserAuthConfig]) (\s a -> s {auth = a} :: CreateDBProxy)
{-# DEPRECATED cdpAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | A Boolean parameter that specifies whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy.
--
-- /Note:/ Consider using 'requireTLS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpRequireTLS :: Lens.Lens' CreateDBProxy (Lude.Maybe Lude.Bool)
cdpRequireTLS = Lens.lens (requireTLS :: CreateDBProxy -> Lude.Maybe Lude.Bool) (\s a -> s {requireTLS = a} :: CreateDBProxy)
{-# DEPRECATED cdpRequireTLS "Use generic-lens or generic-optics with 'requireTLS' instead." #-}

-- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
--
-- /Note:/ Consider using 'idleClientTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpIdleClientTimeout :: Lens.Lens' CreateDBProxy (Lude.Maybe Lude.Int)
cdpIdleClientTimeout = Lens.lens (idleClientTimeout :: CreateDBProxy -> Lude.Maybe Lude.Int) (\s a -> s {idleClientTimeout = a} :: CreateDBProxy)
{-# DEPRECATED cdpIdleClientTimeout "Use generic-lens or generic-optics with 'idleClientTimeout' instead." #-}

-- | One or more VPC security group IDs to associate with the new proxy.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpVPCSecurityGroupIds :: Lens.Lens' CreateDBProxy (Lude.Maybe [Lude.Text])
cdpVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: CreateDBProxy -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: CreateDBProxy)
{-# DEPRECATED cdpVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpDBProxyName :: Lens.Lens' CreateDBProxy Lude.Text
cdpDBProxyName = Lens.lens (dbProxyName :: CreateDBProxy -> Lude.Text) (\s a -> s {dbProxyName = a} :: CreateDBProxy)
{-# DEPRECATED cdpDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

-- | An optional set of key-value pairs to associate arbitrary data of your choosing with the proxy.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpTags :: Lens.Lens' CreateDBProxy (Lude.Maybe [Tag])
cdpTags = Lens.lens (tags :: CreateDBProxy -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBProxy)
{-# DEPRECATED cdpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpRoleARN :: Lens.Lens' CreateDBProxy Lude.Text
cdpRoleARN = Lens.lens (roleARN :: CreateDBProxy -> Lude.Text) (\s a -> s {roleARN = a} :: CreateDBProxy)
{-# DEPRECATED cdpRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateDBProxy where
  type Rs CreateDBProxy = CreateDBProxyResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBProxyResult"
      ( \s h x ->
          CreateDBProxyResponse'
            Lude.<$> (x Lude..@? "DBProxy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBProxy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBProxy where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBProxy where
  toQuery CreateDBProxy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBProxy" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DebugLogging" Lude.=: debugLogging,
        "VpcSubnetIds" Lude.=: Lude.toQueryList "member" vpcSubnetIds,
        "EngineFamily" Lude.=: engineFamily,
        "Auth" Lude.=: Lude.toQueryList "member" auth,
        "RequireTLS" Lude.=: requireTLS,
        "IdleClientTimeout" Lude.=: idleClientTimeout,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> vpcSecurityGroupIds),
        "DBProxyName" Lude.=: dbProxyName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "RoleArn" Lude.=: roleARN
      ]

-- | /See:/ 'mkCreateDBProxyResponse' smart constructor.
data CreateDBProxyResponse = CreateDBProxyResponse'
  { -- | The @DBProxy@ structure corresponding to the new proxy.
    dbProxy :: Lude.Maybe DBProxy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBProxyResponse' with the minimum fields required to make a request.
--
-- * 'dbProxy' - The @DBProxy@ structure corresponding to the new proxy.
-- * 'responseStatus' - The response status code.
mkCreateDBProxyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBProxyResponse
mkCreateDBProxyResponse pResponseStatus_ =
  CreateDBProxyResponse'
    { dbProxy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @DBProxy@ structure corresponding to the new proxy.
--
-- /Note:/ Consider using 'dbProxy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdprsDBProxy :: Lens.Lens' CreateDBProxyResponse (Lude.Maybe DBProxy)
cdprsDBProxy = Lens.lens (dbProxy :: CreateDBProxyResponse -> Lude.Maybe DBProxy) (\s a -> s {dbProxy = a} :: CreateDBProxyResponse)
{-# DEPRECATED cdprsDBProxy "Use generic-lens or generic-optics with 'dbProxy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdprsResponseStatus :: Lens.Lens' CreateDBProxyResponse Lude.Int
cdprsResponseStatus = Lens.lens (responseStatus :: CreateDBProxyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBProxyResponse)
{-# DEPRECATED cdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
