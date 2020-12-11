-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxy
  ( DBProxy (..),

    -- * Smart constructor
    mkDBProxy,

    -- * Lenses
    dpStatus,
    dpDBProxyARN,
    dpDebugLogging,
    dpVPCSubnetIds,
    dpEngineFamily,
    dpAuth,
    dpRequireTLS,
    dpIdleClientTimeout,
    dpUpdatedDate,
    dpCreatedDate,
    dpVPCSecurityGroupIds,
    dpDBProxyName,
    dpEndpoint,
    dpRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.DBProxyStatus
import Network.AWS.RDS.Types.UserAuthConfigInfo

-- | The data structure representing a proxy managed by the RDS Proxy.
--
-- This data type is used as a response element in the @DescribeDBProxies@ action.
--
-- /See:/ 'mkDBProxy' smart constructor.
data DBProxy = DBProxy'
  { status :: Lude.Maybe DBProxyStatus,
    dbProxyARN :: Lude.Maybe Lude.Text,
    debugLogging :: Lude.Maybe Lude.Bool,
    vpcSubnetIds :: Lude.Maybe [Lude.Text],
    engineFamily :: Lude.Maybe Lude.Text,
    auth :: Lude.Maybe [UserAuthConfigInfo],
    requireTLS :: Lude.Maybe Lude.Bool,
    idleClientTimeout :: Lude.Maybe Lude.Int,
    updatedDate :: Lude.Maybe Lude.ISO8601,
    createdDate :: Lude.Maybe Lude.ISO8601,
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    dbProxyName :: Lude.Maybe Lude.Text,
    endpoint :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBProxy' with the minimum fields required to make a request.
--
-- * 'auth' - One or more data structures specifying the authorization mechanism to connect to the associated RDS DB instance or Aurora DB cluster.
-- * 'createdDate' - The date and time when the proxy was first created.
-- * 'dbProxyARN' - The Amazon Resource Name (ARN) for the proxy.
-- * 'dbProxyName' - The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region.
-- * 'debugLogging' - Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
-- * 'endpoint' - The endpoint that you can use to connect to the proxy. You include the endpoint value in the connection string for a database client application.
-- * 'engineFamily' - The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
-- * 'idleClientTimeout' - The number of seconds a connection to the proxy can have no activity before the proxy drops the client connection. The proxy keeps the underlying database connection open and puts it back into the connection pool for reuse by later connection requests.
--
-- Default: 1800 (30 minutes)
-- Constraints: 1 to 28,800
-- * 'requireTLS' - Indicates whether Transport Layer Security (TLS) encryption is required for connections to the proxy.
-- * 'roleARN' - The Amazon Resource Name (ARN) for the IAM role that the proxy uses to access Amazon Secrets Manager.
-- * 'status' - The current status of this proxy. A status of @available@ means the proxy is ready to handle requests. Other values indicate that you must wait for the proxy to be ready, or take some action to resolve an issue.
-- * 'updatedDate' - The date and time when the proxy was last updated.
-- * 'vpcSecurityGroupIds' - Provides a list of VPC security groups that the proxy belongs to.
-- * 'vpcSubnetIds' - The EC2 subnet IDs for the proxy.
mkDBProxy ::
  DBProxy
mkDBProxy =
  DBProxy'
    { status = Lude.Nothing,
      dbProxyARN = Lude.Nothing,
      debugLogging = Lude.Nothing,
      vpcSubnetIds = Lude.Nothing,
      engineFamily = Lude.Nothing,
      auth = Lude.Nothing,
      requireTLS = Lude.Nothing,
      idleClientTimeout = Lude.Nothing,
      updatedDate = Lude.Nothing,
      createdDate = Lude.Nothing,
      vpcSecurityGroupIds = Lude.Nothing,
      dbProxyName = Lude.Nothing,
      endpoint = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The current status of this proxy. A status of @available@ means the proxy is ready to handle requests. Other values indicate that you must wait for the proxy to be ready, or take some action to resolve an issue.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpStatus :: Lens.Lens' DBProxy (Lude.Maybe DBProxyStatus)
dpStatus = Lens.lens (status :: DBProxy -> Lude.Maybe DBProxyStatus) (\s a -> s {status = a} :: DBProxy)
{-# DEPRECATED dpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) for the proxy.
--
-- /Note:/ Consider using 'dbProxyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDBProxyARN :: Lens.Lens' DBProxy (Lude.Maybe Lude.Text)
dpDBProxyARN = Lens.lens (dbProxyARN :: DBProxy -> Lude.Maybe Lude.Text) (\s a -> s {dbProxyARN = a} :: DBProxy)
{-# DEPRECATED dpDBProxyARN "Use generic-lens or generic-optics with 'dbProxyARN' instead." #-}

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- /Note:/ Consider using 'debugLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDebugLogging :: Lens.Lens' DBProxy (Lude.Maybe Lude.Bool)
dpDebugLogging = Lens.lens (debugLogging :: DBProxy -> Lude.Maybe Lude.Bool) (\s a -> s {debugLogging = a} :: DBProxy)
{-# DEPRECATED dpDebugLogging "Use generic-lens or generic-optics with 'debugLogging' instead." #-}

-- | The EC2 subnet IDs for the proxy.
--
-- /Note:/ Consider using 'vpcSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpVPCSubnetIds :: Lens.Lens' DBProxy (Lude.Maybe [Lude.Text])
dpVPCSubnetIds = Lens.lens (vpcSubnetIds :: DBProxy -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSubnetIds = a} :: DBProxy)
{-# DEPRECATED dpVPCSubnetIds "Use generic-lens or generic-optics with 'vpcSubnetIds' instead." #-}

-- | The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
--
-- /Note:/ Consider using 'engineFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpEngineFamily :: Lens.Lens' DBProxy (Lude.Maybe Lude.Text)
dpEngineFamily = Lens.lens (engineFamily :: DBProxy -> Lude.Maybe Lude.Text) (\s a -> s {engineFamily = a} :: DBProxy)
{-# DEPRECATED dpEngineFamily "Use generic-lens or generic-optics with 'engineFamily' instead." #-}

-- | One or more data structures specifying the authorization mechanism to connect to the associated RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpAuth :: Lens.Lens' DBProxy (Lude.Maybe [UserAuthConfigInfo])
dpAuth = Lens.lens (auth :: DBProxy -> Lude.Maybe [UserAuthConfigInfo]) (\s a -> s {auth = a} :: DBProxy)
{-# DEPRECATED dpAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | Indicates whether Transport Layer Security (TLS) encryption is required for connections to the proxy.
--
-- /Note:/ Consider using 'requireTLS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpRequireTLS :: Lens.Lens' DBProxy (Lude.Maybe Lude.Bool)
dpRequireTLS = Lens.lens (requireTLS :: DBProxy -> Lude.Maybe Lude.Bool) (\s a -> s {requireTLS = a} :: DBProxy)
{-# DEPRECATED dpRequireTLS "Use generic-lens or generic-optics with 'requireTLS' instead." #-}

-- | The number of seconds a connection to the proxy can have no activity before the proxy drops the client connection. The proxy keeps the underlying database connection open and puts it back into the connection pool for reuse by later connection requests.
--
-- Default: 1800 (30 minutes)
-- Constraints: 1 to 28,800
--
-- /Note:/ Consider using 'idleClientTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpIdleClientTimeout :: Lens.Lens' DBProxy (Lude.Maybe Lude.Int)
dpIdleClientTimeout = Lens.lens (idleClientTimeout :: DBProxy -> Lude.Maybe Lude.Int) (\s a -> s {idleClientTimeout = a} :: DBProxy)
{-# DEPRECATED dpIdleClientTimeout "Use generic-lens or generic-optics with 'idleClientTimeout' instead." #-}

-- | The date and time when the proxy was last updated.
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpUpdatedDate :: Lens.Lens' DBProxy (Lude.Maybe Lude.ISO8601)
dpUpdatedDate = Lens.lens (updatedDate :: DBProxy -> Lude.Maybe Lude.ISO8601) (\s a -> s {updatedDate = a} :: DBProxy)
{-# DEPRECATED dpUpdatedDate "Use generic-lens or generic-optics with 'updatedDate' instead." #-}

-- | The date and time when the proxy was first created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpCreatedDate :: Lens.Lens' DBProxy (Lude.Maybe Lude.ISO8601)
dpCreatedDate = Lens.lens (createdDate :: DBProxy -> Lude.Maybe Lude.ISO8601) (\s a -> s {createdDate = a} :: DBProxy)
{-# DEPRECATED dpCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | Provides a list of VPC security groups that the proxy belongs to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpVPCSecurityGroupIds :: Lens.Lens' DBProxy (Lude.Maybe [Lude.Text])
dpVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: DBProxy -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: DBProxy)
{-# DEPRECATED dpVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDBProxyName :: Lens.Lens' DBProxy (Lude.Maybe Lude.Text)
dpDBProxyName = Lens.lens (dbProxyName :: DBProxy -> Lude.Maybe Lude.Text) (\s a -> s {dbProxyName = a} :: DBProxy)
{-# DEPRECATED dpDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

-- | The endpoint that you can use to connect to the proxy. You include the endpoint value in the connection string for a database client application.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpEndpoint :: Lens.Lens' DBProxy (Lude.Maybe Lude.Text)
dpEndpoint = Lens.lens (endpoint :: DBProxy -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: DBProxy)
{-# DEPRECATED dpEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to access Amazon Secrets Manager.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpRoleARN :: Lens.Lens' DBProxy (Lude.Maybe Lude.Text)
dpRoleARN = Lens.lens (roleARN :: DBProxy -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DBProxy)
{-# DEPRECATED dpRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromXML DBProxy where
  parseXML x =
    DBProxy'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DBProxyArn")
      Lude.<*> (x Lude..@? "DebugLogging")
      Lude.<*> ( x Lude..@? "VpcSubnetIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "EngineFamily")
      Lude.<*> ( x Lude..@? "Auth" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "RequireTLS")
      Lude.<*> (x Lude..@? "IdleClientTimeout")
      Lude.<*> (x Lude..@? "UpdatedDate")
      Lude.<*> (x Lude..@? "CreatedDate")
      Lude.<*> ( x Lude..@? "VpcSecurityGroupIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DBProxyName")
      Lude.<*> (x Lude..@? "Endpoint")
      Lude.<*> (x Lude..@? "RoleArn")
