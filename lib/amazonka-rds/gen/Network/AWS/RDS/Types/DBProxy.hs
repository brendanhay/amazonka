{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dbpAuth,
    dbpCreatedDate,
    dbpDBProxyArn,
    dbpDBProxyName,
    dbpDebugLogging,
    dbpEndpoint,
    dbpEngineFamily,
    dbpIdleClientTimeout,
    dbpRequireTLS,
    dbpRoleArn,
    dbpStatus,
    dbpUpdatedDate,
    dbpVpcSecurityGroupIds,
    dbpVpcSubnetIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBProxyArn as Types
import qualified Network.AWS.RDS.Types.DBProxyName as Types
import qualified Network.AWS.RDS.Types.DBProxyStatus as Types
import qualified Network.AWS.RDS.Types.RoleArn as Types
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.UserAuthConfigInfo as Types

-- | The data structure representing a proxy managed by the RDS Proxy.
--
-- This data type is used as a response element in the @DescribeDBProxies@ action.
--
-- /See:/ 'mkDBProxy' smart constructor.
data DBProxy = DBProxy'
  { -- | One or more data structures specifying the authorization mechanism to connect to the associated RDS DB instance or Aurora DB cluster.
    auth :: Core.Maybe [Types.UserAuthConfigInfo],
    -- | The date and time when the proxy was first created.
    createdDate :: Core.Maybe Core.UTCTime,
    -- | The Amazon Resource Name (ARN) for the proxy.
    dBProxyArn :: Core.Maybe Types.DBProxyArn,
    -- | The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region.
    dBProxyName :: Core.Maybe Types.DBProxyName,
    -- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
    debugLogging :: Core.Maybe Core.Bool,
    -- | The endpoint that you can use to connect to the proxy. You include the endpoint value in the connection string for a database client application.
    endpoint :: Core.Maybe Types.String,
    -- | The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
    engineFamily :: Core.Maybe Types.String,
    -- | The number of seconds a connection to the proxy can have no activity before the proxy drops the client connection. The proxy keeps the underlying database connection open and puts it back into the connection pool for reuse by later connection requests.
    --
    -- Default: 1800 (30 minutes)
    -- Constraints: 1 to 28,800
    idleClientTimeout :: Core.Maybe Core.Int,
    -- | Indicates whether Transport Layer Security (TLS) encryption is required for connections to the proxy.
    requireTLS :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to access Amazon Secrets Manager.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The current status of this proxy. A status of @available@ means the proxy is ready to handle requests. Other values indicate that you must wait for the proxy to be ready, or take some action to resolve an issue.
    status :: Core.Maybe Types.DBProxyStatus,
    -- | The date and time when the proxy was last updated.
    updatedDate :: Core.Maybe Core.UTCTime,
    -- | Provides a list of VPC security groups that the proxy belongs to.
    vpcSecurityGroupIds :: Core.Maybe [Types.String],
    -- | The EC2 subnet IDs for the proxy.
    vpcSubnetIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DBProxy' value with any optional fields omitted.
mkDBProxy ::
  DBProxy
mkDBProxy =
  DBProxy'
    { auth = Core.Nothing,
      createdDate = Core.Nothing,
      dBProxyArn = Core.Nothing,
      dBProxyName = Core.Nothing,
      debugLogging = Core.Nothing,
      endpoint = Core.Nothing,
      engineFamily = Core.Nothing,
      idleClientTimeout = Core.Nothing,
      requireTLS = Core.Nothing,
      roleArn = Core.Nothing,
      status = Core.Nothing,
      updatedDate = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing,
      vpcSubnetIds = Core.Nothing
    }

-- | One or more data structures specifying the authorization mechanism to connect to the associated RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpAuth :: Lens.Lens' DBProxy (Core.Maybe [Types.UserAuthConfigInfo])
dbpAuth = Lens.field @"auth"
{-# DEPRECATED dbpAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | The date and time when the proxy was first created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpCreatedDate :: Lens.Lens' DBProxy (Core.Maybe Core.UTCTime)
dbpCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED dbpCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The Amazon Resource Name (ARN) for the proxy.
--
-- /Note:/ Consider using 'dBProxyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpDBProxyArn :: Lens.Lens' DBProxy (Core.Maybe Types.DBProxyArn)
dbpDBProxyArn = Lens.field @"dBProxyArn"
{-# DEPRECATED dbpDBProxyArn "Use generic-lens or generic-optics with 'dBProxyArn' instead." #-}

-- | The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpDBProxyName :: Lens.Lens' DBProxy (Core.Maybe Types.DBProxyName)
dbpDBProxyName = Lens.field @"dBProxyName"
{-# DEPRECATED dbpDBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead." #-}

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- /Note:/ Consider using 'debugLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpDebugLogging :: Lens.Lens' DBProxy (Core.Maybe Core.Bool)
dbpDebugLogging = Lens.field @"debugLogging"
{-# DEPRECATED dbpDebugLogging "Use generic-lens or generic-optics with 'debugLogging' instead." #-}

-- | The endpoint that you can use to connect to the proxy. You include the endpoint value in the connection string for a database client application.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpEndpoint :: Lens.Lens' DBProxy (Core.Maybe Types.String)
dbpEndpoint = Lens.field @"endpoint"
{-# DEPRECATED dbpEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
--
-- /Note:/ Consider using 'engineFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpEngineFamily :: Lens.Lens' DBProxy (Core.Maybe Types.String)
dbpEngineFamily = Lens.field @"engineFamily"
{-# DEPRECATED dbpEngineFamily "Use generic-lens or generic-optics with 'engineFamily' instead." #-}

-- | The number of seconds a connection to the proxy can have no activity before the proxy drops the client connection. The proxy keeps the underlying database connection open and puts it back into the connection pool for reuse by later connection requests.
--
-- Default: 1800 (30 minutes)
-- Constraints: 1 to 28,800
--
-- /Note:/ Consider using 'idleClientTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpIdleClientTimeout :: Lens.Lens' DBProxy (Core.Maybe Core.Int)
dbpIdleClientTimeout = Lens.field @"idleClientTimeout"
{-# DEPRECATED dbpIdleClientTimeout "Use generic-lens or generic-optics with 'idleClientTimeout' instead." #-}

-- | Indicates whether Transport Layer Security (TLS) encryption is required for connections to the proxy.
--
-- /Note:/ Consider using 'requireTLS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpRequireTLS :: Lens.Lens' DBProxy (Core.Maybe Core.Bool)
dbpRequireTLS = Lens.field @"requireTLS"
{-# DEPRECATED dbpRequireTLS "Use generic-lens or generic-optics with 'requireTLS' instead." #-}

-- | The Amazon Resource Name (ARN) for the IAM role that the proxy uses to access Amazon Secrets Manager.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpRoleArn :: Lens.Lens' DBProxy (Core.Maybe Types.RoleArn)
dbpRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dbpRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The current status of this proxy. A status of @available@ means the proxy is ready to handle requests. Other values indicate that you must wait for the proxy to be ready, or take some action to resolve an issue.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpStatus :: Lens.Lens' DBProxy (Core.Maybe Types.DBProxyStatus)
dbpStatus = Lens.field @"status"
{-# DEPRECATED dbpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time when the proxy was last updated.
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpUpdatedDate :: Lens.Lens' DBProxy (Core.Maybe Core.UTCTime)
dbpUpdatedDate = Lens.field @"updatedDate"
{-# DEPRECATED dbpUpdatedDate "Use generic-lens or generic-optics with 'updatedDate' instead." #-}

-- | Provides a list of VPC security groups that the proxy belongs to.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpVpcSecurityGroupIds :: Lens.Lens' DBProxy (Core.Maybe [Types.String])
dbpVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED dbpVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The EC2 subnet IDs for the proxy.
--
-- /Note:/ Consider using 'vpcSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpVpcSubnetIds :: Lens.Lens' DBProxy (Core.Maybe [Types.String])
dbpVpcSubnetIds = Lens.field @"vpcSubnetIds"
{-# DEPRECATED dbpVpcSubnetIds "Use generic-lens or generic-optics with 'vpcSubnetIds' instead." #-}

instance Core.FromXML DBProxy where
  parseXML x =
    DBProxy'
      Core.<$> (x Core..@? "Auth" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "CreatedDate")
      Core.<*> (x Core..@? "DBProxyArn")
      Core.<*> (x Core..@? "DBProxyName")
      Core.<*> (x Core..@? "DebugLogging")
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "EngineFamily")
      Core.<*> (x Core..@? "IdleClientTimeout")
      Core.<*> (x Core..@? "RequireTLS")
      Core.<*> (x Core..@? "RoleArn")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "UpdatedDate")
      Core.<*> ( x Core..@? "VpcSecurityGroupIds"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "VpcSubnetIds" Core..<@> Core.parseXMLList "member")
