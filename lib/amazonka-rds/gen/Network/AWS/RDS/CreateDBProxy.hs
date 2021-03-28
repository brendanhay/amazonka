{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDBProxy (..)
    , mkCreateDBProxy
    -- ** Request lenses
    , cdbpDBProxyName
    , cdbpEngineFamily
    , cdbpAuth
    , cdbpRoleArn
    , cdbpVpcSubnetIds
    , cdbpDebugLogging
    , cdbpIdleClientTimeout
    , cdbpRequireTLS
    , cdbpTags
    , cdbpVpcSecurityGroupIds

    -- * Destructuring the response
    , CreateDBProxyResponse (..)
    , mkCreateDBProxyResponse
    -- ** Response lenses
    , cdbprrsDBProxy
    , cdbprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDBProxy' smart constructor.
data CreateDBProxy = CreateDBProxy'
  { dBProxyName :: Core.Text
    -- ^ The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
  , engineFamily :: Types.EngineFamily
    -- ^ The kinds of databases that the proxy can connect to. This value determines which database network protocol the proxy recognizes when it interprets network traffic to and from the database. The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
  , auth :: [Types.UserAuthConfig]
    -- ^ The authorization mechanism that the proxy uses.
  , roleArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
  , vpcSubnetIds :: [Core.Text]
    -- ^ One or more VPC subnet IDs to associate with the new proxy.
  , debugLogging :: Core.Maybe Core.Bool
    -- ^ Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
  , idleClientTimeout :: Core.Maybe Core.Int
    -- ^ The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
  , requireTLS :: Core.Maybe Core.Bool
    -- ^ A Boolean parameter that specifies whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An optional set of key-value pairs to associate arbitrary data of your choosing with the proxy.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ One or more VPC security group IDs to associate with the new proxy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBProxy' value with any optional fields omitted.
mkCreateDBProxy
    :: Core.Text -- ^ 'dBProxyName'
    -> Types.EngineFamily -- ^ 'engineFamily'
    -> Core.Text -- ^ 'roleArn'
    -> CreateDBProxy
mkCreateDBProxy dBProxyName engineFamily roleArn
  = CreateDBProxy'{dBProxyName, engineFamily, auth = Core.mempty,
                   roleArn, vpcSubnetIds = Core.mempty, debugLogging = Core.Nothing,
                   idleClientTimeout = Core.Nothing, requireTLS = Core.Nothing,
                   tags = Core.Nothing, vpcSecurityGroupIds = Core.Nothing}

-- | The identifier for the proxy. This name must be unique for all proxies owned by your AWS account in the specified AWS Region. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpDBProxyName :: Lens.Lens' CreateDBProxy Core.Text
cdbpDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE cdbpDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | The kinds of databases that the proxy can connect to. This value determines which database network protocol the proxy recognizes when it interprets network traffic to and from the database. The engine family applies to MySQL and PostgreSQL for both RDS and Aurora.
--
-- /Note:/ Consider using 'engineFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpEngineFamily :: Lens.Lens' CreateDBProxy Types.EngineFamily
cdbpEngineFamily = Lens.field @"engineFamily"
{-# INLINEABLE cdbpEngineFamily #-}
{-# DEPRECATED engineFamily "Use generic-lens or generic-optics with 'engineFamily' instead"  #-}

-- | The authorization mechanism that the proxy uses.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpAuth :: Lens.Lens' CreateDBProxy [Types.UserAuthConfig]
cdbpAuth = Lens.field @"auth"
{-# INLINEABLE cdbpAuth #-}
{-# DEPRECATED auth "Use generic-lens or generic-optics with 'auth' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpRoleArn :: Lens.Lens' CreateDBProxy Core.Text
cdbpRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cdbpRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | One or more VPC subnet IDs to associate with the new proxy.
--
-- /Note:/ Consider using 'vpcSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpVpcSubnetIds :: Lens.Lens' CreateDBProxy [Core.Text]
cdbpVpcSubnetIds = Lens.field @"vpcSubnetIds"
{-# INLINEABLE cdbpVpcSubnetIds #-}
{-# DEPRECATED vpcSubnetIds "Use generic-lens or generic-optics with 'vpcSubnetIds' instead"  #-}

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- /Note:/ Consider using 'debugLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpDebugLogging :: Lens.Lens' CreateDBProxy (Core.Maybe Core.Bool)
cdbpDebugLogging = Lens.field @"debugLogging"
{-# INLINEABLE cdbpDebugLogging #-}
{-# DEPRECATED debugLogging "Use generic-lens or generic-optics with 'debugLogging' instead"  #-}

-- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
--
-- /Note:/ Consider using 'idleClientTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpIdleClientTimeout :: Lens.Lens' CreateDBProxy (Core.Maybe Core.Int)
cdbpIdleClientTimeout = Lens.field @"idleClientTimeout"
{-# INLINEABLE cdbpIdleClientTimeout #-}
{-# DEPRECATED idleClientTimeout "Use generic-lens or generic-optics with 'idleClientTimeout' instead"  #-}

-- | A Boolean parameter that specifies whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy.
--
-- /Note:/ Consider using 'requireTLS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpRequireTLS :: Lens.Lens' CreateDBProxy (Core.Maybe Core.Bool)
cdbpRequireTLS = Lens.field @"requireTLS"
{-# INLINEABLE cdbpRequireTLS #-}
{-# DEPRECATED requireTLS "Use generic-lens or generic-optics with 'requireTLS' instead"  #-}

-- | An optional set of key-value pairs to associate arbitrary data of your choosing with the proxy.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpTags :: Lens.Lens' CreateDBProxy (Core.Maybe [Types.Tag])
cdbpTags = Lens.field @"tags"
{-# INLINEABLE cdbpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | One or more VPC security group IDs to associate with the new proxy.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbpVpcSecurityGroupIds :: Lens.Lens' CreateDBProxy (Core.Maybe [Core.Text])
cdbpVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE cdbpVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery CreateDBProxy where
        toQuery CreateDBProxy{..}
          = Core.toQueryPair "Action" ("CreateDBProxy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBProxyName" dBProxyName
              Core.<> Core.toQueryPair "EngineFamily" engineFamily
              Core.<> Core.toQueryPair "Auth" (Core.toQueryList "member" auth)
              Core.<> Core.toQueryPair "RoleArn" roleArn
              Core.<>
              Core.toQueryPair "VpcSubnetIds"
                (Core.toQueryList "member" vpcSubnetIds)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DebugLogging")
                debugLogging
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IdleClientTimeout")
                idleClientTimeout
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RequireTLS") requireTLS
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   vpcSecurityGroupIds)

instance Core.ToHeaders CreateDBProxy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBProxy where
        type Rs CreateDBProxy = CreateDBProxyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateDBProxyResult"
              (\ s h x ->
                 CreateDBProxyResponse' Core.<$>
                   (x Core..@? "DBProxy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDBProxyResponse' smart constructor.
data CreateDBProxyResponse = CreateDBProxyResponse'
  { dBProxy :: Core.Maybe Types.DBProxy
    -- ^ The @DBProxy@ structure corresponding to the new proxy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDBProxyResponse' value with any optional fields omitted.
mkCreateDBProxyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDBProxyResponse
mkCreateDBProxyResponse responseStatus
  = CreateDBProxyResponse'{dBProxy = Core.Nothing, responseStatus}

-- | The @DBProxy@ structure corresponding to the new proxy.
--
-- /Note:/ Consider using 'dBProxy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbprrsDBProxy :: Lens.Lens' CreateDBProxyResponse (Core.Maybe Types.DBProxy)
cdbprrsDBProxy = Lens.field @"dBProxy"
{-# INLINEABLE cdbprrsDBProxy #-}
{-# DEPRECATED dBProxy "Use generic-lens or generic-optics with 'dBProxy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbprrsResponseStatus :: Lens.Lens' CreateDBProxyResponse Core.Int
cdbprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
