{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyDBProxy (..)
    , mkModifyDBProxy
    -- ** Request lenses
    , mdbpDBProxyName
    , mdbpAuth
    , mdbpDebugLogging
    , mdbpIdleClientTimeout
    , mdbpNewDBProxyName
    , mdbpRequireTLS
    , mdbpRoleArn
    , mdbpSecurityGroups

    -- * Destructuring the response
    , ModifyDBProxyResponse (..)
    , mkModifyDBProxyResponse
    -- ** Response lenses
    , mdbprrsDBProxy
    , mdbprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyDBProxy' smart constructor.
data ModifyDBProxy = ModifyDBProxy'
  { dBProxyName :: Core.Text
    -- ^ The identifier for the @DBProxy@ to modify.
  , auth :: Core.Maybe [Types.UserAuthConfig]
    -- ^ The new authentication settings for the @DBProxy@ .
  , debugLogging :: Core.Maybe Core.Bool
    -- ^ Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
  , idleClientTimeout :: Core.Maybe Core.Int
    -- ^ The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
  , newDBProxyName :: Core.Maybe Core.Text
    -- ^ The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
  , requireTLS :: Core.Maybe Core.Bool
    -- ^ Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
  , roleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
  , securityGroups :: Core.Maybe [Core.Text]
    -- ^ The new list of security groups for the @DBProxy@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBProxy' value with any optional fields omitted.
mkModifyDBProxy
    :: Core.Text -- ^ 'dBProxyName'
    -> ModifyDBProxy
mkModifyDBProxy dBProxyName
  = ModifyDBProxy'{dBProxyName, auth = Core.Nothing,
                   debugLogging = Core.Nothing, idleClientTimeout = Core.Nothing,
                   newDBProxyName = Core.Nothing, requireTLS = Core.Nothing,
                   roleArn = Core.Nothing, securityGroups = Core.Nothing}

-- | The identifier for the @DBProxy@ to modify.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpDBProxyName :: Lens.Lens' ModifyDBProxy Core.Text
mdbpDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE mdbpDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | The new authentication settings for the @DBProxy@ .
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpAuth :: Lens.Lens' ModifyDBProxy (Core.Maybe [Types.UserAuthConfig])
mdbpAuth = Lens.field @"auth"
{-# INLINEABLE mdbpAuth #-}
{-# DEPRECATED auth "Use generic-lens or generic-optics with 'auth' instead"  #-}

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- /Note:/ Consider using 'debugLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpDebugLogging :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Bool)
mdbpDebugLogging = Lens.field @"debugLogging"
{-# INLINEABLE mdbpDebugLogging #-}
{-# DEPRECATED debugLogging "Use generic-lens or generic-optics with 'debugLogging' instead"  #-}

-- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
--
-- /Note:/ Consider using 'idleClientTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpIdleClientTimeout :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Int)
mdbpIdleClientTimeout = Lens.field @"idleClientTimeout"
{-# INLINEABLE mdbpIdleClientTimeout #-}
{-# DEPRECATED idleClientTimeout "Use generic-lens or generic-optics with 'idleClientTimeout' instead"  #-}

-- | The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'newDBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpNewDBProxyName :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Text)
mdbpNewDBProxyName = Lens.field @"newDBProxyName"
{-# INLINEABLE mdbpNewDBProxyName #-}
{-# DEPRECATED newDBProxyName "Use generic-lens or generic-optics with 'newDBProxyName' instead"  #-}

-- | Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
--
-- /Note:/ Consider using 'requireTLS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpRequireTLS :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Bool)
mdbpRequireTLS = Lens.field @"requireTLS"
{-# INLINEABLE mdbpRequireTLS #-}
{-# DEPRECATED requireTLS "Use generic-lens or generic-optics with 'requireTLS' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpRoleArn :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Text)
mdbpRoleArn = Lens.field @"roleArn"
{-# INLINEABLE mdbpRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The new list of security groups for the @DBProxy@ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpSecurityGroups :: Lens.Lens' ModifyDBProxy (Core.Maybe [Core.Text])
mdbpSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE mdbpSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

instance Core.ToQuery ModifyDBProxy where
        toQuery ModifyDBProxy{..}
          = Core.toQueryPair "Action" ("ModifyDBProxy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBProxyName" dBProxyName
              Core.<>
              Core.toQueryPair "Auth"
                (Core.maybe Core.mempty (Core.toQueryList "member") auth)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DebugLogging")
                debugLogging
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IdleClientTimeout")
                idleClientTimeout
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NewDBProxyName")
                newDBProxyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RequireTLS") requireTLS
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "RoleArn") roleArn
              Core.<>
              Core.toQueryPair "SecurityGroups"
                (Core.maybe Core.mempty (Core.toQueryList "member") securityGroups)

instance Core.ToHeaders ModifyDBProxy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyDBProxy where
        type Rs ModifyDBProxy = ModifyDBProxyResponse
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
          = Response.receiveXMLWrapper "ModifyDBProxyResult"
              (\ s h x ->
                 ModifyDBProxyResponse' Core.<$>
                   (x Core..@? "DBProxy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyDBProxyResponse' smart constructor.
data ModifyDBProxyResponse = ModifyDBProxyResponse'
  { dBProxy :: Core.Maybe Types.DBProxy
    -- ^ The @DBProxy@ object representing the new settings for the proxy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyDBProxyResponse' value with any optional fields omitted.
mkModifyDBProxyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyDBProxyResponse
mkModifyDBProxyResponse responseStatus
  = ModifyDBProxyResponse'{dBProxy = Core.Nothing, responseStatus}

-- | The @DBProxy@ object representing the new settings for the proxy.
--
-- /Note:/ Consider using 'dBProxy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbprrsDBProxy :: Lens.Lens' ModifyDBProxyResponse (Core.Maybe Types.DBProxy)
mdbprrsDBProxy = Lens.field @"dBProxy"
{-# INLINEABLE mdbprrsDBProxy #-}
{-# DEPRECATED dBProxy "Use generic-lens or generic-optics with 'dBProxy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbprrsResponseStatus :: Lens.Lens' ModifyDBProxyResponse Core.Int
mdbprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mdbprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
