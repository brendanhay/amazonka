{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    mdbpDBProxyName,
    mdbpAuth,
    mdbpDebugLogging,
    mdbpIdleClientTimeout,
    mdbpNewDBProxyName,
    mdbpRequireTLS,
    mdbpRoleArn,
    mdbpSecurityGroups,

    -- * Destructuring the response
    ModifyDBProxyResponse (..),
    mkModifyDBProxyResponse,

    -- ** Response lenses
    mdbprrsDBProxy,
    mdbprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyDBProxy' smart constructor.
data ModifyDBProxy = ModifyDBProxy'
  { -- | The identifier for the @DBProxy@ to modify.
    dBProxyName :: Types.String,
    -- | The new authentication settings for the @DBProxy@ .
    auth :: Core.Maybe [Types.UserAuthConfig],
    -- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
    debugLogging :: Core.Maybe Core.Bool,
    -- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
    idleClientTimeout :: Core.Maybe Core.Int,
    -- | The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
    newDBProxyName :: Core.Maybe Types.String,
    -- | Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
    requireTLS :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
    roleArn :: Core.Maybe Types.String,
    -- | The new list of security groups for the @DBProxy@ .
    securityGroups :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBProxy' value with any optional fields omitted.
mkModifyDBProxy ::
  -- | 'dBProxyName'
  Types.String ->
  ModifyDBProxy
mkModifyDBProxy dBProxyName =
  ModifyDBProxy'
    { dBProxyName,
      auth = Core.Nothing,
      debugLogging = Core.Nothing,
      idleClientTimeout = Core.Nothing,
      newDBProxyName = Core.Nothing,
      requireTLS = Core.Nothing,
      roleArn = Core.Nothing,
      securityGroups = Core.Nothing
    }

-- | The identifier for the @DBProxy@ to modify.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpDBProxyName :: Lens.Lens' ModifyDBProxy Types.String
mdbpDBProxyName = Lens.field @"dBProxyName"
{-# DEPRECATED mdbpDBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead." #-}

-- | The new authentication settings for the @DBProxy@ .
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpAuth :: Lens.Lens' ModifyDBProxy (Core.Maybe [Types.UserAuthConfig])
mdbpAuth = Lens.field @"auth"
{-# DEPRECATED mdbpAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | Whether the proxy includes detailed information about SQL statements in its logs. This information helps you to debug issues involving SQL behavior or the performance and scalability of the proxy connections. The debug information includes the text of SQL statements that you submit through the proxy. Thus, only enable this setting when needed for debugging, and only when you have security measures in place to safeguard any sensitive information that appears in the logs.
--
-- /Note:/ Consider using 'debugLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpDebugLogging :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Bool)
mdbpDebugLogging = Lens.field @"debugLogging"
{-# DEPRECATED mdbpDebugLogging "Use generic-lens or generic-optics with 'debugLogging' instead." #-}

-- | The number of seconds that a connection to the proxy can be inactive before the proxy disconnects it. You can set this value higher or lower than the connection timeout limit for the associated database.
--
-- /Note:/ Consider using 'idleClientTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpIdleClientTimeout :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Int)
mdbpIdleClientTimeout = Lens.field @"idleClientTimeout"
{-# DEPRECATED mdbpIdleClientTimeout "Use generic-lens or generic-optics with 'idleClientTimeout' instead." #-}

-- | The new identifier for the @DBProxy@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'newDBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpNewDBProxyName :: Lens.Lens' ModifyDBProxy (Core.Maybe Types.String)
mdbpNewDBProxyName = Lens.field @"newDBProxyName"
{-# DEPRECATED mdbpNewDBProxyName "Use generic-lens or generic-optics with 'newDBProxyName' instead." #-}

-- | Whether Transport Layer Security (TLS) encryption is required for connections to the proxy. By enabling this setting, you can enforce encrypted TLS connections to the proxy, even if the associated database doesn't use TLS.
--
-- /Note:/ Consider using 'requireTLS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpRequireTLS :: Lens.Lens' ModifyDBProxy (Core.Maybe Core.Bool)
mdbpRequireTLS = Lens.field @"requireTLS"
{-# DEPRECATED mdbpRequireTLS "Use generic-lens or generic-optics with 'requireTLS' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that the proxy uses to access secrets in AWS Secrets Manager.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpRoleArn :: Lens.Lens' ModifyDBProxy (Core.Maybe Types.String)
mdbpRoleArn = Lens.field @"roleArn"
{-# DEPRECATED mdbpRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The new list of security groups for the @DBProxy@ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpSecurityGroups :: Lens.Lens' ModifyDBProxy (Core.Maybe [Types.String])
mdbpSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED mdbpSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

instance Core.AWSRequest ModifyDBProxy where
  type Rs ModifyDBProxy = ModifyDBProxyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyDBProxy")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBProxyName" dBProxyName)
                Core.<> ( Core.toQueryValue
                            "Auth"
                            (Core.toQueryList "member" Core.<$> auth)
                        )
                Core.<> (Core.toQueryValue "DebugLogging" Core.<$> debugLogging)
                Core.<> (Core.toQueryValue "IdleClientTimeout" Core.<$> idleClientTimeout)
                Core.<> (Core.toQueryValue "NewDBProxyName" Core.<$> newDBProxyName)
                Core.<> (Core.toQueryValue "RequireTLS" Core.<$> requireTLS)
                Core.<> (Core.toQueryValue "RoleArn" Core.<$> roleArn)
                Core.<> ( Core.toQueryValue
                            "SecurityGroups"
                            (Core.toQueryList "member" Core.<$> securityGroups)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBProxyResult"
      ( \s h x ->
          ModifyDBProxyResponse'
            Core.<$> (x Core..@? "DBProxy") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDBProxyResponse' smart constructor.
data ModifyDBProxyResponse = ModifyDBProxyResponse'
  { -- | The @DBProxy@ object representing the new settings for the proxy.
    dBProxy :: Core.Maybe Types.DBProxy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyDBProxyResponse' value with any optional fields omitted.
mkModifyDBProxyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDBProxyResponse
mkModifyDBProxyResponse responseStatus =
  ModifyDBProxyResponse' {dBProxy = Core.Nothing, responseStatus}

-- | The @DBProxy@ object representing the new settings for the proxy.
--
-- /Note:/ Consider using 'dBProxy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbprrsDBProxy :: Lens.Lens' ModifyDBProxyResponse (Core.Maybe Types.DBProxy)
mdbprrsDBProxy = Lens.field @"dBProxy"
{-# DEPRECATED mdbprrsDBProxy "Use generic-lens or generic-optics with 'dBProxy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbprrsResponseStatus :: Lens.Lens' ModifyDBProxyResponse Core.Int
mdbprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdbprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
