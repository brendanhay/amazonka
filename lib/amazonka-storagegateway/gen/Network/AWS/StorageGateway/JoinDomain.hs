{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.JoinDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a file gateway to an Active Directory domain. This operation is only supported for file gateways that support the SMB file protocol.
module Network.AWS.StorageGateway.JoinDomain
  ( -- * Creating a request
    JoinDomain (..),
    mkJoinDomain,

    -- ** Request lenses
    jdGatewayARN,
    jdDomainName,
    jdUserName,
    jdPassword,
    jdDomainControllers,
    jdOrganizationalUnit,
    jdTimeoutInSeconds,

    -- * Destructuring the response
    JoinDomainResponse (..),
    mkJoinDomainResponse,

    -- ** Response lenses
    jdrrsActiveDirectoryStatus,
    jdrrsGatewayARN,
    jdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | JoinDomainInput
--
-- /See:/ 'mkJoinDomain' smart constructor.
data JoinDomain = JoinDomain'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@ operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Types.GatewayARN,
    -- | The name of the domain that you want the gateway to join.
    domainName :: Types.DomainName,
    -- | Sets the user name of user who has permission to add the gateway to the Active Directory domain. The domain user account should be enabled to join computers to the domain. For example, you can use the domain administrator account or an account with delegated permissions to join computers to the domain.
    userName :: Types.DomainUserName,
    -- | Sets the password of the user who has permission to add the gateway to the Active Directory domain.
    password :: Types.DomainUserPassword,
    -- | List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (“:”). For example, @mydc.mydomain.com:389@ .
    domainControllers :: Core.Maybe [Types.Host],
    -- | The organizational unit (OU) is a container in an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
    organizationalUnit :: Core.Maybe Types.OrganizationalUnit,
    -- | Specifies the time in seconds, in which the @JoinDomain@ operation must complete. The default is 20 seconds.
    timeoutInSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JoinDomain' value with any optional fields omitted.
mkJoinDomain ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'domainName'
  Types.DomainName ->
  -- | 'userName'
  Types.DomainUserName ->
  -- | 'password'
  Types.DomainUserPassword ->
  JoinDomain
mkJoinDomain gatewayARN domainName userName password =
  JoinDomain'
    { gatewayARN,
      domainName,
      userName,
      password,
      domainControllers = Core.Nothing,
      organizationalUnit = Core.Nothing,
      timeoutInSeconds = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@ operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdGatewayARN :: Lens.Lens' JoinDomain Types.GatewayARN
jdGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED jdGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The name of the domain that you want the gateway to join.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDomainName :: Lens.Lens' JoinDomain Types.DomainName
jdDomainName = Lens.field @"domainName"
{-# DEPRECATED jdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Sets the user name of user who has permission to add the gateway to the Active Directory domain. The domain user account should be enabled to join computers to the domain. For example, you can use the domain administrator account or an account with delegated permissions to join computers to the domain.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdUserName :: Lens.Lens' JoinDomain Types.DomainUserName
jdUserName = Lens.field @"userName"
{-# DEPRECATED jdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Sets the password of the user who has permission to add the gateway to the Active Directory domain.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdPassword :: Lens.Lens' JoinDomain Types.DomainUserPassword
jdPassword = Lens.field @"password"
{-# DEPRECATED jdPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (“:”). For example, @mydc.mydomain.com:389@ .
--
-- /Note:/ Consider using 'domainControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDomainControllers :: Lens.Lens' JoinDomain (Core.Maybe [Types.Host])
jdDomainControllers = Lens.field @"domainControllers"
{-# DEPRECATED jdDomainControllers "Use generic-lens or generic-optics with 'domainControllers' instead." #-}

-- | The organizational unit (OU) is a container in an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdOrganizationalUnit :: Lens.Lens' JoinDomain (Core.Maybe Types.OrganizationalUnit)
jdOrganizationalUnit = Lens.field @"organizationalUnit"
{-# DEPRECATED jdOrganizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead." #-}

-- | Specifies the time in seconds, in which the @JoinDomain@ operation must complete. The default is 20 seconds.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTimeoutInSeconds :: Lens.Lens' JoinDomain (Core.Maybe Core.Natural)
jdTimeoutInSeconds = Lens.field @"timeoutInSeconds"
{-# DEPRECATED jdTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

instance Core.FromJSON JoinDomain where
  toJSON JoinDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("DomainName" Core..= domainName),
            Core.Just ("UserName" Core..= userName),
            Core.Just ("Password" Core..= password),
            ("DomainControllers" Core..=) Core.<$> domainControllers,
            ("OrganizationalUnit" Core..=) Core.<$> organizationalUnit,
            ("TimeoutInSeconds" Core..=) Core.<$> timeoutInSeconds
          ]
      )

instance Core.AWSRequest JoinDomain where
  type Rs JoinDomain = JoinDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.JoinDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          JoinDomainResponse'
            Core.<$> (x Core..:? "ActiveDirectoryStatus")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | JoinDomainOutput
--
-- /See:/ 'mkJoinDomainResponse' smart constructor.
data JoinDomainResponse = JoinDomainResponse'
  { -- | Indicates the status of the gateway as a member of the Active Directory domain.
    --
    --
    --     * @ACCESS_DENIED@ : Indicates that the @JoinDomain@ operation failed due to an authentication error.
    --
    --
    --     * @DETACHED@ : Indicates that gateway is not joined to a domain.
    --
    --
    --     * @JOINED@ : Indicates that the gateway has successfully joined a domain.
    --
    --
    --     * @JOINING@ : Indicates that a @JoinDomain@ operation is in progress.
    --
    --
    --     * @NETWORK_ERROR@ : Indicates that @JoinDomain@ operation failed due to a network or connectivity error.
    --
    --
    --     * @TIMEOUT@ : Indicates that the @JoinDomain@ operation failed because the operation didn't complete within the allotted time.
    --
    --
    --     * @UNKNOWN_ERROR@ : Indicates that the @JoinDomain@ operation failed due to another type of error.
    activeDirectoryStatus :: Core.Maybe Types.ActiveDirectoryStatus,
    -- | The unique Amazon Resource Name (ARN) of the gateway that joined the domain.
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JoinDomainResponse' value with any optional fields omitted.
mkJoinDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  JoinDomainResponse
mkJoinDomainResponse responseStatus =
  JoinDomainResponse'
    { activeDirectoryStatus = Core.Nothing,
      gatewayARN = Core.Nothing,
      responseStatus
    }

-- | Indicates the status of the gateway as a member of the Active Directory domain.
--
--
--     * @ACCESS_DENIED@ : Indicates that the @JoinDomain@ operation failed due to an authentication error.
--
--
--     * @DETACHED@ : Indicates that gateway is not joined to a domain.
--
--
--     * @JOINED@ : Indicates that the gateway has successfully joined a domain.
--
--
--     * @JOINING@ : Indicates that a @JoinDomain@ operation is in progress.
--
--
--     * @NETWORK_ERROR@ : Indicates that @JoinDomain@ operation failed due to a network or connectivity error.
--
--
--     * @TIMEOUT@ : Indicates that the @JoinDomain@ operation failed because the operation didn't complete within the allotted time.
--
--
--     * @UNKNOWN_ERROR@ : Indicates that the @JoinDomain@ operation failed due to another type of error.
--
--
--
-- /Note:/ Consider using 'activeDirectoryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrrsActiveDirectoryStatus :: Lens.Lens' JoinDomainResponse (Core.Maybe Types.ActiveDirectoryStatus)
jdrrsActiveDirectoryStatus = Lens.field @"activeDirectoryStatus"
{-# DEPRECATED jdrrsActiveDirectoryStatus "Use generic-lens or generic-optics with 'activeDirectoryStatus' instead." #-}

-- | The unique Amazon Resource Name (ARN) of the gateway that joined the domain.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrrsGatewayARN :: Lens.Lens' JoinDomainResponse (Core.Maybe Types.GatewayARN)
jdrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED jdrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrrsResponseStatus :: Lens.Lens' JoinDomainResponse Core.Int
jdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED jdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
