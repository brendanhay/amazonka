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
    jdOrganizationalUnit,
    jdUserName,
    jdTimeoutInSeconds,
    jdDomainName,
    jdPassword,
    jdDomainControllers,

    -- * Destructuring the response
    JoinDomainResponse (..),
    mkJoinDomainResponse,

    -- ** Response lenses
    jdrsGatewayARN,
    jdrsActiveDirectoryStatus,
    jdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | JoinDomainInput
--
-- /See:/ 'mkJoinDomain' smart constructor.
data JoinDomain = JoinDomain'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@ operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Lude.Text,
    -- | The organizational unit (OU) is a container in an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
    organizationalUnit :: Lude.Maybe Lude.Text,
    -- | Sets the user name of user who has permission to add the gateway to the Active Directory domain. The domain user account should be enabled to join computers to the domain. For example, you can use the domain administrator account or an account with delegated permissions to join computers to the domain.
    userName :: Lude.Text,
    -- | Specifies the time in seconds, in which the @JoinDomain@ operation must complete. The default is 20 seconds.
    timeoutInSeconds :: Lude.Maybe Lude.Natural,
    -- | The name of the domain that you want the gateway to join.
    domainName :: Lude.Text,
    -- | Sets the password of the user who has permission to add the gateway to the Active Directory domain.
    password :: Lude.Sensitive Lude.Text,
    -- | List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (“:”). For example, @mydc.mydomain.com:389@ .
    domainControllers :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JoinDomain' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@ operation to return a list of gateways for your account and AWS Region.
-- * 'organizationalUnit' - The organizational unit (OU) is a container in an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
-- * 'userName' - Sets the user name of user who has permission to add the gateway to the Active Directory domain. The domain user account should be enabled to join computers to the domain. For example, you can use the domain administrator account or an account with delegated permissions to join computers to the domain.
-- * 'timeoutInSeconds' - Specifies the time in seconds, in which the @JoinDomain@ operation must complete. The default is 20 seconds.
-- * 'domainName' - The name of the domain that you want the gateway to join.
-- * 'password' - Sets the password of the user who has permission to add the gateway to the Active Directory domain.
-- * 'domainControllers' - List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (“:”). For example, @mydc.mydomain.com:389@ .
mkJoinDomain ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  JoinDomain
mkJoinDomain pGatewayARN_ pUserName_ pDomainName_ pPassword_ =
  JoinDomain'
    { gatewayARN = pGatewayARN_,
      organizationalUnit = Lude.Nothing,
      userName = pUserName_,
      timeoutInSeconds = Lude.Nothing,
      domainName = pDomainName_,
      password = pPassword_,
      domainControllers = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@ operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdGatewayARN :: Lens.Lens' JoinDomain Lude.Text
jdGatewayARN = Lens.lens (gatewayARN :: JoinDomain -> Lude.Text) (\s a -> s {gatewayARN = a} :: JoinDomain)
{-# DEPRECATED jdGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The organizational unit (OU) is a container in an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdOrganizationalUnit :: Lens.Lens' JoinDomain (Lude.Maybe Lude.Text)
jdOrganizationalUnit = Lens.lens (organizationalUnit :: JoinDomain -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnit = a} :: JoinDomain)
{-# DEPRECATED jdOrganizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead." #-}

-- | Sets the user name of user who has permission to add the gateway to the Active Directory domain. The domain user account should be enabled to join computers to the domain. For example, you can use the domain administrator account or an account with delegated permissions to join computers to the domain.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdUserName :: Lens.Lens' JoinDomain Lude.Text
jdUserName = Lens.lens (userName :: JoinDomain -> Lude.Text) (\s a -> s {userName = a} :: JoinDomain)
{-# DEPRECATED jdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Specifies the time in seconds, in which the @JoinDomain@ operation must complete. The default is 20 seconds.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTimeoutInSeconds :: Lens.Lens' JoinDomain (Lude.Maybe Lude.Natural)
jdTimeoutInSeconds = Lens.lens (timeoutInSeconds :: JoinDomain -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInSeconds = a} :: JoinDomain)
{-# DEPRECATED jdTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | The name of the domain that you want the gateway to join.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDomainName :: Lens.Lens' JoinDomain Lude.Text
jdDomainName = Lens.lens (domainName :: JoinDomain -> Lude.Text) (\s a -> s {domainName = a} :: JoinDomain)
{-# DEPRECATED jdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Sets the password of the user who has permission to add the gateway to the Active Directory domain.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdPassword :: Lens.Lens' JoinDomain (Lude.Sensitive Lude.Text)
jdPassword = Lens.lens (password :: JoinDomain -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: JoinDomain)
{-# DEPRECATED jdPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (“:”). For example, @mydc.mydomain.com:389@ .
--
-- /Note:/ Consider using 'domainControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDomainControllers :: Lens.Lens' JoinDomain (Lude.Maybe [Lude.Text])
jdDomainControllers = Lens.lens (domainControllers :: JoinDomain -> Lude.Maybe [Lude.Text]) (\s a -> s {domainControllers = a} :: JoinDomain)
{-# DEPRECATED jdDomainControllers "Use generic-lens or generic-optics with 'domainControllers' instead." #-}

instance Lude.AWSRequest JoinDomain where
  type Rs JoinDomain = JoinDomainResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          JoinDomainResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "ActiveDirectoryStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders JoinDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.JoinDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON JoinDomain where
  toJSON JoinDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            ("OrganizationalUnit" Lude..=) Lude.<$> organizationalUnit,
            Lude.Just ("UserName" Lude..= userName),
            ("TimeoutInSeconds" Lude..=) Lude.<$> timeoutInSeconds,
            Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("Password" Lude..= password),
            ("DomainControllers" Lude..=) Lude.<$> domainControllers
          ]
      )

instance Lude.ToPath JoinDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery JoinDomain where
  toQuery = Lude.const Lude.mempty

-- | JoinDomainOutput
--
-- /See:/ 'mkJoinDomainResponse' smart constructor.
data JoinDomainResponse = JoinDomainResponse'
  { -- | The unique Amazon Resource Name (ARN) of the gateway that joined the domain.
    gatewayARN :: Lude.Maybe Lude.Text,
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
    activeDirectoryStatus :: Lude.Maybe ActiveDirectoryStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JoinDomainResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that joined the domain.
-- * 'activeDirectoryStatus' - Indicates the status of the gateway as a member of the Active Directory domain.
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
-- * 'responseStatus' - The response status code.
mkJoinDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  JoinDomainResponse
mkJoinDomainResponse pResponseStatus_ =
  JoinDomainResponse'
    { gatewayARN = Lude.Nothing,
      activeDirectoryStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique Amazon Resource Name (ARN) of the gateway that joined the domain.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrsGatewayARN :: Lens.Lens' JoinDomainResponse (Lude.Maybe Lude.Text)
jdrsGatewayARN = Lens.lens (gatewayARN :: JoinDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: JoinDomainResponse)
{-# DEPRECATED jdrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

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
jdrsActiveDirectoryStatus :: Lens.Lens' JoinDomainResponse (Lude.Maybe ActiveDirectoryStatus)
jdrsActiveDirectoryStatus = Lens.lens (activeDirectoryStatus :: JoinDomainResponse -> Lude.Maybe ActiveDirectoryStatus) (\s a -> s {activeDirectoryStatus = a} :: JoinDomainResponse)
{-# DEPRECATED jdrsActiveDirectoryStatus "Use generic-lens or generic-optics with 'activeDirectoryStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrsResponseStatus :: Lens.Lens' JoinDomainResponse Lude.Int
jdrsResponseStatus = Lens.lens (responseStatus :: JoinDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: JoinDomainResponse)
{-# DEPRECATED jdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
