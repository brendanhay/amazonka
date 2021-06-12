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
-- Module      : Network.AWS.StorageGateway.JoinDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a file gateway to an Active Directory domain. This operation is
-- only supported for file gateways that support the SMB file protocol.
module Network.AWS.StorageGateway.JoinDomain
  ( -- * Creating a Request
    JoinDomain (..),
    newJoinDomain,

    -- * Request Lenses
    joinDomain_domainControllers,
    joinDomain_organizationalUnit,
    joinDomain_timeoutInSeconds,
    joinDomain_gatewayARN,
    joinDomain_domainName,
    joinDomain_userName,
    joinDomain_password,

    -- * Destructuring the Response
    JoinDomainResponse (..),
    newJoinDomainResponse,

    -- * Response Lenses
    joinDomainResponse_activeDirectoryStatus,
    joinDomainResponse_gatewayARN,
    joinDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | JoinDomainInput
--
-- /See:/ 'newJoinDomain' smart constructor.
data JoinDomain = JoinDomain'
  { -- | List of IPv4 addresses, NetBIOS names, or host names of your domain
    -- server. If you need to specify the port number include it after the
    -- colon (“:”). For example, @mydc.mydomain.com:389@.
    domainControllers :: Core.Maybe [Core.Text],
    -- | The organizational unit (OU) is a container in an Active Directory that
    -- can hold users, groups, computers, and other OUs and this parameter
    -- specifies the OU that the gateway will join within the AD domain.
    organizationalUnit :: Core.Maybe Core.Text,
    -- | Specifies the time in seconds, in which the @JoinDomain@ operation must
    -- complete. The default is 20 seconds.
    timeoutInSeconds :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
    -- operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Core.Text,
    -- | The name of the domain that you want the gateway to join.
    domainName :: Core.Text,
    -- | Sets the user name of user who has permission to add the gateway to the
    -- Active Directory domain. The domain user account should be enabled to
    -- join computers to the domain. For example, you can use the domain
    -- administrator account or an account with delegated permissions to join
    -- computers to the domain.
    userName :: Core.Text,
    -- | Sets the password of the user who has permission to add the gateway to
    -- the Active Directory domain.
    password :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'JoinDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainControllers', 'joinDomain_domainControllers' - List of IPv4 addresses, NetBIOS names, or host names of your domain
-- server. If you need to specify the port number include it after the
-- colon (“:”). For example, @mydc.mydomain.com:389@.
--
-- 'organizationalUnit', 'joinDomain_organizationalUnit' - The organizational unit (OU) is a container in an Active Directory that
-- can hold users, groups, computers, and other OUs and this parameter
-- specifies the OU that the gateway will join within the AD domain.
--
-- 'timeoutInSeconds', 'joinDomain_timeoutInSeconds' - Specifies the time in seconds, in which the @JoinDomain@ operation must
-- complete. The default is 20 seconds.
--
-- 'gatewayARN', 'joinDomain_gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and AWS Region.
--
-- 'domainName', 'joinDomain_domainName' - The name of the domain that you want the gateway to join.
--
-- 'userName', 'joinDomain_userName' - Sets the user name of user who has permission to add the gateway to the
-- Active Directory domain. The domain user account should be enabled to
-- join computers to the domain. For example, you can use the domain
-- administrator account or an account with delegated permissions to join
-- computers to the domain.
--
-- 'password', 'joinDomain_password' - Sets the password of the user who has permission to add the gateway to
-- the Active Directory domain.
newJoinDomain ::
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'domainName'
  Core.Text ->
  -- | 'userName'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  JoinDomain
newJoinDomain
  pGatewayARN_
  pDomainName_
  pUserName_
  pPassword_ =
    JoinDomain'
      { domainControllers = Core.Nothing,
        organizationalUnit = Core.Nothing,
        timeoutInSeconds = Core.Nothing,
        gatewayARN = pGatewayARN_,
        domainName = pDomainName_,
        userName = pUserName_,
        password = Core._Sensitive Lens.# pPassword_
      }

-- | List of IPv4 addresses, NetBIOS names, or host names of your domain
-- server. If you need to specify the port number include it after the
-- colon (“:”). For example, @mydc.mydomain.com:389@.
joinDomain_domainControllers :: Lens.Lens' JoinDomain (Core.Maybe [Core.Text])
joinDomain_domainControllers = Lens.lens (\JoinDomain' {domainControllers} -> domainControllers) (\s@JoinDomain' {} a -> s {domainControllers = a} :: JoinDomain) Core.. Lens.mapping Lens._Coerce

-- | The organizational unit (OU) is a container in an Active Directory that
-- can hold users, groups, computers, and other OUs and this parameter
-- specifies the OU that the gateway will join within the AD domain.
joinDomain_organizationalUnit :: Lens.Lens' JoinDomain (Core.Maybe Core.Text)
joinDomain_organizationalUnit = Lens.lens (\JoinDomain' {organizationalUnit} -> organizationalUnit) (\s@JoinDomain' {} a -> s {organizationalUnit = a} :: JoinDomain)

-- | Specifies the time in seconds, in which the @JoinDomain@ operation must
-- complete. The default is 20 seconds.
joinDomain_timeoutInSeconds :: Lens.Lens' JoinDomain (Core.Maybe Core.Natural)
joinDomain_timeoutInSeconds = Lens.lens (\JoinDomain' {timeoutInSeconds} -> timeoutInSeconds) (\s@JoinDomain' {} a -> s {timeoutInSeconds = a} :: JoinDomain)

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and AWS Region.
joinDomain_gatewayARN :: Lens.Lens' JoinDomain Core.Text
joinDomain_gatewayARN = Lens.lens (\JoinDomain' {gatewayARN} -> gatewayARN) (\s@JoinDomain' {} a -> s {gatewayARN = a} :: JoinDomain)

-- | The name of the domain that you want the gateway to join.
joinDomain_domainName :: Lens.Lens' JoinDomain Core.Text
joinDomain_domainName = Lens.lens (\JoinDomain' {domainName} -> domainName) (\s@JoinDomain' {} a -> s {domainName = a} :: JoinDomain)

-- | Sets the user name of user who has permission to add the gateway to the
-- Active Directory domain. The domain user account should be enabled to
-- join computers to the domain. For example, you can use the domain
-- administrator account or an account with delegated permissions to join
-- computers to the domain.
joinDomain_userName :: Lens.Lens' JoinDomain Core.Text
joinDomain_userName = Lens.lens (\JoinDomain' {userName} -> userName) (\s@JoinDomain' {} a -> s {userName = a} :: JoinDomain)

-- | Sets the password of the user who has permission to add the gateway to
-- the Active Directory domain.
joinDomain_password :: Lens.Lens' JoinDomain Core.Text
joinDomain_password = Lens.lens (\JoinDomain' {password} -> password) (\s@JoinDomain' {} a -> s {password = a} :: JoinDomain) Core.. Core._Sensitive

instance Core.AWSRequest JoinDomain where
  type AWSResponse JoinDomain = JoinDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          JoinDomainResponse'
            Core.<$> (x Core..?> "ActiveDirectoryStatus")
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable JoinDomain

instance Core.NFData JoinDomain

instance Core.ToHeaders JoinDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.JoinDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON JoinDomain where
  toJSON JoinDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DomainControllers" Core..=)
              Core.<$> domainControllers,
            ("OrganizationalUnit" Core..=)
              Core.<$> organizationalUnit,
            ("TimeoutInSeconds" Core..=)
              Core.<$> timeoutInSeconds,
            Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("DomainName" Core..= domainName),
            Core.Just ("UserName" Core..= userName),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath JoinDomain where
  toPath = Core.const "/"

instance Core.ToQuery JoinDomain where
  toQuery = Core.const Core.mempty

-- | JoinDomainOutput
--
-- /See:/ 'newJoinDomainResponse' smart constructor.
data JoinDomainResponse = JoinDomainResponse'
  { -- | Indicates the status of the gateway as a member of the Active Directory
    -- domain.
    --
    -- -   @ACCESS_DENIED@: Indicates that the @JoinDomain@ operation failed
    --     due to an authentication error.
    --
    -- -   @DETACHED@: Indicates that gateway is not joined to a domain.
    --
    -- -   @JOINED@: Indicates that the gateway has successfully joined a
    --     domain.
    --
    -- -   @JOINING@: Indicates that a @JoinDomain@ operation is in progress.
    --
    -- -   @NETWORK_ERROR@: Indicates that @JoinDomain@ operation failed due to
    --     a network or connectivity error.
    --
    -- -   @TIMEOUT@: Indicates that the @JoinDomain@ operation failed because
    --     the operation didn\'t complete within the allotted time.
    --
    -- -   @UNKNOWN_ERROR@: Indicates that the @JoinDomain@ operation failed
    --     due to another type of error.
    activeDirectoryStatus :: Core.Maybe ActiveDirectoryStatus,
    -- | The unique Amazon Resource Name (ARN) of the gateway that joined the
    -- domain.
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JoinDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryStatus', 'joinDomainResponse_activeDirectoryStatus' - Indicates the status of the gateway as a member of the Active Directory
-- domain.
--
-- -   @ACCESS_DENIED@: Indicates that the @JoinDomain@ operation failed
--     due to an authentication error.
--
-- -   @DETACHED@: Indicates that gateway is not joined to a domain.
--
-- -   @JOINED@: Indicates that the gateway has successfully joined a
--     domain.
--
-- -   @JOINING@: Indicates that a @JoinDomain@ operation is in progress.
--
-- -   @NETWORK_ERROR@: Indicates that @JoinDomain@ operation failed due to
--     a network or connectivity error.
--
-- -   @TIMEOUT@: Indicates that the @JoinDomain@ operation failed because
--     the operation didn\'t complete within the allotted time.
--
-- -   @UNKNOWN_ERROR@: Indicates that the @JoinDomain@ operation failed
--     due to another type of error.
--
-- 'gatewayARN', 'joinDomainResponse_gatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that joined the
-- domain.
--
-- 'httpStatus', 'joinDomainResponse_httpStatus' - The response's http status code.
newJoinDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  JoinDomainResponse
newJoinDomainResponse pHttpStatus_ =
  JoinDomainResponse'
    { activeDirectoryStatus =
        Core.Nothing,
      gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the status of the gateway as a member of the Active Directory
-- domain.
--
-- -   @ACCESS_DENIED@: Indicates that the @JoinDomain@ operation failed
--     due to an authentication error.
--
-- -   @DETACHED@: Indicates that gateway is not joined to a domain.
--
-- -   @JOINED@: Indicates that the gateway has successfully joined a
--     domain.
--
-- -   @JOINING@: Indicates that a @JoinDomain@ operation is in progress.
--
-- -   @NETWORK_ERROR@: Indicates that @JoinDomain@ operation failed due to
--     a network or connectivity error.
--
-- -   @TIMEOUT@: Indicates that the @JoinDomain@ operation failed because
--     the operation didn\'t complete within the allotted time.
--
-- -   @UNKNOWN_ERROR@: Indicates that the @JoinDomain@ operation failed
--     due to another type of error.
joinDomainResponse_activeDirectoryStatus :: Lens.Lens' JoinDomainResponse (Core.Maybe ActiveDirectoryStatus)
joinDomainResponse_activeDirectoryStatus = Lens.lens (\JoinDomainResponse' {activeDirectoryStatus} -> activeDirectoryStatus) (\s@JoinDomainResponse' {} a -> s {activeDirectoryStatus = a} :: JoinDomainResponse)

-- | The unique Amazon Resource Name (ARN) of the gateway that joined the
-- domain.
joinDomainResponse_gatewayARN :: Lens.Lens' JoinDomainResponse (Core.Maybe Core.Text)
joinDomainResponse_gatewayARN = Lens.lens (\JoinDomainResponse' {gatewayARN} -> gatewayARN) (\s@JoinDomainResponse' {} a -> s {gatewayARN = a} :: JoinDomainResponse)

-- | The response's http status code.
joinDomainResponse_httpStatus :: Lens.Lens' JoinDomainResponse Core.Int
joinDomainResponse_httpStatus = Lens.lens (\JoinDomainResponse' {httpStatus} -> httpStatus) (\s@JoinDomainResponse' {} a -> s {httpStatus = a} :: JoinDomainResponse)

instance Core.NFData JoinDomainResponse
