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
-- Module      : Amazonka.StorageGateway.JoinDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a file gateway to an Active Directory domain. This operation is
-- only supported for file gateways that support the SMB file protocol.
module Amazonka.StorageGateway.JoinDomain
  ( -- * Creating a Request
    JoinDomain (..),
    newJoinDomain,

    -- * Request Lenses
    joinDomain_organizationalUnit,
    joinDomain_timeoutInSeconds,
    joinDomain_domainControllers,
    joinDomain_gatewayARN,
    joinDomain_domainName,
    joinDomain_userName,
    joinDomain_password,

    -- * Destructuring the Response
    JoinDomainResponse (..),
    newJoinDomainResponse,

    -- * Response Lenses
    joinDomainResponse_gatewayARN,
    joinDomainResponse_activeDirectoryStatus,
    joinDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | JoinDomainInput
--
-- /See:/ 'newJoinDomain' smart constructor.
data JoinDomain = JoinDomain'
  { -- | The organizational unit (OU) is a container in an Active Directory that
    -- can hold users, groups, computers, and other OUs and this parameter
    -- specifies the OU that the gateway will join within the AD domain.
    organizationalUnit :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time in seconds, in which the @JoinDomain@ operation must
    -- complete. The default is 20 seconds.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | List of IPv4 addresses, NetBIOS names, or host names of your domain
    -- server. If you need to specify the port number include it after the
    -- colon (“:”). For example, @mydc.mydomain.com:389@.
    domainControllers :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayARN :: Prelude.Text,
    -- | The name of the domain that you want the gateway to join.
    domainName :: Prelude.Text,
    -- | Sets the user name of user who has permission to add the gateway to the
    -- Active Directory domain. The domain user account should be enabled to
    -- join computers to the domain. For example, you can use the domain
    -- administrator account or an account with delegated permissions to join
    -- computers to the domain.
    userName :: Prelude.Text,
    -- | Sets the password of the user who has permission to add the gateway to
    -- the Active Directory domain.
    password :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnit', 'joinDomain_organizationalUnit' - The organizational unit (OU) is a container in an Active Directory that
-- can hold users, groups, computers, and other OUs and this parameter
-- specifies the OU that the gateway will join within the AD domain.
--
-- 'timeoutInSeconds', 'joinDomain_timeoutInSeconds' - Specifies the time in seconds, in which the @JoinDomain@ operation must
-- complete. The default is 20 seconds.
--
-- 'domainControllers', 'joinDomain_domainControllers' - List of IPv4 addresses, NetBIOS names, or host names of your domain
-- server. If you need to specify the port number include it after the
-- colon (“:”). For example, @mydc.mydomain.com:389@.
--
-- 'gatewayARN', 'joinDomain_gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
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
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  JoinDomain
newJoinDomain
  pGatewayARN_
  pDomainName_
  pUserName_
  pPassword_ =
    JoinDomain'
      { organizationalUnit = Prelude.Nothing,
        timeoutInSeconds = Prelude.Nothing,
        domainControllers = Prelude.Nothing,
        gatewayARN = pGatewayARN_,
        domainName = pDomainName_,
        userName = pUserName_,
        password = Core._Sensitive Lens.# pPassword_
      }

-- | The organizational unit (OU) is a container in an Active Directory that
-- can hold users, groups, computers, and other OUs and this parameter
-- specifies the OU that the gateway will join within the AD domain.
joinDomain_organizationalUnit :: Lens.Lens' JoinDomain (Prelude.Maybe Prelude.Text)
joinDomain_organizationalUnit = Lens.lens (\JoinDomain' {organizationalUnit} -> organizationalUnit) (\s@JoinDomain' {} a -> s {organizationalUnit = a} :: JoinDomain)

-- | Specifies the time in seconds, in which the @JoinDomain@ operation must
-- complete. The default is 20 seconds.
joinDomain_timeoutInSeconds :: Lens.Lens' JoinDomain (Prelude.Maybe Prelude.Natural)
joinDomain_timeoutInSeconds = Lens.lens (\JoinDomain' {timeoutInSeconds} -> timeoutInSeconds) (\s@JoinDomain' {} a -> s {timeoutInSeconds = a} :: JoinDomain)

-- | List of IPv4 addresses, NetBIOS names, or host names of your domain
-- server. If you need to specify the port number include it after the
-- colon (“:”). For example, @mydc.mydomain.com:389@.
joinDomain_domainControllers :: Lens.Lens' JoinDomain (Prelude.Maybe [Prelude.Text])
joinDomain_domainControllers = Lens.lens (\JoinDomain' {domainControllers} -> domainControllers) (\s@JoinDomain' {} a -> s {domainControllers = a} :: JoinDomain) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
joinDomain_gatewayARN :: Lens.Lens' JoinDomain Prelude.Text
joinDomain_gatewayARN = Lens.lens (\JoinDomain' {gatewayARN} -> gatewayARN) (\s@JoinDomain' {} a -> s {gatewayARN = a} :: JoinDomain)

-- | The name of the domain that you want the gateway to join.
joinDomain_domainName :: Lens.Lens' JoinDomain Prelude.Text
joinDomain_domainName = Lens.lens (\JoinDomain' {domainName} -> domainName) (\s@JoinDomain' {} a -> s {domainName = a} :: JoinDomain)

-- | Sets the user name of user who has permission to add the gateway to the
-- Active Directory domain. The domain user account should be enabled to
-- join computers to the domain. For example, you can use the domain
-- administrator account or an account with delegated permissions to join
-- computers to the domain.
joinDomain_userName :: Lens.Lens' JoinDomain Prelude.Text
joinDomain_userName = Lens.lens (\JoinDomain' {userName} -> userName) (\s@JoinDomain' {} a -> s {userName = a} :: JoinDomain)

-- | Sets the password of the user who has permission to add the gateway to
-- the Active Directory domain.
joinDomain_password :: Lens.Lens' JoinDomain Prelude.Text
joinDomain_password = Lens.lens (\JoinDomain' {password} -> password) (\s@JoinDomain' {} a -> s {password = a} :: JoinDomain) Prelude.. Core._Sensitive

instance Core.AWSRequest JoinDomain where
  type AWSResponse JoinDomain = JoinDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          JoinDomainResponse'
            Prelude.<$> (x Core..?> "GatewayARN")
            Prelude.<*> (x Core..?> "ActiveDirectoryStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable JoinDomain where
  hashWithSalt _salt JoinDomain' {..} =
    _salt `Prelude.hashWithSalt` organizationalUnit
      `Prelude.hashWithSalt` timeoutInSeconds
      `Prelude.hashWithSalt` domainControllers
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` password

instance Prelude.NFData JoinDomain where
  rnf JoinDomain' {..} =
    Prelude.rnf organizationalUnit
      `Prelude.seq` Prelude.rnf timeoutInSeconds
      `Prelude.seq` Prelude.rnf domainControllers
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf password

instance Core.ToHeaders JoinDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.JoinDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON JoinDomain where
  toJSON JoinDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OrganizationalUnit" Core..=)
              Prelude.<$> organizationalUnit,
            ("TimeoutInSeconds" Core..=)
              Prelude.<$> timeoutInSeconds,
            ("DomainControllers" Core..=)
              Prelude.<$> domainControllers,
            Prelude.Just ("GatewayARN" Core..= gatewayARN),
            Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just ("UserName" Core..= userName),
            Prelude.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath JoinDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery JoinDomain where
  toQuery = Prelude.const Prelude.mempty

-- | JoinDomainOutput
--
-- /See:/ 'newJoinDomainResponse' smart constructor.
data JoinDomainResponse = JoinDomainResponse'
  { -- | The unique Amazon Resource Name (ARN) of the gateway that joined the
    -- domain.
    gatewayARN :: Prelude.Maybe Prelude.Text,
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
    activeDirectoryStatus :: Prelude.Maybe ActiveDirectoryStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'joinDomainResponse_gatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that joined the
-- domain.
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
-- 'httpStatus', 'joinDomainResponse_httpStatus' - The response's http status code.
newJoinDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  JoinDomainResponse
newJoinDomainResponse pHttpStatus_ =
  JoinDomainResponse'
    { gatewayARN = Prelude.Nothing,
      activeDirectoryStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Amazon Resource Name (ARN) of the gateway that joined the
-- domain.
joinDomainResponse_gatewayARN :: Lens.Lens' JoinDomainResponse (Prelude.Maybe Prelude.Text)
joinDomainResponse_gatewayARN = Lens.lens (\JoinDomainResponse' {gatewayARN} -> gatewayARN) (\s@JoinDomainResponse' {} a -> s {gatewayARN = a} :: JoinDomainResponse)

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
joinDomainResponse_activeDirectoryStatus :: Lens.Lens' JoinDomainResponse (Prelude.Maybe ActiveDirectoryStatus)
joinDomainResponse_activeDirectoryStatus = Lens.lens (\JoinDomainResponse' {activeDirectoryStatus} -> activeDirectoryStatus) (\s@JoinDomainResponse' {} a -> s {activeDirectoryStatus = a} :: JoinDomainResponse)

-- | The response's http status code.
joinDomainResponse_httpStatus :: Lens.Lens' JoinDomainResponse Prelude.Int
joinDomainResponse_httpStatus = Lens.lens (\JoinDomainResponse' {httpStatus} -> httpStatus) (\s@JoinDomainResponse' {} a -> s {httpStatus = a} :: JoinDomainResponse)

instance Prelude.NFData JoinDomainResponse where
  rnf JoinDomainResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf activeDirectoryStatus
      `Prelude.seq` Prelude.rnf httpStatus
