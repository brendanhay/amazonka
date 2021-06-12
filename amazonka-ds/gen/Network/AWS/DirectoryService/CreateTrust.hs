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
-- Module      : Network.AWS.DirectoryService.CreateTrust
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service for Microsoft Active Directory allows you to
-- configure trust relationships. For example, you can establish a trust
-- between your AWS Managed Microsoft AD directory, and your existing
-- on-premises Microsoft Active Directory. This would allow you to provide
-- users and groups access to resources in either domain, with a single set
-- of credentials.
--
-- This action initiates the creation of the AWS side of a trust
-- relationship between an AWS Managed Microsoft AD directory and an
-- external domain. You can create either a forest trust or an external
-- trust.
module Network.AWS.DirectoryService.CreateTrust
  ( -- * Creating a Request
    CreateTrust (..),
    newCreateTrust,

    -- * Request Lenses
    createTrust_trustType,
    createTrust_selectiveAuth,
    createTrust_conditionalForwarderIpAddrs,
    createTrust_directoryId,
    createTrust_remoteDomainName,
    createTrust_trustPassword,
    createTrust_trustDirection,

    -- * Destructuring the Response
    CreateTrustResponse (..),
    newCreateTrustResponse,

    -- * Response Lenses
    createTrustResponse_trustId,
    createTrustResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | AWS Directory Service for Microsoft Active Directory allows you to
-- configure trust relationships. For example, you can establish a trust
-- between your AWS Managed Microsoft AD directory, and your existing
-- on-premises Microsoft Active Directory. This would allow you to provide
-- users and groups access to resources in either domain, with a single set
-- of credentials.
--
-- This action initiates the creation of the AWS side of a trust
-- relationship between an AWS Managed Microsoft AD directory and an
-- external domain.
--
-- /See:/ 'newCreateTrust' smart constructor.
data CreateTrust = CreateTrust'
  { -- | The trust relationship type. @Forest@ is the default.
    trustType :: Core.Maybe TrustType,
    -- | Optional parameter to enable selective authentication for the trust.
    selectiveAuth :: Core.Maybe SelectiveAuth,
    -- | The IP addresses of the remote DNS server associated with
    -- RemoteDomainName.
    conditionalForwarderIpAddrs :: Core.Maybe [Core.Text],
    -- | The Directory ID of the AWS Managed Microsoft AD directory for which to
    -- establish the trust relationship.
    directoryId :: Core.Text,
    -- | The Fully Qualified Domain Name (FQDN) of the external domain for which
    -- to create the trust relationship.
    remoteDomainName :: Core.Text,
    -- | The trust password. The must be the same password that was used when
    -- creating the trust relationship on the external domain.
    trustPassword :: Core.Sensitive Core.Text,
    -- | The direction of the trust relationship.
    trustDirection :: TrustDirection
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustType', 'createTrust_trustType' - The trust relationship type. @Forest@ is the default.
--
-- 'selectiveAuth', 'createTrust_selectiveAuth' - Optional parameter to enable selective authentication for the trust.
--
-- 'conditionalForwarderIpAddrs', 'createTrust_conditionalForwarderIpAddrs' - The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
--
-- 'directoryId', 'createTrust_directoryId' - The Directory ID of the AWS Managed Microsoft AD directory for which to
-- establish the trust relationship.
--
-- 'remoteDomainName', 'createTrust_remoteDomainName' - The Fully Qualified Domain Name (FQDN) of the external domain for which
-- to create the trust relationship.
--
-- 'trustPassword', 'createTrust_trustPassword' - The trust password. The must be the same password that was used when
-- creating the trust relationship on the external domain.
--
-- 'trustDirection', 'createTrust_trustDirection' - The direction of the trust relationship.
newCreateTrust ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'remoteDomainName'
  Core.Text ->
  -- | 'trustPassword'
  Core.Text ->
  -- | 'trustDirection'
  TrustDirection ->
  CreateTrust
newCreateTrust
  pDirectoryId_
  pRemoteDomainName_
  pTrustPassword_
  pTrustDirection_ =
    CreateTrust'
      { trustType = Core.Nothing,
        selectiveAuth = Core.Nothing,
        conditionalForwarderIpAddrs = Core.Nothing,
        directoryId = pDirectoryId_,
        remoteDomainName = pRemoteDomainName_,
        trustPassword =
          Core._Sensitive Lens.# pTrustPassword_,
        trustDirection = pTrustDirection_
      }

-- | The trust relationship type. @Forest@ is the default.
createTrust_trustType :: Lens.Lens' CreateTrust (Core.Maybe TrustType)
createTrust_trustType = Lens.lens (\CreateTrust' {trustType} -> trustType) (\s@CreateTrust' {} a -> s {trustType = a} :: CreateTrust)

-- | Optional parameter to enable selective authentication for the trust.
createTrust_selectiveAuth :: Lens.Lens' CreateTrust (Core.Maybe SelectiveAuth)
createTrust_selectiveAuth = Lens.lens (\CreateTrust' {selectiveAuth} -> selectiveAuth) (\s@CreateTrust' {} a -> s {selectiveAuth = a} :: CreateTrust)

-- | The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
createTrust_conditionalForwarderIpAddrs :: Lens.Lens' CreateTrust (Core.Maybe [Core.Text])
createTrust_conditionalForwarderIpAddrs = Lens.lens (\CreateTrust' {conditionalForwarderIpAddrs} -> conditionalForwarderIpAddrs) (\s@CreateTrust' {} a -> s {conditionalForwarderIpAddrs = a} :: CreateTrust) Core.. Lens.mapping Lens._Coerce

-- | The Directory ID of the AWS Managed Microsoft AD directory for which to
-- establish the trust relationship.
createTrust_directoryId :: Lens.Lens' CreateTrust Core.Text
createTrust_directoryId = Lens.lens (\CreateTrust' {directoryId} -> directoryId) (\s@CreateTrust' {} a -> s {directoryId = a} :: CreateTrust)

-- | The Fully Qualified Domain Name (FQDN) of the external domain for which
-- to create the trust relationship.
createTrust_remoteDomainName :: Lens.Lens' CreateTrust Core.Text
createTrust_remoteDomainName = Lens.lens (\CreateTrust' {remoteDomainName} -> remoteDomainName) (\s@CreateTrust' {} a -> s {remoteDomainName = a} :: CreateTrust)

-- | The trust password. The must be the same password that was used when
-- creating the trust relationship on the external domain.
createTrust_trustPassword :: Lens.Lens' CreateTrust Core.Text
createTrust_trustPassword = Lens.lens (\CreateTrust' {trustPassword} -> trustPassword) (\s@CreateTrust' {} a -> s {trustPassword = a} :: CreateTrust) Core.. Core._Sensitive

-- | The direction of the trust relationship.
createTrust_trustDirection :: Lens.Lens' CreateTrust TrustDirection
createTrust_trustDirection = Lens.lens (\CreateTrust' {trustDirection} -> trustDirection) (\s@CreateTrust' {} a -> s {trustDirection = a} :: CreateTrust)

instance Core.AWSRequest CreateTrust where
  type AWSResponse CreateTrust = CreateTrustResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrustResponse'
            Core.<$> (x Core..?> "TrustId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTrust

instance Core.NFData CreateTrust

instance Core.ToHeaders CreateTrust where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.CreateTrust" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTrust where
  toJSON CreateTrust' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TrustType" Core..=) Core.<$> trustType,
            ("SelectiveAuth" Core..=) Core.<$> selectiveAuth,
            ("ConditionalForwarderIpAddrs" Core..=)
              Core.<$> conditionalForwarderIpAddrs,
            Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just
              ("RemoteDomainName" Core..= remoteDomainName),
            Core.Just ("TrustPassword" Core..= trustPassword),
            Core.Just ("TrustDirection" Core..= trustDirection)
          ]
      )

instance Core.ToPath CreateTrust where
  toPath = Core.const "/"

instance Core.ToQuery CreateTrust where
  toQuery = Core.const Core.mempty

-- | The result of a CreateTrust request.
--
-- /See:/ 'newCreateTrustResponse' smart constructor.
data CreateTrustResponse = CreateTrustResponse'
  { -- | A unique identifier for the trust relationship that was created.
    trustId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrustResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustId', 'createTrustResponse_trustId' - A unique identifier for the trust relationship that was created.
--
-- 'httpStatus', 'createTrustResponse_httpStatus' - The response's http status code.
newCreateTrustResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTrustResponse
newCreateTrustResponse pHttpStatus_ =
  CreateTrustResponse'
    { trustId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the trust relationship that was created.
createTrustResponse_trustId :: Lens.Lens' CreateTrustResponse (Core.Maybe Core.Text)
createTrustResponse_trustId = Lens.lens (\CreateTrustResponse' {trustId} -> trustId) (\s@CreateTrustResponse' {} a -> s {trustId = a} :: CreateTrustResponse)

-- | The response's http status code.
createTrustResponse_httpStatus :: Lens.Lens' CreateTrustResponse Core.Int
createTrustResponse_httpStatus = Lens.lens (\CreateTrustResponse' {httpStatus} -> httpStatus) (\s@CreateTrustResponse' {} a -> s {httpStatus = a} :: CreateTrustResponse)

instance Core.NFData CreateTrustResponse
