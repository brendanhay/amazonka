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
-- Module      : Network.AWS.DirectoryService.CreateConditionalForwarder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a conditional forwarder associated with your AWS directory.
-- Conditional forwarders are required in order to set up a trust
-- relationship with another domain. The conditional forwarder points to
-- the trusted domain.
module Network.AWS.DirectoryService.CreateConditionalForwarder
  ( -- * Creating a Request
    CreateConditionalForwarder (..),
    newCreateConditionalForwarder,

    -- * Request Lenses
    createConditionalForwarder_directoryId,
    createConditionalForwarder_remoteDomainName,
    createConditionalForwarder_dnsIpAddrs,

    -- * Destructuring the Response
    CreateConditionalForwarderResponse (..),
    newCreateConditionalForwarderResponse,

    -- * Response Lenses
    createConditionalForwarderResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the creation of a conditional forwarder for your AWS Directory
-- Service for Microsoft Active Directory. Conditional forwarders are
-- required in order to set up a trust relationship with another domain.
--
-- /See:/ 'newCreateConditionalForwarder' smart constructor.
data CreateConditionalForwarder = CreateConditionalForwarder'
  { -- | The directory ID of the AWS directory for which you are creating the
    -- conditional forwarder.
    directoryId :: Core.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which
    -- you will set up a trust relationship.
    remoteDomainName :: Core.Text,
    -- | The IP addresses of the remote DNS server associated with
    -- RemoteDomainName.
    dnsIpAddrs :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConditionalForwarder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'createConditionalForwarder_directoryId' - The directory ID of the AWS directory for which you are creating the
-- conditional forwarder.
--
-- 'remoteDomainName', 'createConditionalForwarder_remoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which
-- you will set up a trust relationship.
--
-- 'dnsIpAddrs', 'createConditionalForwarder_dnsIpAddrs' - The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
newCreateConditionalForwarder ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'remoteDomainName'
  Core.Text ->
  CreateConditionalForwarder
newCreateConditionalForwarder
  pDirectoryId_
  pRemoteDomainName_ =
    CreateConditionalForwarder'
      { directoryId =
          pDirectoryId_,
        remoteDomainName = pRemoteDomainName_,
        dnsIpAddrs = Core.mempty
      }

-- | The directory ID of the AWS directory for which you are creating the
-- conditional forwarder.
createConditionalForwarder_directoryId :: Lens.Lens' CreateConditionalForwarder Core.Text
createConditionalForwarder_directoryId = Lens.lens (\CreateConditionalForwarder' {directoryId} -> directoryId) (\s@CreateConditionalForwarder' {} a -> s {directoryId = a} :: CreateConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domain with which
-- you will set up a trust relationship.
createConditionalForwarder_remoteDomainName :: Lens.Lens' CreateConditionalForwarder Core.Text
createConditionalForwarder_remoteDomainName = Lens.lens (\CreateConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@CreateConditionalForwarder' {} a -> s {remoteDomainName = a} :: CreateConditionalForwarder)

-- | The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
createConditionalForwarder_dnsIpAddrs :: Lens.Lens' CreateConditionalForwarder [Core.Text]
createConditionalForwarder_dnsIpAddrs = Lens.lens (\CreateConditionalForwarder' {dnsIpAddrs} -> dnsIpAddrs) (\s@CreateConditionalForwarder' {} a -> s {dnsIpAddrs = a} :: CreateConditionalForwarder) Core.. Lens._Coerce

instance Core.AWSRequest CreateConditionalForwarder where
  type
    AWSResponse CreateConditionalForwarder =
      CreateConditionalForwarderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConditionalForwarderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateConditionalForwarder

instance Core.NFData CreateConditionalForwarder

instance Core.ToHeaders CreateConditionalForwarder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.CreateConditionalForwarder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateConditionalForwarder where
  toJSON CreateConditionalForwarder' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just
              ("RemoteDomainName" Core..= remoteDomainName),
            Core.Just ("DnsIpAddrs" Core..= dnsIpAddrs)
          ]
      )

instance Core.ToPath CreateConditionalForwarder where
  toPath = Core.const "/"

instance Core.ToQuery CreateConditionalForwarder where
  toQuery = Core.const Core.mempty

-- | The result of a CreateConditinalForwarder request.
--
-- /See:/ 'newCreateConditionalForwarderResponse' smart constructor.
data CreateConditionalForwarderResponse = CreateConditionalForwarderResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConditionalForwarderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConditionalForwarderResponse_httpStatus' - The response's http status code.
newCreateConditionalForwarderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConditionalForwarderResponse
newCreateConditionalForwarderResponse pHttpStatus_ =
  CreateConditionalForwarderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createConditionalForwarderResponse_httpStatus :: Lens.Lens' CreateConditionalForwarderResponse Core.Int
createConditionalForwarderResponse_httpStatus = Lens.lens (\CreateConditionalForwarderResponse' {httpStatus} -> httpStatus) (\s@CreateConditionalForwarderResponse' {} a -> s {httpStatus = a} :: CreateConditionalForwarderResponse)

instance
  Core.NFData
    CreateConditionalForwarderResponse
