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
-- Module      : Network.AWS.DirectoryService.UpdateConditionalForwarder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a conditional forwarder that has been set up for your AWS
-- directory.
module Network.AWS.DirectoryService.UpdateConditionalForwarder
  ( -- * Creating a Request
    UpdateConditionalForwarder (..),
    newUpdateConditionalForwarder,

    -- * Request Lenses
    updateConditionalForwarder_directoryId,
    updateConditionalForwarder_remoteDomainName,
    updateConditionalForwarder_dnsIpAddrs,

    -- * Destructuring the Response
    UpdateConditionalForwarderResponse (..),
    newUpdateConditionalForwarderResponse,

    -- * Response Lenses
    updateConditionalForwarderResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a conditional forwarder.
--
-- /See:/ 'newUpdateConditionalForwarder' smart constructor.
data UpdateConditionalForwarder = UpdateConditionalForwarder'
  { -- | The directory ID of the AWS directory for which to update the
    -- conditional forwarder.
    directoryId :: Core.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which
    -- you will set up a trust relationship.
    remoteDomainName :: Core.Text,
    -- | The updated IP addresses of the remote DNS server associated with the
    -- conditional forwarder.
    dnsIpAddrs :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConditionalForwarder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'updateConditionalForwarder_directoryId' - The directory ID of the AWS directory for which to update the
-- conditional forwarder.
--
-- 'remoteDomainName', 'updateConditionalForwarder_remoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which
-- you will set up a trust relationship.
--
-- 'dnsIpAddrs', 'updateConditionalForwarder_dnsIpAddrs' - The updated IP addresses of the remote DNS server associated with the
-- conditional forwarder.
newUpdateConditionalForwarder ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'remoteDomainName'
  Core.Text ->
  UpdateConditionalForwarder
newUpdateConditionalForwarder
  pDirectoryId_
  pRemoteDomainName_ =
    UpdateConditionalForwarder'
      { directoryId =
          pDirectoryId_,
        remoteDomainName = pRemoteDomainName_,
        dnsIpAddrs = Core.mempty
      }

-- | The directory ID of the AWS directory for which to update the
-- conditional forwarder.
updateConditionalForwarder_directoryId :: Lens.Lens' UpdateConditionalForwarder Core.Text
updateConditionalForwarder_directoryId = Lens.lens (\UpdateConditionalForwarder' {directoryId} -> directoryId) (\s@UpdateConditionalForwarder' {} a -> s {directoryId = a} :: UpdateConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domain with which
-- you will set up a trust relationship.
updateConditionalForwarder_remoteDomainName :: Lens.Lens' UpdateConditionalForwarder Core.Text
updateConditionalForwarder_remoteDomainName = Lens.lens (\UpdateConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@UpdateConditionalForwarder' {} a -> s {remoteDomainName = a} :: UpdateConditionalForwarder)

-- | The updated IP addresses of the remote DNS server associated with the
-- conditional forwarder.
updateConditionalForwarder_dnsIpAddrs :: Lens.Lens' UpdateConditionalForwarder [Core.Text]
updateConditionalForwarder_dnsIpAddrs = Lens.lens (\UpdateConditionalForwarder' {dnsIpAddrs} -> dnsIpAddrs) (\s@UpdateConditionalForwarder' {} a -> s {dnsIpAddrs = a} :: UpdateConditionalForwarder) Core.. Lens._Coerce

instance Core.AWSRequest UpdateConditionalForwarder where
  type
    AWSResponse UpdateConditionalForwarder =
      UpdateConditionalForwarderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConditionalForwarderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateConditionalForwarder

instance Core.NFData UpdateConditionalForwarder

instance Core.ToHeaders UpdateConditionalForwarder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.UpdateConditionalForwarder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateConditionalForwarder where
  toJSON UpdateConditionalForwarder' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just
              ("RemoteDomainName" Core..= remoteDomainName),
            Core.Just ("DnsIpAddrs" Core..= dnsIpAddrs)
          ]
      )

instance Core.ToPath UpdateConditionalForwarder where
  toPath = Core.const "/"

instance Core.ToQuery UpdateConditionalForwarder where
  toQuery = Core.const Core.mempty

-- | The result of an UpdateConditionalForwarder request.
--
-- /See:/ 'newUpdateConditionalForwarderResponse' smart constructor.
data UpdateConditionalForwarderResponse = UpdateConditionalForwarderResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConditionalForwarderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConditionalForwarderResponse_httpStatus' - The response's http status code.
newUpdateConditionalForwarderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateConditionalForwarderResponse
newUpdateConditionalForwarderResponse pHttpStatus_ =
  UpdateConditionalForwarderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConditionalForwarderResponse_httpStatus :: Lens.Lens' UpdateConditionalForwarderResponse Core.Int
updateConditionalForwarderResponse_httpStatus = Lens.lens (\UpdateConditionalForwarderResponse' {httpStatus} -> httpStatus) (\s@UpdateConditionalForwarderResponse' {} a -> s {httpStatus = a} :: UpdateConditionalForwarderResponse)

instance
  Core.NFData
    UpdateConditionalForwarderResponse
