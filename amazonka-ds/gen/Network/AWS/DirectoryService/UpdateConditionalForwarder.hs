{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a conditional forwarder.
--
-- /See:/ 'newUpdateConditionalForwarder' smart constructor.
data UpdateConditionalForwarder = UpdateConditionalForwarder'
  { -- | The directory ID of the AWS directory for which to update the
    -- conditional forwarder.
    directoryId :: Prelude.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which
    -- you will set up a trust relationship.
    remoteDomainName :: Prelude.Text,
    -- | The updated IP addresses of the remote DNS server associated with the
    -- conditional forwarder.
    dnsIpAddrs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'remoteDomainName'
  Prelude.Text ->
  UpdateConditionalForwarder
newUpdateConditionalForwarder
  pDirectoryId_
  pRemoteDomainName_ =
    UpdateConditionalForwarder'
      { directoryId =
          pDirectoryId_,
        remoteDomainName = pRemoteDomainName_,
        dnsIpAddrs = Prelude.mempty
      }

-- | The directory ID of the AWS directory for which to update the
-- conditional forwarder.
updateConditionalForwarder_directoryId :: Lens.Lens' UpdateConditionalForwarder Prelude.Text
updateConditionalForwarder_directoryId = Lens.lens (\UpdateConditionalForwarder' {directoryId} -> directoryId) (\s@UpdateConditionalForwarder' {} a -> s {directoryId = a} :: UpdateConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domain with which
-- you will set up a trust relationship.
updateConditionalForwarder_remoteDomainName :: Lens.Lens' UpdateConditionalForwarder Prelude.Text
updateConditionalForwarder_remoteDomainName = Lens.lens (\UpdateConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@UpdateConditionalForwarder' {} a -> s {remoteDomainName = a} :: UpdateConditionalForwarder)

-- | The updated IP addresses of the remote DNS server associated with the
-- conditional forwarder.
updateConditionalForwarder_dnsIpAddrs :: Lens.Lens' UpdateConditionalForwarder [Prelude.Text]
updateConditionalForwarder_dnsIpAddrs = Lens.lens (\UpdateConditionalForwarder' {dnsIpAddrs} -> dnsIpAddrs) (\s@UpdateConditionalForwarder' {} a -> s {dnsIpAddrs = a} :: UpdateConditionalForwarder) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    UpdateConditionalForwarder
  where
  type
    Rs UpdateConditionalForwarder =
      UpdateConditionalForwarderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConditionalForwarderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConditionalForwarder

instance Prelude.NFData UpdateConditionalForwarder

instance Prelude.ToHeaders UpdateConditionalForwarder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.UpdateConditionalForwarder" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateConditionalForwarder where
  toJSON UpdateConditionalForwarder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just
              ("RemoteDomainName" Prelude..= remoteDomainName),
            Prelude.Just ("DnsIpAddrs" Prelude..= dnsIpAddrs)
          ]
      )

instance Prelude.ToPath UpdateConditionalForwarder where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateConditionalForwarder where
  toQuery = Prelude.const Prelude.mempty

-- | The result of an UpdateConditionalForwarder request.
--
-- /See:/ 'newUpdateConditionalForwarderResponse' smart constructor.
data UpdateConditionalForwarderResponse = UpdateConditionalForwarderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateConditionalForwarderResponse
newUpdateConditionalForwarderResponse pHttpStatus_ =
  UpdateConditionalForwarderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConditionalForwarderResponse_httpStatus :: Lens.Lens' UpdateConditionalForwarderResponse Prelude.Int
updateConditionalForwarderResponse_httpStatus = Lens.lens (\UpdateConditionalForwarderResponse' {httpStatus} -> httpStatus) (\s@UpdateConditionalForwarderResponse' {} a -> s {httpStatus = a} :: UpdateConditionalForwarderResponse)

instance
  Prelude.NFData
    UpdateConditionalForwarderResponse
