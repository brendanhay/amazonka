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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    directoryId :: Prelude.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which
    -- you will set up a trust relationship.
    remoteDomainName :: Prelude.Text,
    -- | The IP addresses of the remote DNS server associated with
    -- RemoteDomainName.
    dnsIpAddrs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'remoteDomainName'
  Prelude.Text ->
  CreateConditionalForwarder
newCreateConditionalForwarder
  pDirectoryId_
  pRemoteDomainName_ =
    CreateConditionalForwarder'
      { directoryId =
          pDirectoryId_,
        remoteDomainName = pRemoteDomainName_,
        dnsIpAddrs = Prelude.mempty
      }

-- | The directory ID of the AWS directory for which you are creating the
-- conditional forwarder.
createConditionalForwarder_directoryId :: Lens.Lens' CreateConditionalForwarder Prelude.Text
createConditionalForwarder_directoryId = Lens.lens (\CreateConditionalForwarder' {directoryId} -> directoryId) (\s@CreateConditionalForwarder' {} a -> s {directoryId = a} :: CreateConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domain with which
-- you will set up a trust relationship.
createConditionalForwarder_remoteDomainName :: Lens.Lens' CreateConditionalForwarder Prelude.Text
createConditionalForwarder_remoteDomainName = Lens.lens (\CreateConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@CreateConditionalForwarder' {} a -> s {remoteDomainName = a} :: CreateConditionalForwarder)

-- | The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
createConditionalForwarder_dnsIpAddrs :: Lens.Lens' CreateConditionalForwarder [Prelude.Text]
createConditionalForwarder_dnsIpAddrs = Lens.lens (\CreateConditionalForwarder' {dnsIpAddrs} -> dnsIpAddrs) (\s@CreateConditionalForwarder' {} a -> s {dnsIpAddrs = a} :: CreateConditionalForwarder) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    CreateConditionalForwarder
  where
  type
    Rs CreateConditionalForwarder =
      CreateConditionalForwarderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConditionalForwarderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConditionalForwarder

instance Prelude.NFData CreateConditionalForwarder

instance Prelude.ToHeaders CreateConditionalForwarder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.CreateConditionalForwarder" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateConditionalForwarder where
  toJSON CreateConditionalForwarder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just
              ("RemoteDomainName" Prelude..= remoteDomainName),
            Prelude.Just ("DnsIpAddrs" Prelude..= dnsIpAddrs)
          ]
      )

instance Prelude.ToPath CreateConditionalForwarder where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateConditionalForwarder where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a CreateConditinalForwarder request.
--
-- /See:/ 'newCreateConditionalForwarderResponse' smart constructor.
data CreateConditionalForwarderResponse = CreateConditionalForwarderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateConditionalForwarderResponse
newCreateConditionalForwarderResponse pHttpStatus_ =
  CreateConditionalForwarderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createConditionalForwarderResponse_httpStatus :: Lens.Lens' CreateConditionalForwarderResponse Prelude.Int
createConditionalForwarderResponse_httpStatus = Lens.lens (\CreateConditionalForwarderResponse' {httpStatus} -> httpStatus) (\s@CreateConditionalForwarderResponse' {} a -> s {httpStatus = a} :: CreateConditionalForwarderResponse)

instance
  Prelude.NFData
    CreateConditionalForwarderResponse
