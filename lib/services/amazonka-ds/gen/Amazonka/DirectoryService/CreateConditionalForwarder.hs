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
-- Module      : Amazonka.DirectoryService.CreateConditionalForwarder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a conditional forwarder associated with your Amazon Web Services
-- directory. Conditional forwarders are required in order to set up a
-- trust relationship with another domain. The conditional forwarder points
-- to the trusted domain.
module Amazonka.DirectoryService.CreateConditionalForwarder
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Initiates the creation of a conditional forwarder for your Directory
-- Service for Microsoft Active Directory. Conditional forwarders are
-- required in order to set up a trust relationship with another domain.
--
-- /See:/ 'newCreateConditionalForwarder' smart constructor.
data CreateConditionalForwarder = CreateConditionalForwarder'
  { -- | The directory ID of the Amazon Web Services directory for which you are
    -- creating the conditional forwarder.
    directoryId :: Prelude.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which
    -- you will set up a trust relationship.
    remoteDomainName :: Prelude.Text,
    -- | The IP addresses of the remote DNS server associated with
    -- RemoteDomainName.
    dnsIpAddrs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConditionalForwarder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'createConditionalForwarder_directoryId' - The directory ID of the Amazon Web Services directory for which you are
-- creating the conditional forwarder.
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

-- | The directory ID of the Amazon Web Services directory for which you are
-- creating the conditional forwarder.
createConditionalForwarder_directoryId :: Lens.Lens' CreateConditionalForwarder Prelude.Text
createConditionalForwarder_directoryId = Lens.lens (\CreateConditionalForwarder' {directoryId} -> directoryId) (\s@CreateConditionalForwarder' {} a -> s {directoryId = a} :: CreateConditionalForwarder)

-- | The fully qualified domain name (FQDN) of the remote domain with which
-- you will set up a trust relationship.
createConditionalForwarder_remoteDomainName :: Lens.Lens' CreateConditionalForwarder Prelude.Text
createConditionalForwarder_remoteDomainName = Lens.lens (\CreateConditionalForwarder' {remoteDomainName} -> remoteDomainName) (\s@CreateConditionalForwarder' {} a -> s {remoteDomainName = a} :: CreateConditionalForwarder)

-- | The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
createConditionalForwarder_dnsIpAddrs :: Lens.Lens' CreateConditionalForwarder [Prelude.Text]
createConditionalForwarder_dnsIpAddrs = Lens.lens (\CreateConditionalForwarder' {dnsIpAddrs} -> dnsIpAddrs) (\s@CreateConditionalForwarder' {} a -> s {dnsIpAddrs = a} :: CreateConditionalForwarder) Prelude.. Lens.coerced

instance Core.AWSRequest CreateConditionalForwarder where
  type
    AWSResponse CreateConditionalForwarder =
      CreateConditionalForwarderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConditionalForwarderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConditionalForwarder where
  hashWithSalt _salt CreateConditionalForwarder' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` remoteDomainName
      `Prelude.hashWithSalt` dnsIpAddrs

instance Prelude.NFData CreateConditionalForwarder where
  rnf CreateConditionalForwarder' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf remoteDomainName
      `Prelude.seq` Prelude.rnf dnsIpAddrs

instance Data.ToHeaders CreateConditionalForwarder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.CreateConditionalForwarder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConditionalForwarder where
  toJSON CreateConditionalForwarder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just
              ("RemoteDomainName" Data..= remoteDomainName),
            Prelude.Just ("DnsIpAddrs" Data..= dnsIpAddrs)
          ]
      )

instance Data.ToPath CreateConditionalForwarder where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConditionalForwarder where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a CreateConditinalForwarder request.
--
-- /See:/ 'newCreateConditionalForwarderResponse' smart constructor.
data CreateConditionalForwarderResponse = CreateConditionalForwarderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CreateConditionalForwarderResponse' {..} =
    Prelude.rnf httpStatus
