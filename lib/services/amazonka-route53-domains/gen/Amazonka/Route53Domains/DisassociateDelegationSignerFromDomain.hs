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
-- Module      : Amazonka.Route53Domains.DisassociateDelegationSignerFromDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a delegation signer (DS) record in the registry zone for this
-- domain name.
module Amazonka.Route53Domains.DisassociateDelegationSignerFromDomain
  ( -- * Creating a Request
    DisassociateDelegationSignerFromDomain (..),
    newDisassociateDelegationSignerFromDomain,

    -- * Request Lenses
    disassociateDelegationSignerFromDomain_domainName,
    disassociateDelegationSignerFromDomain_id,

    -- * Destructuring the Response
    DisassociateDelegationSignerFromDomainResponse (..),
    newDisassociateDelegationSignerFromDomainResponse,

    -- * Response Lenses
    disassociateDelegationSignerFromDomainResponse_operationId,
    disassociateDelegationSignerFromDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newDisassociateDelegationSignerFromDomain' smart constructor.
data DisassociateDelegationSignerFromDomain = DisassociateDelegationSignerFromDomain'
  { -- | Name of the domain.
    domainName :: Prelude.Text,
    -- | An internal identification number assigned to each DS record after it’s
    -- created. You can retrieve it as part of DNSSEC information returned by
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetDomainDetail.html GetDomainDetail>.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDelegationSignerFromDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'disassociateDelegationSignerFromDomain_domainName' - Name of the domain.
--
-- 'id', 'disassociateDelegationSignerFromDomain_id' - An internal identification number assigned to each DS record after it’s
-- created. You can retrieve it as part of DNSSEC information returned by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetDomainDetail.html GetDomainDetail>.
newDisassociateDelegationSignerFromDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DisassociateDelegationSignerFromDomain
newDisassociateDelegationSignerFromDomain
  pDomainName_
  pId_ =
    DisassociateDelegationSignerFromDomain'
      { domainName =
          pDomainName_,
        id = pId_
      }

-- | Name of the domain.
disassociateDelegationSignerFromDomain_domainName :: Lens.Lens' DisassociateDelegationSignerFromDomain Prelude.Text
disassociateDelegationSignerFromDomain_domainName = Lens.lens (\DisassociateDelegationSignerFromDomain' {domainName} -> domainName) (\s@DisassociateDelegationSignerFromDomain' {} a -> s {domainName = a} :: DisassociateDelegationSignerFromDomain)

-- | An internal identification number assigned to each DS record after it’s
-- created. You can retrieve it as part of DNSSEC information returned by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetDomainDetail.html GetDomainDetail>.
disassociateDelegationSignerFromDomain_id :: Lens.Lens' DisassociateDelegationSignerFromDomain Prelude.Text
disassociateDelegationSignerFromDomain_id = Lens.lens (\DisassociateDelegationSignerFromDomain' {id} -> id) (\s@DisassociateDelegationSignerFromDomain' {} a -> s {id = a} :: DisassociateDelegationSignerFromDomain)

instance
  Core.AWSRequest
    DisassociateDelegationSignerFromDomain
  where
  type
    AWSResponse
      DisassociateDelegationSignerFromDomain =
      DisassociateDelegationSignerFromDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateDelegationSignerFromDomainResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateDelegationSignerFromDomain
  where
  hashWithSalt
    _salt
    DisassociateDelegationSignerFromDomain' {..} =
      _salt
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DisassociateDelegationSignerFromDomain
  where
  rnf DisassociateDelegationSignerFromDomain' {..} =
    Prelude.rnf domainName `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DisassociateDelegationSignerFromDomain
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.DisassociateDelegationSignerFromDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateDelegationSignerFromDomain
  where
  toJSON DisassociateDelegationSignerFromDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance
  Data.ToPath
    DisassociateDelegationSignerFromDomain
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateDelegationSignerFromDomain
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDelegationSignerFromDomainResponse' smart constructor.
data DisassociateDelegationSignerFromDomainResponse = DisassociateDelegationSignerFromDomainResponse'
  { -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDelegationSignerFromDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'disassociateDelegationSignerFromDomainResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'httpStatus', 'disassociateDelegationSignerFromDomainResponse_httpStatus' - The response's http status code.
newDisassociateDelegationSignerFromDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateDelegationSignerFromDomainResponse
newDisassociateDelegationSignerFromDomainResponse
  pHttpStatus_ =
    DisassociateDelegationSignerFromDomainResponse'
      { operationId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
disassociateDelegationSignerFromDomainResponse_operationId :: Lens.Lens' DisassociateDelegationSignerFromDomainResponse (Prelude.Maybe Prelude.Text)
disassociateDelegationSignerFromDomainResponse_operationId = Lens.lens (\DisassociateDelegationSignerFromDomainResponse' {operationId} -> operationId) (\s@DisassociateDelegationSignerFromDomainResponse' {} a -> s {operationId = a} :: DisassociateDelegationSignerFromDomainResponse)

-- | The response's http status code.
disassociateDelegationSignerFromDomainResponse_httpStatus :: Lens.Lens' DisassociateDelegationSignerFromDomainResponse Prelude.Int
disassociateDelegationSignerFromDomainResponse_httpStatus = Lens.lens (\DisassociateDelegationSignerFromDomainResponse' {httpStatus} -> httpStatus) (\s@DisassociateDelegationSignerFromDomainResponse' {} a -> s {httpStatus = a} :: DisassociateDelegationSignerFromDomainResponse)

instance
  Prelude.NFData
    DisassociateDelegationSignerFromDomainResponse
  where
  rnf
    DisassociateDelegationSignerFromDomainResponse' {..} =
      Prelude.rnf operationId
        `Prelude.seq` Prelude.rnf httpStatus
