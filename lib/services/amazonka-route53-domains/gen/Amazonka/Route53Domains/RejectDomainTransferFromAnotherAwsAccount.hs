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
-- Module      : Amazonka.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects the transfer of a domain from another Amazon Web Services
-- account to the current Amazon Web Services account. You initiate a
-- transfer betweenAmazon Web Services accounts using
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>.
--
-- Use either
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations>
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- to determine whether the operation succeeded.
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- provides additional information, for example,
-- @Domain Transfer from Aws Account 111122223333 has been cancelled@.
module Amazonka.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
  ( -- * Creating a Request
    RejectDomainTransferFromAnotherAwsAccount (..),
    newRejectDomainTransferFromAnotherAwsAccount,

    -- * Request Lenses
    rejectDomainTransferFromAnotherAwsAccount_domainName,

    -- * Destructuring the Response
    RejectDomainTransferFromAnotherAwsAccountResponse (..),
    newRejectDomainTransferFromAnotherAwsAccountResponse,

    -- * Response Lenses
    rejectDomainTransferFromAnotherAwsAccountResponse_operationId,
    rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The RejectDomainTransferFromAnotherAwsAccount request includes the
-- following element.
--
-- /See:/ 'newRejectDomainTransferFromAnotherAwsAccount' smart constructor.
data RejectDomainTransferFromAnotherAwsAccount = RejectDomainTransferFromAnotherAwsAccount'
  { -- | The name of the domain that was specified when another Amazon Web
    -- Services account submitted a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
    -- request.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectDomainTransferFromAnotherAwsAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'rejectDomainTransferFromAnotherAwsAccount_domainName' - The name of the domain that was specified when another Amazon Web
-- Services account submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
newRejectDomainTransferFromAnotherAwsAccount ::
  -- | 'domainName'
  Prelude.Text ->
  RejectDomainTransferFromAnotherAwsAccount
newRejectDomainTransferFromAnotherAwsAccount
  pDomainName_ =
    RejectDomainTransferFromAnotherAwsAccount'
      { domainName =
          pDomainName_
      }

-- | The name of the domain that was specified when another Amazon Web
-- Services account submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
rejectDomainTransferFromAnotherAwsAccount_domainName :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccount Prelude.Text
rejectDomainTransferFromAnotherAwsAccount_domainName = Lens.lens (\RejectDomainTransferFromAnotherAwsAccount' {domainName} -> domainName) (\s@RejectDomainTransferFromAnotherAwsAccount' {} a -> s {domainName = a} :: RejectDomainTransferFromAnotherAwsAccount)

instance
  Core.AWSRequest
    RejectDomainTransferFromAnotherAwsAccount
  where
  type
    AWSResponse
      RejectDomainTransferFromAnotherAwsAccount =
      RejectDomainTransferFromAnotherAwsAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectDomainTransferFromAnotherAwsAccountResponse'
            Prelude.<$> (x Data..?> "OperationId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectDomainTransferFromAnotherAwsAccount
  where
  hashWithSalt
    _salt
    RejectDomainTransferFromAnotherAwsAccount' {..} =
      _salt `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    RejectDomainTransferFromAnotherAwsAccount
  where
  rnf RejectDomainTransferFromAnotherAwsAccount' {..} =
    Prelude.rnf domainName

instance
  Data.ToHeaders
    RejectDomainTransferFromAnotherAwsAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.RejectDomainTransferFromAnotherAwsAccount" ::
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
    RejectDomainTransferFromAnotherAwsAccount
  where
  toJSON RejectDomainTransferFromAnotherAwsAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance
  Data.ToPath
    RejectDomainTransferFromAnotherAwsAccount
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RejectDomainTransferFromAnotherAwsAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | The RejectDomainTransferFromAnotherAwsAccount response includes the
-- following element.
--
-- /See:/ 'newRejectDomainTransferFromAnotherAwsAccountResponse' smart constructor.
data RejectDomainTransferFromAnotherAwsAccountResponse = RejectDomainTransferFromAnotherAwsAccountResponse'
  { -- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to
    -- track the progress of the request. Because the transfer request was
    -- rejected, the value is no longer valid, and you can\'t use
    -- @GetOperationDetail@ to query the operation status.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectDomainTransferFromAnotherAwsAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'rejectDomainTransferFromAnotherAwsAccountResponse_operationId' - The identifier that @TransferDomainToAnotherAwsAccount@ returned to
-- track the progress of the request. Because the transfer request was
-- rejected, the value is no longer valid, and you can\'t use
-- @GetOperationDetail@ to query the operation status.
--
-- 'httpStatus', 'rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus' - The response's http status code.
newRejectDomainTransferFromAnotherAwsAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectDomainTransferFromAnotherAwsAccountResponse
newRejectDomainTransferFromAnotherAwsAccountResponse
  pHttpStatus_ =
    RejectDomainTransferFromAnotherAwsAccountResponse'
      { operationId =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to
-- track the progress of the request. Because the transfer request was
-- rejected, the value is no longer valid, and you can\'t use
-- @GetOperationDetail@ to query the operation status.
rejectDomainTransferFromAnotherAwsAccountResponse_operationId :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccountResponse (Prelude.Maybe Prelude.Text)
rejectDomainTransferFromAnotherAwsAccountResponse_operationId = Lens.lens (\RejectDomainTransferFromAnotherAwsAccountResponse' {operationId} -> operationId) (\s@RejectDomainTransferFromAnotherAwsAccountResponse' {} a -> s {operationId = a} :: RejectDomainTransferFromAnotherAwsAccountResponse)

-- | The response's http status code.
rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccountResponse Prelude.Int
rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus = Lens.lens (\RejectDomainTransferFromAnotherAwsAccountResponse' {httpStatus} -> httpStatus) (\s@RejectDomainTransferFromAnotherAwsAccountResponse' {} a -> s {httpStatus = a} :: RejectDomainTransferFromAnotherAwsAccountResponse)

instance
  Prelude.NFData
    RejectDomainTransferFromAnotherAwsAccountResponse
  where
  rnf
    RejectDomainTransferFromAnotherAwsAccountResponse' {..} =
      Prelude.rnf operationId
        `Prelude.seq` Prelude.rnf httpStatus
