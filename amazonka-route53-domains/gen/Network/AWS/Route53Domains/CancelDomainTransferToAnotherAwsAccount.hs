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
-- Module      : Network.AWS.Route53Domains.CancelDomainTransferToAnotherAwsAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the transfer of a domain from the current AWS account to another
-- AWS account. You initiate a transfer between AWS accounts using
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>.
--
-- You must cancel the transfer before the other AWS account accepts the
-- transfer using
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount>.
--
-- Use either
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations>
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- to determine whether the operation succeeded.
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- provides additional information, for example,
-- @Domain Transfer from Aws Account 111122223333 has been cancelled@.
module Network.AWS.Route53Domains.CancelDomainTransferToAnotherAwsAccount
  ( -- * Creating a Request
    CancelDomainTransferToAnotherAwsAccount (..),
    newCancelDomainTransferToAnotherAwsAccount,

    -- * Request Lenses
    cancelDomainTransferToAnotherAwsAccount_domainName,

    -- * Destructuring the Response
    CancelDomainTransferToAnotherAwsAccountResponse (..),
    newCancelDomainTransferToAnotherAwsAccountResponse,

    -- * Response Lenses
    cancelDomainTransferToAnotherAwsAccountResponse_operationId,
    cancelDomainTransferToAnotherAwsAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The CancelDomainTransferToAnotherAwsAccount request includes the
-- following element.
--
-- /See:/ 'newCancelDomainTransferToAnotherAwsAccount' smart constructor.
data CancelDomainTransferToAnotherAwsAccount = CancelDomainTransferToAnotherAwsAccount'
  { -- | The name of the domain for which you want to cancel the transfer to
    -- another AWS account.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelDomainTransferToAnotherAwsAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'cancelDomainTransferToAnotherAwsAccount_domainName' - The name of the domain for which you want to cancel the transfer to
-- another AWS account.
newCancelDomainTransferToAnotherAwsAccount ::
  -- | 'domainName'
  Prelude.Text ->
  CancelDomainTransferToAnotherAwsAccount
newCancelDomainTransferToAnotherAwsAccount
  pDomainName_ =
    CancelDomainTransferToAnotherAwsAccount'
      { domainName =
          pDomainName_
      }

-- | The name of the domain for which you want to cancel the transfer to
-- another AWS account.
cancelDomainTransferToAnotherAwsAccount_domainName :: Lens.Lens' CancelDomainTransferToAnotherAwsAccount Prelude.Text
cancelDomainTransferToAnotherAwsAccount_domainName = Lens.lens (\CancelDomainTransferToAnotherAwsAccount' {domainName} -> domainName) (\s@CancelDomainTransferToAnotherAwsAccount' {} a -> s {domainName = a} :: CancelDomainTransferToAnotherAwsAccount)

instance
  Prelude.AWSRequest
    CancelDomainTransferToAnotherAwsAccount
  where
  type
    Rs CancelDomainTransferToAnotherAwsAccount =
      CancelDomainTransferToAnotherAwsAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelDomainTransferToAnotherAwsAccountResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelDomainTransferToAnotherAwsAccount

instance
  Prelude.NFData
    CancelDomainTransferToAnotherAwsAccount

instance
  Prelude.ToHeaders
    CancelDomainTransferToAnotherAwsAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.CancelDomainTransferToAnotherAwsAccount" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    CancelDomainTransferToAnotherAwsAccount
  where
  toJSON CancelDomainTransferToAnotherAwsAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Prelude..= domainName)]
      )

instance
  Prelude.ToPath
    CancelDomainTransferToAnotherAwsAccount
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CancelDomainTransferToAnotherAwsAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | The @CancelDomainTransferToAnotherAwsAccount@ response includes the
-- following element.
--
-- /See:/ 'newCancelDomainTransferToAnotherAwsAccountResponse' smart constructor.
data CancelDomainTransferToAnotherAwsAccountResponse = CancelDomainTransferToAnotherAwsAccountResponse'
  { -- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to
    -- track the progress of the request. Because the transfer request was
    -- canceled, the value is no longer valid, and you can\'t use
    -- @GetOperationDetail@ to query the operation status.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelDomainTransferToAnotherAwsAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'cancelDomainTransferToAnotherAwsAccountResponse_operationId' - The identifier that @TransferDomainToAnotherAwsAccount@ returned to
-- track the progress of the request. Because the transfer request was
-- canceled, the value is no longer valid, and you can\'t use
-- @GetOperationDetail@ to query the operation status.
--
-- 'httpStatus', 'cancelDomainTransferToAnotherAwsAccountResponse_httpStatus' - The response's http status code.
newCancelDomainTransferToAnotherAwsAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelDomainTransferToAnotherAwsAccountResponse
newCancelDomainTransferToAnotherAwsAccountResponse
  pHttpStatus_ =
    CancelDomainTransferToAnotherAwsAccountResponse'
      { operationId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to
-- track the progress of the request. Because the transfer request was
-- canceled, the value is no longer valid, and you can\'t use
-- @GetOperationDetail@ to query the operation status.
cancelDomainTransferToAnotherAwsAccountResponse_operationId :: Lens.Lens' CancelDomainTransferToAnotherAwsAccountResponse (Prelude.Maybe Prelude.Text)
cancelDomainTransferToAnotherAwsAccountResponse_operationId = Lens.lens (\CancelDomainTransferToAnotherAwsAccountResponse' {operationId} -> operationId) (\s@CancelDomainTransferToAnotherAwsAccountResponse' {} a -> s {operationId = a} :: CancelDomainTransferToAnotherAwsAccountResponse)

-- | The response's http status code.
cancelDomainTransferToAnotherAwsAccountResponse_httpStatus :: Lens.Lens' CancelDomainTransferToAnotherAwsAccountResponse Prelude.Int
cancelDomainTransferToAnotherAwsAccountResponse_httpStatus = Lens.lens (\CancelDomainTransferToAnotherAwsAccountResponse' {httpStatus} -> httpStatus) (\s@CancelDomainTransferToAnotherAwsAccountResponse' {} a -> s {httpStatus = a} :: CancelDomainTransferToAnotherAwsAccountResponse)

instance
  Prelude.NFData
    CancelDomainTransferToAnotherAwsAccountResponse
