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
-- Module      : Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects the transfer of a domain from another AWS account to the current
-- AWS account. You initiate a transfer between AWS accounts using
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
module Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The RejectDomainTransferFromAnotherAwsAccount request includes the
-- following element.
--
-- /See:/ 'newRejectDomainTransferFromAnotherAwsAccount' smart constructor.
data RejectDomainTransferFromAnotherAwsAccount = RejectDomainTransferFromAnotherAwsAccount'
  { -- | The name of the domain that was specified when another AWS account
    -- submitted a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
    -- request.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RejectDomainTransferFromAnotherAwsAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'rejectDomainTransferFromAnotherAwsAccount_domainName' - The name of the domain that was specified when another AWS account
-- submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
newRejectDomainTransferFromAnotherAwsAccount ::
  -- | 'domainName'
  Core.Text ->
  RejectDomainTransferFromAnotherAwsAccount
newRejectDomainTransferFromAnotherAwsAccount
  pDomainName_ =
    RejectDomainTransferFromAnotherAwsAccount'
      { domainName =
          pDomainName_
      }

-- | The name of the domain that was specified when another AWS account
-- submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
rejectDomainTransferFromAnotherAwsAccount_domainName :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccount Core.Text
rejectDomainTransferFromAnotherAwsAccount_domainName = Lens.lens (\RejectDomainTransferFromAnotherAwsAccount' {domainName} -> domainName) (\s@RejectDomainTransferFromAnotherAwsAccount' {} a -> s {domainName = a} :: RejectDomainTransferFromAnotherAwsAccount)

instance
  Core.AWSRequest
    RejectDomainTransferFromAnotherAwsAccount
  where
  type
    AWSResponse
      RejectDomainTransferFromAnotherAwsAccount =
      RejectDomainTransferFromAnotherAwsAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectDomainTransferFromAnotherAwsAccountResponse'
            Core.<$> (x Core..?> "OperationId")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RejectDomainTransferFromAnotherAwsAccount

instance
  Core.NFData
    RejectDomainTransferFromAnotherAwsAccount

instance
  Core.ToHeaders
    RejectDomainTransferFromAnotherAwsAccount
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.RejectDomainTransferFromAnotherAwsAccount" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    RejectDomainTransferFromAnotherAwsAccount
  where
  toJSON RejectDomainTransferFromAnotherAwsAccount' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DomainName" Core..= domainName)]
      )

instance
  Core.ToPath
    RejectDomainTransferFromAnotherAwsAccount
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    RejectDomainTransferFromAnotherAwsAccount
  where
  toQuery = Core.const Core.mempty

-- | The RejectDomainTransferFromAnotherAwsAccount response includes the
-- following element.
--
-- /See:/ 'newRejectDomainTransferFromAnotherAwsAccountResponse' smart constructor.
data RejectDomainTransferFromAnotherAwsAccountResponse = RejectDomainTransferFromAnotherAwsAccountResponse'
  { -- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to
    -- track the progress of the request. Because the transfer request was
    -- rejected, the value is no longer valid, and you can\'t use
    -- @GetOperationDetail@ to query the operation status.
    operationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RejectDomainTransferFromAnotherAwsAccountResponse
newRejectDomainTransferFromAnotherAwsAccountResponse
  pHttpStatus_ =
    RejectDomainTransferFromAnotherAwsAccountResponse'
      { operationId =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to
-- track the progress of the request. Because the transfer request was
-- rejected, the value is no longer valid, and you can\'t use
-- @GetOperationDetail@ to query the operation status.
rejectDomainTransferFromAnotherAwsAccountResponse_operationId :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccountResponse (Core.Maybe Core.Text)
rejectDomainTransferFromAnotherAwsAccountResponse_operationId = Lens.lens (\RejectDomainTransferFromAnotherAwsAccountResponse' {operationId} -> operationId) (\s@RejectDomainTransferFromAnotherAwsAccountResponse' {} a -> s {operationId = a} :: RejectDomainTransferFromAnotherAwsAccountResponse)

-- | The response's http status code.
rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus :: Lens.Lens' RejectDomainTransferFromAnotherAwsAccountResponse Core.Int
rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus = Lens.lens (\RejectDomainTransferFromAnotherAwsAccountResponse' {httpStatus} -> httpStatus) (\s@RejectDomainTransferFromAnotherAwsAccountResponse' {} a -> s {httpStatus = a} :: RejectDomainTransferFromAnotherAwsAccountResponse)

instance
  Core.NFData
    RejectDomainTransferFromAnotherAwsAccountResponse
