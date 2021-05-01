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
-- Module      : Network.AWS.Route53Domains.TransferDomainToAnotherAwsAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers a domain from the current AWS account to another AWS account.
-- Note the following:
--
-- -   The AWS account that you\'re transferring the domain to must accept
--     the transfer. If the other account doesn\'t accept the transfer
--     within 3 days, we cancel the transfer. See
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount>.
--
-- -   You can cancel the transfer before the other account accepts it. See
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_CancelDomainTransferToAnotherAwsAccount.html CancelDomainTransferToAnotherAwsAccount>.
--
-- -   The other account can reject the transfer. See
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_RejectDomainTransferFromAnotherAwsAccount.html RejectDomainTransferFromAnotherAwsAccount>.
--
-- When you transfer a domain from one AWS account to another, Route 53
-- doesn\'t transfer the hosted zone that is associated with the domain.
-- DNS resolution isn\'t affected if the domain and the hosted zone are
-- owned by separate accounts, so transferring the hosted zone is optional.
-- For information about transferring the hosted zone to another AWS
-- account, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/hosted-zones-migrating.html Migrating a Hosted Zone to a Different AWS Account>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Use either
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations>
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- to determine whether the operation succeeded.
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- provides additional information, for example,
-- @Domain Transfer from Aws Account 111122223333 has been cancelled@.
module Network.AWS.Route53Domains.TransferDomainToAnotherAwsAccount
  ( -- * Creating a Request
    TransferDomainToAnotherAwsAccount (..),
    newTransferDomainToAnotherAwsAccount,

    -- * Request Lenses
    transferDomainToAnotherAwsAccount_domainName,
    transferDomainToAnotherAwsAccount_accountId,

    -- * Destructuring the Response
    TransferDomainToAnotherAwsAccountResponse (..),
    newTransferDomainToAnotherAwsAccountResponse,

    -- * Response Lenses
    transferDomainToAnotherAwsAccountResponse_operationId,
    transferDomainToAnotherAwsAccountResponse_password,
    transferDomainToAnotherAwsAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The TransferDomainToAnotherAwsAccount request includes the following
-- elements.
--
-- /See:/ 'newTransferDomainToAnotherAwsAccount' smart constructor.
data TransferDomainToAnotherAwsAccount = TransferDomainToAnotherAwsAccount'
  { -- | The name of the domain that you want to transfer from the current AWS
    -- account to another account.
    domainName :: Prelude.Text,
    -- | The account ID of the AWS account that you want to transfer the domain
    -- to, for example, @111122223333@.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransferDomainToAnotherAwsAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'transferDomainToAnotherAwsAccount_domainName' - The name of the domain that you want to transfer from the current AWS
-- account to another account.
--
-- 'accountId', 'transferDomainToAnotherAwsAccount_accountId' - The account ID of the AWS account that you want to transfer the domain
-- to, for example, @111122223333@.
newTransferDomainToAnotherAwsAccount ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  TransferDomainToAnotherAwsAccount
newTransferDomainToAnotherAwsAccount
  pDomainName_
  pAccountId_ =
    TransferDomainToAnotherAwsAccount'
      { domainName =
          pDomainName_,
        accountId = pAccountId_
      }

-- | The name of the domain that you want to transfer from the current AWS
-- account to another account.
transferDomainToAnotherAwsAccount_domainName :: Lens.Lens' TransferDomainToAnotherAwsAccount Prelude.Text
transferDomainToAnotherAwsAccount_domainName = Lens.lens (\TransferDomainToAnotherAwsAccount' {domainName} -> domainName) (\s@TransferDomainToAnotherAwsAccount' {} a -> s {domainName = a} :: TransferDomainToAnotherAwsAccount)

-- | The account ID of the AWS account that you want to transfer the domain
-- to, for example, @111122223333@.
transferDomainToAnotherAwsAccount_accountId :: Lens.Lens' TransferDomainToAnotherAwsAccount Prelude.Text
transferDomainToAnotherAwsAccount_accountId = Lens.lens (\TransferDomainToAnotherAwsAccount' {accountId} -> accountId) (\s@TransferDomainToAnotherAwsAccount' {} a -> s {accountId = a} :: TransferDomainToAnotherAwsAccount)

instance
  Prelude.AWSRequest
    TransferDomainToAnotherAwsAccount
  where
  type
    Rs TransferDomainToAnotherAwsAccount =
      TransferDomainToAnotherAwsAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TransferDomainToAnotherAwsAccountResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
              Prelude.<*> (x Prelude..?> "Password")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    TransferDomainToAnotherAwsAccount

instance
  Prelude.NFData
    TransferDomainToAnotherAwsAccount

instance
  Prelude.ToHeaders
    TransferDomainToAnotherAwsAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.TransferDomainToAnotherAwsAccount" ::
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
    TransferDomainToAnotherAwsAccount
  where
  toJSON TransferDomainToAnotherAwsAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Prelude..= domainName),
            Prelude.Just ("AccountId" Prelude..= accountId)
          ]
      )

instance
  Prelude.ToPath
    TransferDomainToAnotherAwsAccount
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    TransferDomainToAnotherAwsAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | The @TransferDomainToAnotherAwsAccount@ response includes the following
-- elements.
--
-- /See:/ 'newTransferDomainToAnotherAwsAccountResponse' smart constructor.
data TransferDomainToAnotherAwsAccountResponse = TransferDomainToAnotherAwsAccountResponse'
  { -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | To finish transferring a domain to another AWS account, the account that
    -- the domain is being transferred to must submit an
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount>
    -- request. The request must include the value of the @Password@ element
    -- that was returned in the @TransferDomainToAnotherAwsAccount@ response.
    password :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransferDomainToAnotherAwsAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'transferDomainToAnotherAwsAccountResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'password', 'transferDomainToAnotherAwsAccountResponse_password' - To finish transferring a domain to another AWS account, the account that
-- the domain is being transferred to must submit an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount>
-- request. The request must include the value of the @Password@ element
-- that was returned in the @TransferDomainToAnotherAwsAccount@ response.
--
-- 'httpStatus', 'transferDomainToAnotherAwsAccountResponse_httpStatus' - The response's http status code.
newTransferDomainToAnotherAwsAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TransferDomainToAnotherAwsAccountResponse
newTransferDomainToAnotherAwsAccountResponse
  pHttpStatus_ =
    TransferDomainToAnotherAwsAccountResponse'
      { operationId =
          Prelude.Nothing,
        password = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
transferDomainToAnotherAwsAccountResponse_operationId :: Lens.Lens' TransferDomainToAnotherAwsAccountResponse (Prelude.Maybe Prelude.Text)
transferDomainToAnotherAwsAccountResponse_operationId = Lens.lens (\TransferDomainToAnotherAwsAccountResponse' {operationId} -> operationId) (\s@TransferDomainToAnotherAwsAccountResponse' {} a -> s {operationId = a} :: TransferDomainToAnotherAwsAccountResponse)

-- | To finish transferring a domain to another AWS account, the account that
-- the domain is being transferred to must submit an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount>
-- request. The request must include the value of the @Password@ element
-- that was returned in the @TransferDomainToAnotherAwsAccount@ response.
transferDomainToAnotherAwsAccountResponse_password :: Lens.Lens' TransferDomainToAnotherAwsAccountResponse (Prelude.Maybe Prelude.Text)
transferDomainToAnotherAwsAccountResponse_password = Lens.lens (\TransferDomainToAnotherAwsAccountResponse' {password} -> password) (\s@TransferDomainToAnotherAwsAccountResponse' {} a -> s {password = a} :: TransferDomainToAnotherAwsAccountResponse)

-- | The response's http status code.
transferDomainToAnotherAwsAccountResponse_httpStatus :: Lens.Lens' TransferDomainToAnotherAwsAccountResponse Prelude.Int
transferDomainToAnotherAwsAccountResponse_httpStatus = Lens.lens (\TransferDomainToAnotherAwsAccountResponse' {httpStatus} -> httpStatus) (\s@TransferDomainToAnotherAwsAccountResponse' {} a -> s {httpStatus = a} :: TransferDomainToAnotherAwsAccountResponse)

instance
  Prelude.NFData
    TransferDomainToAnotherAwsAccountResponse
