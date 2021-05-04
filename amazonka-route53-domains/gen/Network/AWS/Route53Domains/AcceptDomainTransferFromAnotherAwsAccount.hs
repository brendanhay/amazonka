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
-- Module      : Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the transfer of a domain from another AWS account to the current
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
module Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
  ( -- * Creating a Request
    AcceptDomainTransferFromAnotherAwsAccount (..),
    newAcceptDomainTransferFromAnotherAwsAccount,

    -- * Request Lenses
    acceptDomainTransferFromAnotherAwsAccount_domainName,
    acceptDomainTransferFromAnotherAwsAccount_password,

    -- * Destructuring the Response
    AcceptDomainTransferFromAnotherAwsAccountResponse (..),
    newAcceptDomainTransferFromAnotherAwsAccountResponse,

    -- * Response Lenses
    acceptDomainTransferFromAnotherAwsAccountResponse_operationId,
    acceptDomainTransferFromAnotherAwsAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The AcceptDomainTransferFromAnotherAwsAccount request includes the
-- following elements.
--
-- /See:/ 'newAcceptDomainTransferFromAnotherAwsAccount' smart constructor.
data AcceptDomainTransferFromAnotherAwsAccount = AcceptDomainTransferFromAnotherAwsAccount'
  { -- | The name of the domain that was specified when another AWS account
    -- submitted a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
    -- request.
    domainName :: Prelude.Text,
    -- | The password that was returned by the
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
    -- request.
    password :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AcceptDomainTransferFromAnotherAwsAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'acceptDomainTransferFromAnotherAwsAccount_domainName' - The name of the domain that was specified when another AWS account
-- submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
--
-- 'password', 'acceptDomainTransferFromAnotherAwsAccount_password' - The password that was returned by the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
newAcceptDomainTransferFromAnotherAwsAccount ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  AcceptDomainTransferFromAnotherAwsAccount
newAcceptDomainTransferFromAnotherAwsAccount
  pDomainName_
  pPassword_ =
    AcceptDomainTransferFromAnotherAwsAccount'
      { domainName =
          pDomainName_,
        password = pPassword_
      }

-- | The name of the domain that was specified when another AWS account
-- submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
acceptDomainTransferFromAnotherAwsAccount_domainName :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccount Prelude.Text
acceptDomainTransferFromAnotherAwsAccount_domainName = Lens.lens (\AcceptDomainTransferFromAnotherAwsAccount' {domainName} -> domainName) (\s@AcceptDomainTransferFromAnotherAwsAccount' {} a -> s {domainName = a} :: AcceptDomainTransferFromAnotherAwsAccount)

-- | The password that was returned by the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
-- request.
acceptDomainTransferFromAnotherAwsAccount_password :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccount Prelude.Text
acceptDomainTransferFromAnotherAwsAccount_password = Lens.lens (\AcceptDomainTransferFromAnotherAwsAccount' {password} -> password) (\s@AcceptDomainTransferFromAnotherAwsAccount' {} a -> s {password = a} :: AcceptDomainTransferFromAnotherAwsAccount)

instance
  Prelude.AWSRequest
    AcceptDomainTransferFromAnotherAwsAccount
  where
  type
    Rs AcceptDomainTransferFromAnotherAwsAccount =
      AcceptDomainTransferFromAnotherAwsAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptDomainTransferFromAnotherAwsAccountResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptDomainTransferFromAnotherAwsAccount

instance
  Prelude.NFData
    AcceptDomainTransferFromAnotherAwsAccount

instance
  Prelude.ToHeaders
    AcceptDomainTransferFromAnotherAwsAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.AcceptDomainTransferFromAnotherAwsAccount" ::
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
    AcceptDomainTransferFromAnotherAwsAccount
  where
  toJSON AcceptDomainTransferFromAnotherAwsAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Prelude..= domainName),
            Prelude.Just ("Password" Prelude..= password)
          ]
      )

instance
  Prelude.ToPath
    AcceptDomainTransferFromAnotherAwsAccount
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AcceptDomainTransferFromAnotherAwsAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | The AcceptDomainTransferFromAnotherAwsAccount response includes the
-- following element.
--
-- /See:/ 'newAcceptDomainTransferFromAnotherAwsAccountResponse' smart constructor.
data AcceptDomainTransferFromAnotherAwsAccountResponse = AcceptDomainTransferFromAnotherAwsAccountResponse'
  { -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AcceptDomainTransferFromAnotherAwsAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'acceptDomainTransferFromAnotherAwsAccountResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'httpStatus', 'acceptDomainTransferFromAnotherAwsAccountResponse_httpStatus' - The response's http status code.
newAcceptDomainTransferFromAnotherAwsAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptDomainTransferFromAnotherAwsAccountResponse
newAcceptDomainTransferFromAnotherAwsAccountResponse
  pHttpStatus_ =
    AcceptDomainTransferFromAnotherAwsAccountResponse'
      { operationId =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
acceptDomainTransferFromAnotherAwsAccountResponse_operationId :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccountResponse (Prelude.Maybe Prelude.Text)
acceptDomainTransferFromAnotherAwsAccountResponse_operationId = Lens.lens (\AcceptDomainTransferFromAnotherAwsAccountResponse' {operationId} -> operationId) (\s@AcceptDomainTransferFromAnotherAwsAccountResponse' {} a -> s {operationId = a} :: AcceptDomainTransferFromAnotherAwsAccountResponse)

-- | The response's http status code.
acceptDomainTransferFromAnotherAwsAccountResponse_httpStatus :: Lens.Lens' AcceptDomainTransferFromAnotherAwsAccountResponse Prelude.Int
acceptDomainTransferFromAnotherAwsAccountResponse_httpStatus = Lens.lens (\AcceptDomainTransferFromAnotherAwsAccountResponse' {httpStatus} -> httpStatus) (\s@AcceptDomainTransferFromAnotherAwsAccountResponse' {} a -> s {httpStatus = a} :: AcceptDomainTransferFromAnotherAwsAccountResponse)

instance
  Prelude.NFData
    AcceptDomainTransferFromAnotherAwsAccountResponse
