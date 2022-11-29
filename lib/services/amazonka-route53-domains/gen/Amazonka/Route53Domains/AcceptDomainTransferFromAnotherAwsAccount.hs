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
-- Module      : Amazonka.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the transfer of a domain from another Amazon Web Services
-- account to the currentAmazon Web Services account. You initiate a
-- transfer between Amazon Web Services accounts using
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>.
--
-- If you use the CLI command at
-- <https://docs.aws.amazon.com/cli/latest/reference/route53domains/accept-domain-transfer-from-another-aws-account.html accept-domain-transfer-from-another-aws-account>,
-- use JSON format as input instead of text because otherwise CLI will
-- throw an error from domain transfer input that includes single quotes.
--
-- Use either
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations>
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- to determine whether the operation succeeded.
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- provides additional information, for example,
-- @Domain Transfer from Aws Account 111122223333 has been cancelled@.
module Amazonka.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The AcceptDomainTransferFromAnotherAwsAccount request includes the
-- following elements.
--
-- /See:/ 'newAcceptDomainTransferFromAnotherAwsAccount' smart constructor.
data AcceptDomainTransferFromAnotherAwsAccount = AcceptDomainTransferFromAnotherAwsAccount'
  { -- | The name of the domain that was specified when another Amazon Web
    -- Services account submitted a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
    -- request.
    domainName :: Prelude.Text,
    -- | The password that was returned by the
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount>
    -- request.
    password :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptDomainTransferFromAnotherAwsAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'acceptDomainTransferFromAnotherAwsAccount_domainName' - The name of the domain that was specified when another Amazon Web
-- Services account submitted a
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

-- | The name of the domain that was specified when another Amazon Web
-- Services account submitted a
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
  Core.AWSRequest
    AcceptDomainTransferFromAnotherAwsAccount
  where
  type
    AWSResponse
      AcceptDomainTransferFromAnotherAwsAccount =
      AcceptDomainTransferFromAnotherAwsAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptDomainTransferFromAnotherAwsAccountResponse'
            Prelude.<$> (x Core..?> "OperationId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptDomainTransferFromAnotherAwsAccount
  where
  hashWithSalt
    _salt
    AcceptDomainTransferFromAnotherAwsAccount' {..} =
      _salt `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` password

instance
  Prelude.NFData
    AcceptDomainTransferFromAnotherAwsAccount
  where
  rnf AcceptDomainTransferFromAnotherAwsAccount' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf password

instance
  Core.ToHeaders
    AcceptDomainTransferFromAnotherAwsAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.AcceptDomainTransferFromAnotherAwsAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    AcceptDomainTransferFromAnotherAwsAccount
  where
  toJSON AcceptDomainTransferFromAnotherAwsAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just ("Password" Core..= password)
          ]
      )

instance
  Core.ToPath
    AcceptDomainTransferFromAnotherAwsAccount
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf
    AcceptDomainTransferFromAnotherAwsAccountResponse' {..} =
      Prelude.rnf operationId
        `Prelude.seq` Prelude.rnf httpStatus
