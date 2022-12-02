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
-- Module      : Amazonka.Account.GetContactInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the primary contact information of an Amazon Web Services
-- account.
--
-- For complete details about how to use the primary contact operations,
-- see
-- <https://docs.aws.amazon.com/accounts/latest/reference/manage-acct-update-contact.html Update the primary and alternate contact information>.
module Amazonka.Account.GetContactInformation
  ( -- * Creating a Request
    GetContactInformation (..),
    newGetContactInformation,

    -- * Request Lenses
    getContactInformation_accountId,

    -- * Destructuring the Response
    GetContactInformationResponse (..),
    newGetContactInformationResponse,

    -- * Response Lenses
    getContactInformationResponse_contactInformation,
    getContactInformationResponse_httpStatus,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContactInformation' smart constructor.
data GetContactInformation = GetContactInformation'
  { -- | Specifies the 12-digit account ID number of the Amazon Web Services
    -- account that you want to access or modify with this operation. If you
    -- don\'t specify this parameter, it defaults to the Amazon Web Services
    -- account of the identity used to call the operation. To use this
    -- parameter, the caller must be an identity in the
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
    -- or a delegated administrator account. The specified account ID must also
    -- be a member account in the same organization. The organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
    -- and the organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
    -- enabled for the Account Management service, and optionally a
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
    -- account assigned.
    --
    -- The management account can\'t specify its own @AccountId@. It must call
    -- the operation in standalone context by not including the @AccountId@
    -- parameter.
    --
    -- To call this operation on an account that is not a member of an
    -- organization, don\'t specify this parameter. Instead, call the operation
    -- using an identity belonging to the account whose contacts you wish to
    -- retrieve or modify.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getContactInformation_accountId' - Specifies the 12-digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation. If you
-- don\'t specify this parameter, it defaults to the Amazon Web Services
-- account of the identity used to call the operation. To use this
-- parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account. The specified account ID must also
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@. It must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, don\'t specify this parameter. Instead, call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
newGetContactInformation ::
  GetContactInformation
newGetContactInformation =
  GetContactInformation' {accountId = Prelude.Nothing}

-- | Specifies the 12-digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation. If you
-- don\'t specify this parameter, it defaults to the Amazon Web Services
-- account of the identity used to call the operation. To use this
-- parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account. The specified account ID must also
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@. It must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, don\'t specify this parameter. Instead, call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
getContactInformation_accountId :: Lens.Lens' GetContactInformation (Prelude.Maybe Prelude.Text)
getContactInformation_accountId = Lens.lens (\GetContactInformation' {accountId} -> accountId) (\s@GetContactInformation' {} a -> s {accountId = a} :: GetContactInformation)

instance Core.AWSRequest GetContactInformation where
  type
    AWSResponse GetContactInformation =
      GetContactInformationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactInformationResponse'
            Prelude.<$> (x Data..?> "ContactInformation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContactInformation where
  hashWithSalt _salt GetContactInformation' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData GetContactInformation where
  rnf GetContactInformation' {..} =
    Prelude.rnf accountId

instance Data.ToHeaders GetContactInformation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContactInformation where
  toJSON GetContactInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [("AccountId" Data..=) Prelude.<$> accountId]
      )

instance Data.ToPath GetContactInformation where
  toPath = Prelude.const "/getContactInformation"

instance Data.ToQuery GetContactInformation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactInformationResponse' smart constructor.
data GetContactInformationResponse = GetContactInformationResponse'
  { -- | Contains the details of the primary contact information associated with
    -- an Amazon Web Services account.
    contactInformation :: Prelude.Maybe ContactInformation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactInformation', 'getContactInformationResponse_contactInformation' - Contains the details of the primary contact information associated with
-- an Amazon Web Services account.
--
-- 'httpStatus', 'getContactInformationResponse_httpStatus' - The response's http status code.
newGetContactInformationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactInformationResponse
newGetContactInformationResponse pHttpStatus_ =
  GetContactInformationResponse'
    { contactInformation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the primary contact information associated with
-- an Amazon Web Services account.
getContactInformationResponse_contactInformation :: Lens.Lens' GetContactInformationResponse (Prelude.Maybe ContactInformation)
getContactInformationResponse_contactInformation = Lens.lens (\GetContactInformationResponse' {contactInformation} -> contactInformation) (\s@GetContactInformationResponse' {} a -> s {contactInformation = a} :: GetContactInformationResponse)

-- | The response's http status code.
getContactInformationResponse_httpStatus :: Lens.Lens' GetContactInformationResponse Prelude.Int
getContactInformationResponse_httpStatus = Lens.lens (\GetContactInformationResponse' {httpStatus} -> httpStatus) (\s@GetContactInformationResponse' {} a -> s {httpStatus = a} :: GetContactInformationResponse)

instance Prelude.NFData GetContactInformationResponse where
  rnf GetContactInformationResponse' {..} =
    Prelude.rnf contactInformation
      `Prelude.seq` Prelude.rnf httpStatus
