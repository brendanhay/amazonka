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
-- Module      : Amazonka.Account.GetAlternateContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified alternate contact attached to an Amazon Web
-- Services account.
--
-- For complete details about how to use the alternate contact operations,
-- see
-- <https://docs.aws.amazon.com/accounts/latest/reference/manage-acct-update-contact.html Access or updating the alternate contacts>.
--
-- Before you can update the alternate contact information for an Amazon
-- Web Services account that is managed by Organizations, you must first
-- enable integration between Amazon Web Services Account Management and
-- Organizations. For more information, see
-- <https://docs.aws.amazon.com/accounts/latest/reference/using-orgs-trusted-access.html Enabling trusted access for Amazon Web Services Account Management>.
module Amazonka.Account.GetAlternateContact
  ( -- * Creating a Request
    GetAlternateContact (..),
    newGetAlternateContact,

    -- * Request Lenses
    getAlternateContact_accountId,
    getAlternateContact_alternateContactType,

    -- * Destructuring the Response
    GetAlternateContactResponse (..),
    newGetAlternateContactResponse,

    -- * Response Lenses
    getAlternateContactResponse_alternateContact,
    getAlternateContactResponse_httpStatus,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAlternateContact' smart constructor.
data GetAlternateContact = GetAlternateContact'
  { -- | Specifies the 12 digit account ID number of the Amazon Web Services
    -- account that you want to access or modify with this operation.
    --
    -- If you do not specify this parameter, it defaults to the Amazon Web
    -- Services account of the identity used to call the operation.
    --
    -- To use this parameter, the caller must be an identity in the
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
    -- or a delegated administrator account, and the specified account ID must
    -- be a member account in the same organization. The organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
    -- and the organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
    -- enabled for the Account Management service, and optionally a
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
    -- account assigned.
    --
    -- The management account can\'t specify its own @AccountId@; it must call
    -- the operation in standalone context by not including the @AccountId@
    -- parameter.
    --
    -- To call this operation on an account that is not a member of an
    -- organization, then don\'t specify this parameter, and call the operation
    -- using an identity belonging to the account whose contacts you wish to
    -- retrieve or modify.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies which alternate contact you want to retrieve.
    alternateContactType :: AlternateContactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAlternateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getAlternateContact_accountId' - Specifies the 12 digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation.
--
-- If you do not specify this parameter, it defaults to the Amazon Web
-- Services account of the identity used to call the operation.
--
-- To use this parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account, and the specified account ID must
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@; it must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, then don\'t specify this parameter, and call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
--
-- 'alternateContactType', 'getAlternateContact_alternateContactType' - Specifies which alternate contact you want to retrieve.
newGetAlternateContact ::
  -- | 'alternateContactType'
  AlternateContactType ->
  GetAlternateContact
newGetAlternateContact pAlternateContactType_ =
  GetAlternateContact'
    { accountId = Prelude.Nothing,
      alternateContactType = pAlternateContactType_
    }

-- | Specifies the 12 digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation.
--
-- If you do not specify this parameter, it defaults to the Amazon Web
-- Services account of the identity used to call the operation.
--
-- To use this parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account, and the specified account ID must
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@; it must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, then don\'t specify this parameter, and call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
getAlternateContact_accountId :: Lens.Lens' GetAlternateContact (Prelude.Maybe Prelude.Text)
getAlternateContact_accountId = Lens.lens (\GetAlternateContact' {accountId} -> accountId) (\s@GetAlternateContact' {} a -> s {accountId = a} :: GetAlternateContact)

-- | Specifies which alternate contact you want to retrieve.
getAlternateContact_alternateContactType :: Lens.Lens' GetAlternateContact AlternateContactType
getAlternateContact_alternateContactType = Lens.lens (\GetAlternateContact' {alternateContactType} -> alternateContactType) (\s@GetAlternateContact' {} a -> s {alternateContactType = a} :: GetAlternateContact)

instance Core.AWSRequest GetAlternateContact where
  type
    AWSResponse GetAlternateContact =
      GetAlternateContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAlternateContactResponse'
            Prelude.<$> (x Data..?> "AlternateContact")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAlternateContact where
  hashWithSalt _salt GetAlternateContact' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` alternateContactType

instance Prelude.NFData GetAlternateContact where
  rnf GetAlternateContact' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf alternateContactType

instance Data.ToHeaders GetAlternateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAlternateContact where
  toJSON GetAlternateContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            Prelude.Just
              ( "AlternateContactType"
                  Data..= alternateContactType
              )
          ]
      )

instance Data.ToPath GetAlternateContact where
  toPath = Prelude.const "/getAlternateContact"

instance Data.ToQuery GetAlternateContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAlternateContactResponse' smart constructor.
data GetAlternateContactResponse = GetAlternateContactResponse'
  { -- | A structure that contains the details for the specified alternate
    -- contact.
    alternateContact :: Prelude.Maybe AlternateContact,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAlternateContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alternateContact', 'getAlternateContactResponse_alternateContact' - A structure that contains the details for the specified alternate
-- contact.
--
-- 'httpStatus', 'getAlternateContactResponse_httpStatus' - The response's http status code.
newGetAlternateContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAlternateContactResponse
newGetAlternateContactResponse pHttpStatus_ =
  GetAlternateContactResponse'
    { alternateContact =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains the details for the specified alternate
-- contact.
getAlternateContactResponse_alternateContact :: Lens.Lens' GetAlternateContactResponse (Prelude.Maybe AlternateContact)
getAlternateContactResponse_alternateContact = Lens.lens (\GetAlternateContactResponse' {alternateContact} -> alternateContact) (\s@GetAlternateContactResponse' {} a -> s {alternateContact = a} :: GetAlternateContactResponse)

-- | The response's http status code.
getAlternateContactResponse_httpStatus :: Lens.Lens' GetAlternateContactResponse Prelude.Int
getAlternateContactResponse_httpStatus = Lens.lens (\GetAlternateContactResponse' {httpStatus} -> httpStatus) (\s@GetAlternateContactResponse' {} a -> s {httpStatus = a} :: GetAlternateContactResponse)

instance Prelude.NFData GetAlternateContactResponse where
  rnf GetAlternateContactResponse' {..} =
    Prelude.rnf alternateContact `Prelude.seq`
      Prelude.rnf httpStatus
