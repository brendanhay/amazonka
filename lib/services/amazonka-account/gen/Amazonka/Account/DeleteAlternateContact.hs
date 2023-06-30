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
-- Module      : Amazonka.Account.DeleteAlternateContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alternate contact from an Amazon Web Services
-- account.
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
module Amazonka.Account.DeleteAlternateContact
  ( -- * Creating a Request
    DeleteAlternateContact (..),
    newDeleteAlternateContact,

    -- * Request Lenses
    deleteAlternateContact_accountId,
    deleteAlternateContact_alternateContactType,

    -- * Destructuring the Response
    DeleteAlternateContactResponse (..),
    newDeleteAlternateContactResponse,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAlternateContact' smart constructor.
data DeleteAlternateContact = DeleteAlternateContact'
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
    -- | Specifies which of the alternate contacts to delete.
    alternateContactType :: AlternateContactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlternateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteAlternateContact_accountId' - Specifies the 12 digit account ID number of the Amazon Web Services
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
-- 'alternateContactType', 'deleteAlternateContact_alternateContactType' - Specifies which of the alternate contacts to delete.
newDeleteAlternateContact ::
  -- | 'alternateContactType'
  AlternateContactType ->
  DeleteAlternateContact
newDeleteAlternateContact pAlternateContactType_ =
  DeleteAlternateContact'
    { accountId =
        Prelude.Nothing,
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
deleteAlternateContact_accountId :: Lens.Lens' DeleteAlternateContact (Prelude.Maybe Prelude.Text)
deleteAlternateContact_accountId = Lens.lens (\DeleteAlternateContact' {accountId} -> accountId) (\s@DeleteAlternateContact' {} a -> s {accountId = a} :: DeleteAlternateContact)

-- | Specifies which of the alternate contacts to delete.
deleteAlternateContact_alternateContactType :: Lens.Lens' DeleteAlternateContact AlternateContactType
deleteAlternateContact_alternateContactType = Lens.lens (\DeleteAlternateContact' {alternateContactType} -> alternateContactType) (\s@DeleteAlternateContact' {} a -> s {alternateContactType = a} :: DeleteAlternateContact)

instance Core.AWSRequest DeleteAlternateContact where
  type
    AWSResponse DeleteAlternateContact =
      DeleteAlternateContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAlternateContactResponse'

instance Prelude.Hashable DeleteAlternateContact where
  hashWithSalt _salt DeleteAlternateContact' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` alternateContactType

instance Prelude.NFData DeleteAlternateContact where
  rnf DeleteAlternateContact' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf alternateContactType

instance Data.ToHeaders DeleteAlternateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAlternateContact where
  toJSON DeleteAlternateContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            Prelude.Just
              ( "AlternateContactType"
                  Data..= alternateContactType
              )
          ]
      )

instance Data.ToPath DeleteAlternateContact where
  toPath = Prelude.const "/deleteAlternateContact"

instance Data.ToQuery DeleteAlternateContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAlternateContactResponse' smart constructor.
data DeleteAlternateContactResponse = DeleteAlternateContactResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlternateContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAlternateContactResponse ::
  DeleteAlternateContactResponse
newDeleteAlternateContactResponse =
  DeleteAlternateContactResponse'

instance
  Prelude.NFData
    DeleteAlternateContactResponse
  where
  rnf _ = ()
