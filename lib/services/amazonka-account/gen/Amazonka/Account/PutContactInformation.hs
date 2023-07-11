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
-- Module      : Amazonka.Account.PutContactInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the primary contact information of an Amazon Web Services
-- account.
--
-- For complete details about how to use the primary contact operations,
-- see
-- <https://docs.aws.amazon.com/accounts/latest/reference/manage-acct-update-contact.html Update the primary and alternate contact information>.
module Amazonka.Account.PutContactInformation
  ( -- * Creating a Request
    PutContactInformation (..),
    newPutContactInformation,

    -- * Request Lenses
    putContactInformation_accountId,
    putContactInformation_contactInformation,

    -- * Destructuring the Response
    PutContactInformationResponse (..),
    newPutContactInformationResponse,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutContactInformation' smart constructor.
data PutContactInformation = PutContactInformation'
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
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Contains the details of the primary contact information associated with
    -- an Amazon Web Services account.
    contactInformation :: ContactInformation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutContactInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'putContactInformation_accountId' - Specifies the 12-digit account ID number of the Amazon Web Services
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
--
-- 'contactInformation', 'putContactInformation_contactInformation' - Contains the details of the primary contact information associated with
-- an Amazon Web Services account.
newPutContactInformation ::
  -- | 'contactInformation'
  ContactInformation ->
  PutContactInformation
newPutContactInformation pContactInformation_ =
  PutContactInformation'
    { accountId = Prelude.Nothing,
      contactInformation = pContactInformation_
    }

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
putContactInformation_accountId :: Lens.Lens' PutContactInformation (Prelude.Maybe Prelude.Text)
putContactInformation_accountId = Lens.lens (\PutContactInformation' {accountId} -> accountId) (\s@PutContactInformation' {} a -> s {accountId = a} :: PutContactInformation)

-- | Contains the details of the primary contact information associated with
-- an Amazon Web Services account.
putContactInformation_contactInformation :: Lens.Lens' PutContactInformation ContactInformation
putContactInformation_contactInformation = Lens.lens (\PutContactInformation' {contactInformation} -> contactInformation) (\s@PutContactInformation' {} a -> s {contactInformation = a} :: PutContactInformation)

instance Core.AWSRequest PutContactInformation where
  type
    AWSResponse PutContactInformation =
      PutContactInformationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutContactInformationResponse'

instance Prelude.Hashable PutContactInformation where
  hashWithSalt _salt PutContactInformation' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` contactInformation

instance Prelude.NFData PutContactInformation where
  rnf PutContactInformation' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf contactInformation

instance Data.ToHeaders PutContactInformation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutContactInformation where
  toJSON PutContactInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            Prelude.Just
              ("ContactInformation" Data..= contactInformation)
          ]
      )

instance Data.ToPath PutContactInformation where
  toPath = Prelude.const "/putContactInformation"

instance Data.ToQuery PutContactInformation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutContactInformationResponse' smart constructor.
data PutContactInformationResponse = PutContactInformationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutContactInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutContactInformationResponse ::
  PutContactInformationResponse
newPutContactInformationResponse =
  PutContactInformationResponse'

instance Prelude.NFData PutContactInformationResponse where
  rnf _ = ()
