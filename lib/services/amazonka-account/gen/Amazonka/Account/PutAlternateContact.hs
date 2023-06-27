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
-- Module      : Amazonka.Account.PutAlternateContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified alternate contact attached to an Amazon Web
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
module Amazonka.Account.PutAlternateContact
  ( -- * Creating a Request
    PutAlternateContact (..),
    newPutAlternateContact,

    -- * Request Lenses
    putAlternateContact_accountId,
    putAlternateContact_alternateContactType,
    putAlternateContact_emailAddress,
    putAlternateContact_name,
    putAlternateContact_phoneNumber,
    putAlternateContact_title,

    -- * Destructuring the Response
    PutAlternateContactResponse (..),
    newPutAlternateContactResponse,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAlternateContact' smart constructor.
data PutAlternateContact = PutAlternateContact'
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
    -- | Specifies which alternate contact you want to create or update.
    alternateContactType :: AlternateContactType,
    -- | Specifies an email address for the alternate contact.
    emailAddress :: Data.Sensitive Prelude.Text,
    -- | Specifies a name for the alternate contact.
    name :: Data.Sensitive Prelude.Text,
    -- | Specifies a phone number for the alternate contact.
    phoneNumber :: Data.Sensitive Prelude.Text,
    -- | Specifies a title for the alternate contact.
    title :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAlternateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'putAlternateContact_accountId' - Specifies the 12 digit account ID number of the Amazon Web Services
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
-- 'alternateContactType', 'putAlternateContact_alternateContactType' - Specifies which alternate contact you want to create or update.
--
-- 'emailAddress', 'putAlternateContact_emailAddress' - Specifies an email address for the alternate contact.
--
-- 'name', 'putAlternateContact_name' - Specifies a name for the alternate contact.
--
-- 'phoneNumber', 'putAlternateContact_phoneNumber' - Specifies a phone number for the alternate contact.
--
-- 'title', 'putAlternateContact_title' - Specifies a title for the alternate contact.
newPutAlternateContact ::
  -- | 'alternateContactType'
  AlternateContactType ->
  -- | 'emailAddress'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'phoneNumber'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  PutAlternateContact
newPutAlternateContact
  pAlternateContactType_
  pEmailAddress_
  pName_
  pPhoneNumber_
  pTitle_ =
    PutAlternateContact'
      { accountId = Prelude.Nothing,
        alternateContactType = pAlternateContactType_,
        emailAddress = Data._Sensitive Lens.# pEmailAddress_,
        name = Data._Sensitive Lens.# pName_,
        phoneNumber = Data._Sensitive Lens.# pPhoneNumber_,
        title = Data._Sensitive Lens.# pTitle_
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
putAlternateContact_accountId :: Lens.Lens' PutAlternateContact (Prelude.Maybe Prelude.Text)
putAlternateContact_accountId = Lens.lens (\PutAlternateContact' {accountId} -> accountId) (\s@PutAlternateContact' {} a -> s {accountId = a} :: PutAlternateContact)

-- | Specifies which alternate contact you want to create or update.
putAlternateContact_alternateContactType :: Lens.Lens' PutAlternateContact AlternateContactType
putAlternateContact_alternateContactType = Lens.lens (\PutAlternateContact' {alternateContactType} -> alternateContactType) (\s@PutAlternateContact' {} a -> s {alternateContactType = a} :: PutAlternateContact)

-- | Specifies an email address for the alternate contact.
putAlternateContact_emailAddress :: Lens.Lens' PutAlternateContact Prelude.Text
putAlternateContact_emailAddress = Lens.lens (\PutAlternateContact' {emailAddress} -> emailAddress) (\s@PutAlternateContact' {} a -> s {emailAddress = a} :: PutAlternateContact) Prelude.. Data._Sensitive

-- | Specifies a name for the alternate contact.
putAlternateContact_name :: Lens.Lens' PutAlternateContact Prelude.Text
putAlternateContact_name = Lens.lens (\PutAlternateContact' {name} -> name) (\s@PutAlternateContact' {} a -> s {name = a} :: PutAlternateContact) Prelude.. Data._Sensitive

-- | Specifies a phone number for the alternate contact.
putAlternateContact_phoneNumber :: Lens.Lens' PutAlternateContact Prelude.Text
putAlternateContact_phoneNumber = Lens.lens (\PutAlternateContact' {phoneNumber} -> phoneNumber) (\s@PutAlternateContact' {} a -> s {phoneNumber = a} :: PutAlternateContact) Prelude.. Data._Sensitive

-- | Specifies a title for the alternate contact.
putAlternateContact_title :: Lens.Lens' PutAlternateContact Prelude.Text
putAlternateContact_title = Lens.lens (\PutAlternateContact' {title} -> title) (\s@PutAlternateContact' {} a -> s {title = a} :: PutAlternateContact) Prelude.. Data._Sensitive

instance Core.AWSRequest PutAlternateContact where
  type
    AWSResponse PutAlternateContact =
      PutAlternateContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutAlternateContactResponse'

instance Prelude.Hashable PutAlternateContact where
  hashWithSalt _salt PutAlternateContact' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` alternateContactType
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` title

instance Prelude.NFData PutAlternateContact where
  rnf PutAlternateContact' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf alternateContactType
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf title

instance Data.ToHeaders PutAlternateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAlternateContact where
  toJSON PutAlternateContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            Prelude.Just
              ( "AlternateContactType"
                  Data..= alternateContactType
              ),
            Prelude.Just ("EmailAddress" Data..= emailAddress),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("PhoneNumber" Data..= phoneNumber),
            Prelude.Just ("Title" Data..= title)
          ]
      )

instance Data.ToPath PutAlternateContact where
  toPath = Prelude.const "/putAlternateContact"

instance Data.ToQuery PutAlternateContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAlternateContactResponse' smart constructor.
data PutAlternateContactResponse = PutAlternateContactResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAlternateContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutAlternateContactResponse ::
  PutAlternateContactResponse
newPutAlternateContactResponse =
  PutAlternateContactResponse'

instance Prelude.NFData PutAlternateContactResponse where
  rnf _ = ()
