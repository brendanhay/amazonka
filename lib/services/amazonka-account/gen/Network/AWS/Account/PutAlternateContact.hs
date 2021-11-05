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
-- Module      : Network.AWS.Account.PutAlternateContact
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Account.PutAlternateContact
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

import Network.AWS.Account.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    emailAddress :: Core.Sensitive Prelude.Text,
    -- | Specifies a name for the alternate contact.
    name :: Core.Sensitive Prelude.Text,
    -- | Specifies a phone number for the alternate contact.
    phoneNumber :: Core.Sensitive Prelude.Text,
    -- | Specifies a title for the alternate contact.
    title :: Core.Sensitive Prelude.Text
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
        emailAddress = Core._Sensitive Lens.# pEmailAddress_,
        name = Core._Sensitive Lens.# pName_,
        phoneNumber = Core._Sensitive Lens.# pPhoneNumber_,
        title = Core._Sensitive Lens.# pTitle_
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
putAlternateContact_emailAddress = Lens.lens (\PutAlternateContact' {emailAddress} -> emailAddress) (\s@PutAlternateContact' {} a -> s {emailAddress = a} :: PutAlternateContact) Prelude.. Core._Sensitive

-- | Specifies a name for the alternate contact.
putAlternateContact_name :: Lens.Lens' PutAlternateContact Prelude.Text
putAlternateContact_name = Lens.lens (\PutAlternateContact' {name} -> name) (\s@PutAlternateContact' {} a -> s {name = a} :: PutAlternateContact) Prelude.. Core._Sensitive

-- | Specifies a phone number for the alternate contact.
putAlternateContact_phoneNumber :: Lens.Lens' PutAlternateContact Prelude.Text
putAlternateContact_phoneNumber = Lens.lens (\PutAlternateContact' {phoneNumber} -> phoneNumber) (\s@PutAlternateContact' {} a -> s {phoneNumber = a} :: PutAlternateContact) Prelude.. Core._Sensitive

-- | Specifies a title for the alternate contact.
putAlternateContact_title :: Lens.Lens' PutAlternateContact Prelude.Text
putAlternateContact_title = Lens.lens (\PutAlternateContact' {title} -> title) (\s@PutAlternateContact' {} a -> s {title = a} :: PutAlternateContact) Prelude.. Core._Sensitive

instance Core.AWSRequest PutAlternateContact where
  type
    AWSResponse PutAlternateContact =
      PutAlternateContactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutAlternateContactResponse'

instance Prelude.Hashable PutAlternateContact

instance Prelude.NFData PutAlternateContact

instance Core.ToHeaders PutAlternateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAlternateContact where
  toJSON PutAlternateContact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccountId" Core..=) Prelude.<$> accountId,
            Prelude.Just
              ( "AlternateContactType"
                  Core..= alternateContactType
              ),
            Prelude.Just ("EmailAddress" Core..= emailAddress),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("PhoneNumber" Core..= phoneNumber),
            Prelude.Just ("Title" Core..= title)
          ]
      )

instance Core.ToPath PutAlternateContact where
  toPath = Prelude.const "/putAlternateContact"

instance Core.ToQuery PutAlternateContact where
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

instance Prelude.NFData PutAlternateContactResponse
