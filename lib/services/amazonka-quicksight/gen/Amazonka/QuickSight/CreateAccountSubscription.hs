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
-- Module      : Amazonka.QuickSight.CreateAccountSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon QuickSight account, or subscribes to Amazon QuickSight
-- Q.
--
-- The Amazon Web Services Region for the account is derived from what is
-- configured in the CLI or SDK. This operation isn\'t supported in the US
-- East (Ohio) Region, South America (Sao Paulo) Region, or Asia Pacific
-- (Singapore) Region.
--
-- Before you use this operation, make sure that you can connect to an
-- existing Amazon Web Services account. If you don\'t have an Amazon Web
-- Services account, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/setting-up-aws-sign-up.html Sign up for Amazon Web Services>
-- in the /Amazon QuickSight User Guide/. The person who signs up for
-- Amazon QuickSight needs to have the correct Identity and Access
-- Management (IAM) permissions. For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/iam-policy-examples.html IAM Policy Examples for Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- If your IAM policy includes both the @Subscribe@ and
-- @CreateAccountSubscription@ actions, make sure that both actions are set
-- to @Allow@. If either action is set to @Deny@, the @Deny@ action
-- prevails and your API call fails.
--
-- You can\'t pass an existing IAM role to access other Amazon Web Services
-- services using this API operation. To pass your existing IAM role to
-- Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/security_iam_service-with-iam.html#security-create-iam-role Passing IAM roles to Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- You can\'t set default resource access on the new account from the
-- Amazon QuickSight API. Instead, add default resource access from the
-- Amazon QuickSight console. For more information about setting default
-- resource access to Amazon Web Services services, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/scoping-policies-defaults.html Setting default resource access to Amazon Web Services services>
-- in the /Amazon QuickSight User Guide/.
module Amazonka.QuickSight.CreateAccountSubscription
  ( -- * Creating a Request
    CreateAccountSubscription (..),
    newCreateAccountSubscription,

    -- * Request Lenses
    createAccountSubscription_activeDirectoryName,
    createAccountSubscription_adminGroup,
    createAccountSubscription_authorGroup,
    createAccountSubscription_contactNumber,
    createAccountSubscription_directoryId,
    createAccountSubscription_emailAddress,
    createAccountSubscription_firstName,
    createAccountSubscription_lastName,
    createAccountSubscription_readerGroup,
    createAccountSubscription_realm,
    createAccountSubscription_edition,
    createAccountSubscription_authenticationMethod,
    createAccountSubscription_awsAccountId,
    createAccountSubscription_accountName,
    createAccountSubscription_notificationEmail,

    -- * Destructuring the Response
    CreateAccountSubscriptionResponse (..),
    newCreateAccountSubscriptionResponse,

    -- * Response Lenses
    createAccountSubscriptionResponse_requestId,
    createAccountSubscriptionResponse_signupResponse,
    createAccountSubscriptionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccountSubscription' smart constructor.
data CreateAccountSubscription = CreateAccountSubscription'
  { -- | The name of your Active Directory. This field is required if
    -- @ACTIVE_DIRECTORY@ is the selected authentication method of the new
    -- Amazon QuickSight account.
    activeDirectoryName :: Prelude.Maybe Prelude.Text,
    -- | The admin group associated with your Active Directory. This field is
    -- required if @ACTIVE_DIRECTORY@ is the selected authentication method of
    -- the new Amazon QuickSight account. For more information about using
    -- Active Directory in Amazon QuickSight, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
    -- in the Amazon QuickSight User Guide.
    adminGroup :: Prelude.Maybe [Prelude.Text],
    -- | The author group associated with your Active Directory. For more
    -- information about using Active Directory in Amazon QuickSight, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
    -- in the Amazon QuickSight User Guide.
    authorGroup :: Prelude.Maybe [Prelude.Text],
    -- | A 10-digit phone number for the author of the Amazon QuickSight account
    -- to use for future communications. This field is required if
    -- @ENTERPPRISE_AND_Q@ is the selected edition of the new Amazon QuickSight
    -- account.
    contactNumber :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Active Directory that is associated with your Amazon
    -- QuickSight account.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The email address of the author of the Amazon QuickSight account to use
    -- for future communications. This field is required if @ENTERPPRISE_AND_Q@
    -- is the selected edition of the new Amazon QuickSight account.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The first name of the author of the Amazon QuickSight account to use for
    -- future communications. This field is required if @ENTERPPRISE_AND_Q@ is
    -- the selected edition of the new Amazon QuickSight account.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The last name of the author of the Amazon QuickSight account to use for
    -- future communications. This field is required if @ENTERPPRISE_AND_Q@ is
    -- the selected edition of the new Amazon QuickSight account.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The reader group associated with your Active Direcrtory. For more
    -- information about using Active Directory in Amazon QuickSight, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
    -- in the /Amazon QuickSight User Guide/.
    readerGroup :: Prelude.Maybe [Prelude.Text],
    -- | The realm of the Active Directory that is associated with your Amazon
    -- QuickSight account. This field is required if @ACTIVE_DIRECTORY@ is the
    -- selected authentication method of the new Amazon QuickSight account.
    realm :: Prelude.Maybe Prelude.Text,
    -- | The edition of Amazon QuickSight that you want your account to have.
    -- Currently, you can choose from @ENTERPRISE@ or @ENTERPRISE_AND_Q@.
    --
    -- If you choose @ENTERPRISE_AND_Q@, the following parameters are required:
    --
    -- -   @FirstName@
    --
    -- -   @LastName@
    --
    -- -   @EmailAddress@
    --
    -- -   @ContactNumber@
    edition :: Edition,
    -- | The method that you want to use to authenticate your Amazon QuickSight
    -- account. Currently, the valid values for this parameter are
    -- @IAM_AND_QUICKSIGHT@, @IAM_ONLY@, and @ACTIVE_DIRECTORY@.
    --
    -- If you choose @ACTIVE_DIRECTORY@, provide an @ActiveDirectoryName@ and
    -- an @AdminGroup@ associated with your Active Directory.
    authenticationMethod :: AuthenticationMethodOption,
    -- | The Amazon Web Services account ID of the account that you\'re using to
    -- create your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The name of your Amazon QuickSight account. This name is unique over all
    -- of Amazon Web Services, and it appears only when users sign in. You
    -- can\'t change @AccountName@ value after the Amazon QuickSight account is
    -- created.
    accountName :: Prelude.Text,
    -- | The email address that you want Amazon QuickSight to send notifications
    -- to regarding your Amazon QuickSight account or Amazon QuickSight
    -- subscription.
    notificationEmail :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryName', 'createAccountSubscription_activeDirectoryName' - The name of your Active Directory. This field is required if
-- @ACTIVE_DIRECTORY@ is the selected authentication method of the new
-- Amazon QuickSight account.
--
-- 'adminGroup', 'createAccountSubscription_adminGroup' - The admin group associated with your Active Directory. This field is
-- required if @ACTIVE_DIRECTORY@ is the selected authentication method of
-- the new Amazon QuickSight account. For more information about using
-- Active Directory in Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
-- in the Amazon QuickSight User Guide.
--
-- 'authorGroup', 'createAccountSubscription_authorGroup' - The author group associated with your Active Directory. For more
-- information about using Active Directory in Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
-- in the Amazon QuickSight User Guide.
--
-- 'contactNumber', 'createAccountSubscription_contactNumber' - A 10-digit phone number for the author of the Amazon QuickSight account
-- to use for future communications. This field is required if
-- @ENTERPPRISE_AND_Q@ is the selected edition of the new Amazon QuickSight
-- account.
--
-- 'directoryId', 'createAccountSubscription_directoryId' - The ID of the Active Directory that is associated with your Amazon
-- QuickSight account.
--
-- 'emailAddress', 'createAccountSubscription_emailAddress' - The email address of the author of the Amazon QuickSight account to use
-- for future communications. This field is required if @ENTERPPRISE_AND_Q@
-- is the selected edition of the new Amazon QuickSight account.
--
-- 'firstName', 'createAccountSubscription_firstName' - The first name of the author of the Amazon QuickSight account to use for
-- future communications. This field is required if @ENTERPPRISE_AND_Q@ is
-- the selected edition of the new Amazon QuickSight account.
--
-- 'lastName', 'createAccountSubscription_lastName' - The last name of the author of the Amazon QuickSight account to use for
-- future communications. This field is required if @ENTERPPRISE_AND_Q@ is
-- the selected edition of the new Amazon QuickSight account.
--
-- 'readerGroup', 'createAccountSubscription_readerGroup' - The reader group associated with your Active Direcrtory. For more
-- information about using Active Directory in Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
-- in the /Amazon QuickSight User Guide/.
--
-- 'realm', 'createAccountSubscription_realm' - The realm of the Active Directory that is associated with your Amazon
-- QuickSight account. This field is required if @ACTIVE_DIRECTORY@ is the
-- selected authentication method of the new Amazon QuickSight account.
--
-- 'edition', 'createAccountSubscription_edition' - The edition of Amazon QuickSight that you want your account to have.
-- Currently, you can choose from @ENTERPRISE@ or @ENTERPRISE_AND_Q@.
--
-- If you choose @ENTERPRISE_AND_Q@, the following parameters are required:
--
-- -   @FirstName@
--
-- -   @LastName@
--
-- -   @EmailAddress@
--
-- -   @ContactNumber@
--
-- 'authenticationMethod', 'createAccountSubscription_authenticationMethod' - The method that you want to use to authenticate your Amazon QuickSight
-- account. Currently, the valid values for this parameter are
-- @IAM_AND_QUICKSIGHT@, @IAM_ONLY@, and @ACTIVE_DIRECTORY@.
--
-- If you choose @ACTIVE_DIRECTORY@, provide an @ActiveDirectoryName@ and
-- an @AdminGroup@ associated with your Active Directory.
--
-- 'awsAccountId', 'createAccountSubscription_awsAccountId' - The Amazon Web Services account ID of the account that you\'re using to
-- create your Amazon QuickSight account.
--
-- 'accountName', 'createAccountSubscription_accountName' - The name of your Amazon QuickSight account. This name is unique over all
-- of Amazon Web Services, and it appears only when users sign in. You
-- can\'t change @AccountName@ value after the Amazon QuickSight account is
-- created.
--
-- 'notificationEmail', 'createAccountSubscription_notificationEmail' - The email address that you want Amazon QuickSight to send notifications
-- to regarding your Amazon QuickSight account or Amazon QuickSight
-- subscription.
newCreateAccountSubscription ::
  -- | 'edition'
  Edition ->
  -- | 'authenticationMethod'
  AuthenticationMethodOption ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'accountName'
  Prelude.Text ->
  -- | 'notificationEmail'
  Prelude.Text ->
  CreateAccountSubscription
newCreateAccountSubscription
  pEdition_
  pAuthenticationMethod_
  pAwsAccountId_
  pAccountName_
  pNotificationEmail_ =
    CreateAccountSubscription'
      { activeDirectoryName =
          Prelude.Nothing,
        adminGroup = Prelude.Nothing,
        authorGroup = Prelude.Nothing,
        contactNumber = Prelude.Nothing,
        directoryId = Prelude.Nothing,
        emailAddress = Prelude.Nothing,
        firstName = Prelude.Nothing,
        lastName = Prelude.Nothing,
        readerGroup = Prelude.Nothing,
        realm = Prelude.Nothing,
        edition = pEdition_,
        authenticationMethod = pAuthenticationMethod_,
        awsAccountId = pAwsAccountId_,
        accountName = pAccountName_,
        notificationEmail = pNotificationEmail_
      }

-- | The name of your Active Directory. This field is required if
-- @ACTIVE_DIRECTORY@ is the selected authentication method of the new
-- Amazon QuickSight account.
createAccountSubscription_activeDirectoryName :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe Prelude.Text)
createAccountSubscription_activeDirectoryName = Lens.lens (\CreateAccountSubscription' {activeDirectoryName} -> activeDirectoryName) (\s@CreateAccountSubscription' {} a -> s {activeDirectoryName = a} :: CreateAccountSubscription)

-- | The admin group associated with your Active Directory. This field is
-- required if @ACTIVE_DIRECTORY@ is the selected authentication method of
-- the new Amazon QuickSight account. For more information about using
-- Active Directory in Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
-- in the Amazon QuickSight User Guide.
createAccountSubscription_adminGroup :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe [Prelude.Text])
createAccountSubscription_adminGroup = Lens.lens (\CreateAccountSubscription' {adminGroup} -> adminGroup) (\s@CreateAccountSubscription' {} a -> s {adminGroup = a} :: CreateAccountSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The author group associated with your Active Directory. For more
-- information about using Active Directory in Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
-- in the Amazon QuickSight User Guide.
createAccountSubscription_authorGroup :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe [Prelude.Text])
createAccountSubscription_authorGroup = Lens.lens (\CreateAccountSubscription' {authorGroup} -> authorGroup) (\s@CreateAccountSubscription' {} a -> s {authorGroup = a} :: CreateAccountSubscription) Prelude.. Lens.mapping Lens.coerced

-- | A 10-digit phone number for the author of the Amazon QuickSight account
-- to use for future communications. This field is required if
-- @ENTERPPRISE_AND_Q@ is the selected edition of the new Amazon QuickSight
-- account.
createAccountSubscription_contactNumber :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe Prelude.Text)
createAccountSubscription_contactNumber = Lens.lens (\CreateAccountSubscription' {contactNumber} -> contactNumber) (\s@CreateAccountSubscription' {} a -> s {contactNumber = a} :: CreateAccountSubscription)

-- | The ID of the Active Directory that is associated with your Amazon
-- QuickSight account.
createAccountSubscription_directoryId :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe Prelude.Text)
createAccountSubscription_directoryId = Lens.lens (\CreateAccountSubscription' {directoryId} -> directoryId) (\s@CreateAccountSubscription' {} a -> s {directoryId = a} :: CreateAccountSubscription)

-- | The email address of the author of the Amazon QuickSight account to use
-- for future communications. This field is required if @ENTERPPRISE_AND_Q@
-- is the selected edition of the new Amazon QuickSight account.
createAccountSubscription_emailAddress :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe Prelude.Text)
createAccountSubscription_emailAddress = Lens.lens (\CreateAccountSubscription' {emailAddress} -> emailAddress) (\s@CreateAccountSubscription' {} a -> s {emailAddress = a} :: CreateAccountSubscription)

-- | The first name of the author of the Amazon QuickSight account to use for
-- future communications. This field is required if @ENTERPPRISE_AND_Q@ is
-- the selected edition of the new Amazon QuickSight account.
createAccountSubscription_firstName :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe Prelude.Text)
createAccountSubscription_firstName = Lens.lens (\CreateAccountSubscription' {firstName} -> firstName) (\s@CreateAccountSubscription' {} a -> s {firstName = a} :: CreateAccountSubscription)

-- | The last name of the author of the Amazon QuickSight account to use for
-- future communications. This field is required if @ENTERPPRISE_AND_Q@ is
-- the selected edition of the new Amazon QuickSight account.
createAccountSubscription_lastName :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe Prelude.Text)
createAccountSubscription_lastName = Lens.lens (\CreateAccountSubscription' {lastName} -> lastName) (\s@CreateAccountSubscription' {} a -> s {lastName = a} :: CreateAccountSubscription)

-- | The reader group associated with your Active Direcrtory. For more
-- information about using Active Directory in Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/aws-directory-service.html Using Active Directory with Amazon QuickSight Enterprise Edition>
-- in the /Amazon QuickSight User Guide/.
createAccountSubscription_readerGroup :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe [Prelude.Text])
createAccountSubscription_readerGroup = Lens.lens (\CreateAccountSubscription' {readerGroup} -> readerGroup) (\s@CreateAccountSubscription' {} a -> s {readerGroup = a} :: CreateAccountSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The realm of the Active Directory that is associated with your Amazon
-- QuickSight account. This field is required if @ACTIVE_DIRECTORY@ is the
-- selected authentication method of the new Amazon QuickSight account.
createAccountSubscription_realm :: Lens.Lens' CreateAccountSubscription (Prelude.Maybe Prelude.Text)
createAccountSubscription_realm = Lens.lens (\CreateAccountSubscription' {realm} -> realm) (\s@CreateAccountSubscription' {} a -> s {realm = a} :: CreateAccountSubscription)

-- | The edition of Amazon QuickSight that you want your account to have.
-- Currently, you can choose from @ENTERPRISE@ or @ENTERPRISE_AND_Q@.
--
-- If you choose @ENTERPRISE_AND_Q@, the following parameters are required:
--
-- -   @FirstName@
--
-- -   @LastName@
--
-- -   @EmailAddress@
--
-- -   @ContactNumber@
createAccountSubscription_edition :: Lens.Lens' CreateAccountSubscription Edition
createAccountSubscription_edition = Lens.lens (\CreateAccountSubscription' {edition} -> edition) (\s@CreateAccountSubscription' {} a -> s {edition = a} :: CreateAccountSubscription)

-- | The method that you want to use to authenticate your Amazon QuickSight
-- account. Currently, the valid values for this parameter are
-- @IAM_AND_QUICKSIGHT@, @IAM_ONLY@, and @ACTIVE_DIRECTORY@.
--
-- If you choose @ACTIVE_DIRECTORY@, provide an @ActiveDirectoryName@ and
-- an @AdminGroup@ associated with your Active Directory.
createAccountSubscription_authenticationMethod :: Lens.Lens' CreateAccountSubscription AuthenticationMethodOption
createAccountSubscription_authenticationMethod = Lens.lens (\CreateAccountSubscription' {authenticationMethod} -> authenticationMethod) (\s@CreateAccountSubscription' {} a -> s {authenticationMethod = a} :: CreateAccountSubscription)

-- | The Amazon Web Services account ID of the account that you\'re using to
-- create your Amazon QuickSight account.
createAccountSubscription_awsAccountId :: Lens.Lens' CreateAccountSubscription Prelude.Text
createAccountSubscription_awsAccountId = Lens.lens (\CreateAccountSubscription' {awsAccountId} -> awsAccountId) (\s@CreateAccountSubscription' {} a -> s {awsAccountId = a} :: CreateAccountSubscription)

-- | The name of your Amazon QuickSight account. This name is unique over all
-- of Amazon Web Services, and it appears only when users sign in. You
-- can\'t change @AccountName@ value after the Amazon QuickSight account is
-- created.
createAccountSubscription_accountName :: Lens.Lens' CreateAccountSubscription Prelude.Text
createAccountSubscription_accountName = Lens.lens (\CreateAccountSubscription' {accountName} -> accountName) (\s@CreateAccountSubscription' {} a -> s {accountName = a} :: CreateAccountSubscription)

-- | The email address that you want Amazon QuickSight to send notifications
-- to regarding your Amazon QuickSight account or Amazon QuickSight
-- subscription.
createAccountSubscription_notificationEmail :: Lens.Lens' CreateAccountSubscription Prelude.Text
createAccountSubscription_notificationEmail = Lens.lens (\CreateAccountSubscription' {notificationEmail} -> notificationEmail) (\s@CreateAccountSubscription' {} a -> s {notificationEmail = a} :: CreateAccountSubscription)

instance Core.AWSRequest CreateAccountSubscription where
  type
    AWSResponse CreateAccountSubscription =
      CreateAccountSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccountSubscriptionResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "SignupResponse")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccountSubscription where
  hashWithSalt _salt CreateAccountSubscription' {..} =
    _salt `Prelude.hashWithSalt` activeDirectoryName
      `Prelude.hashWithSalt` adminGroup
      `Prelude.hashWithSalt` authorGroup
      `Prelude.hashWithSalt` contactNumber
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` readerGroup
      `Prelude.hashWithSalt` realm
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` authenticationMethod
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` notificationEmail

instance Prelude.NFData CreateAccountSubscription where
  rnf CreateAccountSubscription' {..} =
    Prelude.rnf activeDirectoryName
      `Prelude.seq` Prelude.rnf adminGroup
      `Prelude.seq` Prelude.rnf authorGroup
      `Prelude.seq` Prelude.rnf contactNumber
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf readerGroup
      `Prelude.seq` Prelude.rnf realm
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf authenticationMethod
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf notificationEmail

instance Data.ToHeaders CreateAccountSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccountSubscription where
  toJSON CreateAccountSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveDirectoryName" Data..=)
              Prelude.<$> activeDirectoryName,
            ("AdminGroup" Data..=) Prelude.<$> adminGroup,
            ("AuthorGroup" Data..=) Prelude.<$> authorGroup,
            ("ContactNumber" Data..=) Prelude.<$> contactNumber,
            ("DirectoryId" Data..=) Prelude.<$> directoryId,
            ("EmailAddress" Data..=) Prelude.<$> emailAddress,
            ("FirstName" Data..=) Prelude.<$> firstName,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("ReaderGroup" Data..=) Prelude.<$> readerGroup,
            ("Realm" Data..=) Prelude.<$> realm,
            Prelude.Just ("Edition" Data..= edition),
            Prelude.Just
              ( "AuthenticationMethod"
                  Data..= authenticationMethod
              ),
            Prelude.Just ("AccountName" Data..= accountName),
            Prelude.Just
              ("NotificationEmail" Data..= notificationEmail)
          ]
      )

instance Data.ToPath CreateAccountSubscription where
  toPath CreateAccountSubscription' {..} =
    Prelude.mconcat
      ["/account/", Data.toBS awsAccountId]

instance Data.ToQuery CreateAccountSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccountSubscriptionResponse' smart constructor.
data CreateAccountSubscriptionResponse = CreateAccountSubscriptionResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A @SignupResponse@ object that returns information about a newly created
    -- Amazon QuickSight account.
    signupResponse :: Prelude.Maybe SignupResponse,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createAccountSubscriptionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'signupResponse', 'createAccountSubscriptionResponse_signupResponse' - A @SignupResponse@ object that returns information about a newly created
-- Amazon QuickSight account.
--
-- 'status', 'createAccountSubscriptionResponse_status' - The HTTP status of the request.
newCreateAccountSubscriptionResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateAccountSubscriptionResponse
newCreateAccountSubscriptionResponse pStatus_ =
  CreateAccountSubscriptionResponse'
    { requestId =
        Prelude.Nothing,
      signupResponse = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
createAccountSubscriptionResponse_requestId :: Lens.Lens' CreateAccountSubscriptionResponse (Prelude.Maybe Prelude.Text)
createAccountSubscriptionResponse_requestId = Lens.lens (\CreateAccountSubscriptionResponse' {requestId} -> requestId) (\s@CreateAccountSubscriptionResponse' {} a -> s {requestId = a} :: CreateAccountSubscriptionResponse)

-- | A @SignupResponse@ object that returns information about a newly created
-- Amazon QuickSight account.
createAccountSubscriptionResponse_signupResponse :: Lens.Lens' CreateAccountSubscriptionResponse (Prelude.Maybe SignupResponse)
createAccountSubscriptionResponse_signupResponse = Lens.lens (\CreateAccountSubscriptionResponse' {signupResponse} -> signupResponse) (\s@CreateAccountSubscriptionResponse' {} a -> s {signupResponse = a} :: CreateAccountSubscriptionResponse)

-- | The HTTP status of the request.
createAccountSubscriptionResponse_status :: Lens.Lens' CreateAccountSubscriptionResponse Prelude.Int
createAccountSubscriptionResponse_status = Lens.lens (\CreateAccountSubscriptionResponse' {status} -> status) (\s@CreateAccountSubscriptionResponse' {} a -> s {status = a} :: CreateAccountSubscriptionResponse)

instance
  Prelude.NFData
    CreateAccountSubscriptionResponse
  where
  rnf CreateAccountSubscriptionResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf signupResponse
      `Prelude.seq` Prelude.rnf status
