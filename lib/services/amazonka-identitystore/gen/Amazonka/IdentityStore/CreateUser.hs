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
-- Module      : Amazonka.IdentityStore.CreateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user within the specified identity store.
module Amazonka.IdentityStore.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_name,
    createUser_userName,
    createUser_locale,
    createUser_timezone,
    createUser_displayName,
    createUser_userType,
    createUser_profileUrl,
    createUser_preferredLanguage,
    createUser_title,
    createUser_emails,
    createUser_addresses,
    createUser_nickName,
    createUser_phoneNumbers,
    createUser_identityStoreId,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_httpStatus,
    createUserResponse_userId,
    createUserResponse_identityStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | An object containing the user\'s name.
    name :: Prelude.Maybe Name,
    -- | A unique string used to identify the user. The length limit is 128
    -- characters. This value can consist of letters, accented characters,
    -- symbols, numbers, and punctuation. This value is specified at the time
    -- the user is created and stored as an attribute of the user object in the
    -- identity store.
    userName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the user\'s geographical region or location.
    locale :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the user\'s time zone.
    timezone :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the user\'s name. This value is typically formatted
    -- for display when the user is referenced. For example, \"John Doe.\"
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string indicating the user\'s type. Possible values depend on each
    -- customer\'s specific needs, so they are left unspecified.
    userType :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing a URL that may be associated with the user.
    profileUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the preferred language of the user. For example,
    -- \"American English\" or \"en-us.\"
    preferredLanguage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the user\'s title. Possible values are left
    -- unspecified given that they depend on each customer\'s specific needs.
    title :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A list of @Email@ objects containing email addresses associated with the
    -- user.
    emails :: Prelude.Maybe (Prelude.NonEmpty Email),
    -- | A list of @Address@ objects containing addresses associated with the
    -- user.
    addresses :: Prelude.Maybe (Prelude.NonEmpty Address),
    -- | A string containing an alternate name for the user.
    nickName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A list of @PhoneNumber@ objects containing phone numbers associated with
    -- the user.
    phoneNumbers :: Prelude.Maybe (Prelude.NonEmpty PhoneNumber),
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createUser_name' - An object containing the user\'s name.
--
-- 'userName', 'createUser_userName' - A unique string used to identify the user. The length limit is 128
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, and punctuation. This value is specified at the time
-- the user is created and stored as an attribute of the user object in the
-- identity store.
--
-- 'locale', 'createUser_locale' - A string containing the user\'s geographical region or location.
--
-- 'timezone', 'createUser_timezone' - A string containing the user\'s time zone.
--
-- 'displayName', 'createUser_displayName' - A string containing the user\'s name. This value is typically formatted
-- for display when the user is referenced. For example, \"John Doe.\"
--
-- 'userType', 'createUser_userType' - A string indicating the user\'s type. Possible values depend on each
-- customer\'s specific needs, so they are left unspecified.
--
-- 'profileUrl', 'createUser_profileUrl' - A string containing a URL that may be associated with the user.
--
-- 'preferredLanguage', 'createUser_preferredLanguage' - A string containing the preferred language of the user. For example,
-- \"American English\" or \"en-us.\"
--
-- 'title', 'createUser_title' - A string containing the user\'s title. Possible values are left
-- unspecified given that they depend on each customer\'s specific needs.
--
-- 'emails', 'createUser_emails' - A list of @Email@ objects containing email addresses associated with the
-- user.
--
-- 'addresses', 'createUser_addresses' - A list of @Address@ objects containing addresses associated with the
-- user.
--
-- 'nickName', 'createUser_nickName' - A string containing an alternate name for the user.
--
-- 'phoneNumbers', 'createUser_phoneNumbers' - A list of @PhoneNumber@ objects containing phone numbers associated with
-- the user.
--
-- 'identityStoreId', 'createUser_identityStoreId' - The globally unique identifier for the identity store.
newCreateUser ::
  -- | 'identityStoreId'
  Prelude.Text ->
  CreateUser
newCreateUser pIdentityStoreId_ =
  CreateUser'
    { name = Prelude.Nothing,
      userName = Prelude.Nothing,
      locale = Prelude.Nothing,
      timezone = Prelude.Nothing,
      displayName = Prelude.Nothing,
      userType = Prelude.Nothing,
      profileUrl = Prelude.Nothing,
      preferredLanguage = Prelude.Nothing,
      title = Prelude.Nothing,
      emails = Prelude.Nothing,
      addresses = Prelude.Nothing,
      nickName = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      identityStoreId = pIdentityStoreId_
    }

-- | An object containing the user\'s name.
createUser_name :: Lens.Lens' CreateUser (Prelude.Maybe Name)
createUser_name = Lens.lens (\CreateUser' {name} -> name) (\s@CreateUser' {} a -> s {name = a} :: CreateUser)

-- | A unique string used to identify the user. The length limit is 128
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, and punctuation. This value is specified at the time
-- the user is created and stored as an attribute of the user object in the
-- identity store.
createUser_userName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_userName = Lens.lens (\CreateUser' {userName} -> userName) (\s@CreateUser' {} a -> s {userName = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the user\'s geographical region or location.
createUser_locale :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_locale = Lens.lens (\CreateUser' {locale} -> locale) (\s@CreateUser' {} a -> s {locale = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the user\'s time zone.
createUser_timezone :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_timezone = Lens.lens (\CreateUser' {timezone} -> timezone) (\s@CreateUser' {} a -> s {timezone = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the user\'s name. This value is typically formatted
-- for display when the user is referenced. For example, \"John Doe.\"
createUser_displayName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_displayName = Lens.lens (\CreateUser' {displayName} -> displayName) (\s@CreateUser' {} a -> s {displayName = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A string indicating the user\'s type. Possible values depend on each
-- customer\'s specific needs, so they are left unspecified.
createUser_userType :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_userType = Lens.lens (\CreateUser' {userType} -> userType) (\s@CreateUser' {} a -> s {userType = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing a URL that may be associated with the user.
createUser_profileUrl :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_profileUrl = Lens.lens (\CreateUser' {profileUrl} -> profileUrl) (\s@CreateUser' {} a -> s {profileUrl = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the preferred language of the user. For example,
-- \"American English\" or \"en-us.\"
createUser_preferredLanguage :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_preferredLanguage = Lens.lens (\CreateUser' {preferredLanguage} -> preferredLanguage) (\s@CreateUser' {} a -> s {preferredLanguage = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the user\'s title. Possible values are left
-- unspecified given that they depend on each customer\'s specific needs.
createUser_title :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_title = Lens.lens (\CreateUser' {title} -> title) (\s@CreateUser' {} a -> s {title = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A list of @Email@ objects containing email addresses associated with the
-- user.
createUser_emails :: Lens.Lens' CreateUser (Prelude.Maybe (Prelude.NonEmpty Email))
createUser_emails = Lens.lens (\CreateUser' {emails} -> emails) (\s@CreateUser' {} a -> s {emails = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | A list of @Address@ objects containing addresses associated with the
-- user.
createUser_addresses :: Lens.Lens' CreateUser (Prelude.Maybe (Prelude.NonEmpty Address))
createUser_addresses = Lens.lens (\CreateUser' {addresses} -> addresses) (\s@CreateUser' {} a -> s {addresses = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | A string containing an alternate name for the user.
createUser_nickName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_nickName = Lens.lens (\CreateUser' {nickName} -> nickName) (\s@CreateUser' {} a -> s {nickName = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | A list of @PhoneNumber@ objects containing phone numbers associated with
-- the user.
createUser_phoneNumbers :: Lens.Lens' CreateUser (Prelude.Maybe (Prelude.NonEmpty PhoneNumber))
createUser_phoneNumbers = Lens.lens (\CreateUser' {phoneNumbers} -> phoneNumbers) (\s@CreateUser' {} a -> s {phoneNumbers = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | The globally unique identifier for the identity store.
createUser_identityStoreId :: Lens.Lens' CreateUser Prelude.Text
createUser_identityStoreId = Lens.lens (\CreateUser' {identityStoreId} -> identityStoreId) (\s@CreateUser' {} a -> s {identityStoreId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UserId")
            Prelude.<*> (x Data..:> "IdentityStoreId")
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` profileUrl
      `Prelude.hashWithSalt` preferredLanguage
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` emails
      `Prelude.hashWithSalt` addresses
      `Prelude.hashWithSalt` nickName
      `Prelude.hashWithSalt` phoneNumbers
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf userType
      `Prelude.seq` Prelude.rnf profileUrl
      `Prelude.seq` Prelude.rnf preferredLanguage
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf emails
      `Prelude.seq` Prelude.rnf addresses
      `Prelude.seq` Prelude.rnf nickName
      `Prelude.seq` Prelude.rnf phoneNumbers
      `Prelude.seq` Prelude.rnf identityStoreId

instance Data.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.CreateUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("UserName" Data..=) Prelude.<$> userName,
            ("Locale" Data..=) Prelude.<$> locale,
            ("Timezone" Data..=) Prelude.<$> timezone,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("UserType" Data..=) Prelude.<$> userType,
            ("ProfileUrl" Data..=) Prelude.<$> profileUrl,
            ("PreferredLanguage" Data..=)
              Prelude.<$> preferredLanguage,
            ("Title" Data..=) Prelude.<$> title,
            ("Emails" Data..=) Prelude.<$> emails,
            ("Addresses" Data..=) Prelude.<$> addresses,
            ("NickName" Data..=) Prelude.<$> nickName,
            ("PhoneNumbers" Data..=) Prelude.<$> phoneNumbers,
            Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId)
          ]
      )

instance Data.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the newly created user in the identity store.
    userId :: Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
--
-- 'userId', 'createUserResponse_userId' - The identifier of the newly created user in the identity store.
--
-- 'identityStoreId', 'createUserResponse_identityStoreId' - The globally unique identifier for the identity store.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  CreateUserResponse
newCreateUserResponse
  pHttpStatus_
  pUserId_
  pIdentityStoreId_ =
    CreateUserResponse'
      { httpStatus = pHttpStatus_,
        userId = pUserId_,
        identityStoreId = pIdentityStoreId_
      }

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

-- | The identifier of the newly created user in the identity store.
createUserResponse_userId :: Lens.Lens' CreateUserResponse Prelude.Text
createUserResponse_userId = Lens.lens (\CreateUserResponse' {userId} -> userId) (\s@CreateUserResponse' {} a -> s {userId = a} :: CreateUserResponse)

-- | The globally unique identifier for the identity store.
createUserResponse_identityStoreId :: Lens.Lens' CreateUserResponse Prelude.Text
createUserResponse_identityStoreId = Lens.lens (\CreateUserResponse' {identityStoreId} -> identityStoreId) (\s@CreateUserResponse' {} a -> s {identityStoreId = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf identityStoreId
