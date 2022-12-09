{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IdentityStore.Types.User
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types.Address
import Amazonka.IdentityStore.Types.Email
import Amazonka.IdentityStore.Types.ExternalId
import Amazonka.IdentityStore.Types.Name
import Amazonka.IdentityStore.Types.PhoneNumber
import qualified Amazonka.Prelude as Prelude

-- | A user object that contains a specified user’s metadata and attributes.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | A list of @Address@ objects containing addresses associated with the
    -- user.
    addresses :: Prelude.Maybe (Prelude.NonEmpty Address),
    -- | A string containing the user\'s name that\'s formatted for display when
    -- the user is referenced. For example, \"John Doe.\"
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A list of @Email@ objects containing email addresses associated with the
    -- user.
    emails :: Prelude.Maybe (Prelude.NonEmpty Email),
    -- | A list of @ExternalId@ objects that contains the identifiers issued to
    -- this resource by an external identity provider.
    externalIds :: Prelude.Maybe (Prelude.NonEmpty ExternalId),
    -- | A string containing the user\'s geographical region or location.
    locale :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An object containing the user\'s name.
    name :: Prelude.Maybe Name,
    -- | A string containing an alternate name for the user.
    nickName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A list of @PhoneNumber@ objects containing phone numbers associated with
    -- the user.
    phoneNumbers :: Prelude.Maybe (Prelude.NonEmpty PhoneNumber),
    -- | A string containing the preferred language of the user. For example,
    -- \"American English\" or \"en-us.\"
    preferredLanguage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing a URL that may be associated with the user.
    profileUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the user\'s time zone.
    timezone :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing the user\'s title. Possible values depend on each
    -- customer\'s specific needs, so they are left unspecified.
    title :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A unique string used to identify the user. The length limit is 128
    -- characters. This value can consist of letters, accented characters,
    -- symbols, numbers, and punctuation. This value is specified at the time
    -- the user is created and stored as an attribute of the user object in the
    -- identity store.
    userName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string indicating the user\'s type. Possible values depend on each
    -- customer\'s specific needs, so they are left unspecified.
    userType :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identifier for a user in the identity store.
    userId :: Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addresses', 'user_addresses' - A list of @Address@ objects containing addresses associated with the
-- user.
--
-- 'displayName', 'user_displayName' - A string containing the user\'s name that\'s formatted for display when
-- the user is referenced. For example, \"John Doe.\"
--
-- 'emails', 'user_emails' - A list of @Email@ objects containing email addresses associated with the
-- user.
--
-- 'externalIds', 'user_externalIds' - A list of @ExternalId@ objects that contains the identifiers issued to
-- this resource by an external identity provider.
--
-- 'locale', 'user_locale' - A string containing the user\'s geographical region or location.
--
-- 'name', 'user_name' - An object containing the user\'s name.
--
-- 'nickName', 'user_nickName' - A string containing an alternate name for the user.
--
-- 'phoneNumbers', 'user_phoneNumbers' - A list of @PhoneNumber@ objects containing phone numbers associated with
-- the user.
--
-- 'preferredLanguage', 'user_preferredLanguage' - A string containing the preferred language of the user. For example,
-- \"American English\" or \"en-us.\"
--
-- 'profileUrl', 'user_profileUrl' - A string containing a URL that may be associated with the user.
--
-- 'timezone', 'user_timezone' - A string containing the user\'s time zone.
--
-- 'title', 'user_title' - A string containing the user\'s title. Possible values depend on each
-- customer\'s specific needs, so they are left unspecified.
--
-- 'userName', 'user_userName' - A unique string used to identify the user. The length limit is 128
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, and punctuation. This value is specified at the time
-- the user is created and stored as an attribute of the user object in the
-- identity store.
--
-- 'userType', 'user_userType' - A string indicating the user\'s type. Possible values depend on each
-- customer\'s specific needs, so they are left unspecified.
--
-- 'userId', 'user_userId' - The identifier for a user in the identity store.
--
-- 'identityStoreId', 'user_identityStoreId' - The globally unique identifier for the identity store.
newUser ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  User
newUser pUserId_ pIdentityStoreId_ =
  User'
    { addresses = Prelude.Nothing,
      displayName = Prelude.Nothing,
      emails = Prelude.Nothing,
      externalIds = Prelude.Nothing,
      locale = Prelude.Nothing,
      name = Prelude.Nothing,
      nickName = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      preferredLanguage = Prelude.Nothing,
      profileUrl = Prelude.Nothing,
      timezone = Prelude.Nothing,
      title = Prelude.Nothing,
      userName = Prelude.Nothing,
      userType = Prelude.Nothing,
      userId = pUserId_,
      identityStoreId = pIdentityStoreId_
    }

-- | A list of @Address@ objects containing addresses associated with the
-- user.
user_addresses :: Lens.Lens' User (Prelude.Maybe (Prelude.NonEmpty Address))
user_addresses = Lens.lens (\User' {addresses} -> addresses) (\s@User' {} a -> s {addresses = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | A string containing the user\'s name that\'s formatted for display when
-- the user is referenced. For example, \"John Doe.\"
user_displayName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_displayName = Lens.lens (\User' {displayName} -> displayName) (\s@User' {} a -> s {displayName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | A list of @Email@ objects containing email addresses associated with the
-- user.
user_emails :: Lens.Lens' User (Prelude.Maybe (Prelude.NonEmpty Email))
user_emails = Lens.lens (\User' {emails} -> emails) (\s@User' {} a -> s {emails = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | A list of @ExternalId@ objects that contains the identifiers issued to
-- this resource by an external identity provider.
user_externalIds :: Lens.Lens' User (Prelude.Maybe (Prelude.NonEmpty ExternalId))
user_externalIds = Lens.lens (\User' {externalIds} -> externalIds) (\s@User' {} a -> s {externalIds = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | A string containing the user\'s geographical region or location.
user_locale :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_locale = Lens.lens (\User' {locale} -> locale) (\s@User' {} a -> s {locale = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | An object containing the user\'s name.
user_name :: Lens.Lens' User (Prelude.Maybe Name)
user_name = Lens.lens (\User' {name} -> name) (\s@User' {} a -> s {name = a} :: User)

-- | A string containing an alternate name for the user.
user_nickName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_nickName = Lens.lens (\User' {nickName} -> nickName) (\s@User' {} a -> s {nickName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | A list of @PhoneNumber@ objects containing phone numbers associated with
-- the user.
user_phoneNumbers :: Lens.Lens' User (Prelude.Maybe (Prelude.NonEmpty PhoneNumber))
user_phoneNumbers = Lens.lens (\User' {phoneNumbers} -> phoneNumbers) (\s@User' {} a -> s {phoneNumbers = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | A string containing the preferred language of the user. For example,
-- \"American English\" or \"en-us.\"
user_preferredLanguage :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_preferredLanguage = Lens.lens (\User' {preferredLanguage} -> preferredLanguage) (\s@User' {} a -> s {preferredLanguage = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing a URL that may be associated with the user.
user_profileUrl :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_profileUrl = Lens.lens (\User' {profileUrl} -> profileUrl) (\s@User' {} a -> s {profileUrl = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the user\'s time zone.
user_timezone :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_timezone = Lens.lens (\User' {timezone} -> timezone) (\s@User' {} a -> s {timezone = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing the user\'s title. Possible values depend on each
-- customer\'s specific needs, so they are left unspecified.
user_title :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_title = Lens.lens (\User' {title} -> title) (\s@User' {} a -> s {title = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | A unique string used to identify the user. The length limit is 128
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, and punctuation. This value is specified at the time
-- the user is created and stored as an attribute of the user object in the
-- identity store.
user_userName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | A string indicating the user\'s type. Possible values depend on each
-- customer\'s specific needs, so they are left unspecified.
user_userType :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userType = Lens.lens (\User' {userType} -> userType) (\s@User' {} a -> s {userType = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier for a user in the identity store.
user_userId :: Lens.Lens' User Prelude.Text
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

-- | The globally unique identifier for the identity store.
user_identityStoreId :: Lens.Lens' User Prelude.Text
user_identityStoreId = Lens.lens (\User' {identityStoreId} -> identityStoreId) (\s@User' {} a -> s {identityStoreId = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "Addresses")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "Emails")
            Prelude.<*> (x Data..:? "ExternalIds")
            Prelude.<*> (x Data..:? "Locale")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NickName")
            Prelude.<*> (x Data..:? "PhoneNumbers")
            Prelude.<*> (x Data..:? "PreferredLanguage")
            Prelude.<*> (x Data..:? "ProfileUrl")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..:? "UserName")
            Prelude.<*> (x Data..:? "UserType")
            Prelude.<*> (x Data..: "UserId")
            Prelude.<*> (x Data..: "IdentityStoreId")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` addresses
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` emails
      `Prelude.hashWithSalt` externalIds
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nickName
      `Prelude.hashWithSalt` phoneNumbers
      `Prelude.hashWithSalt` preferredLanguage
      `Prelude.hashWithSalt` profileUrl
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf addresses
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf emails
      `Prelude.seq` Prelude.rnf externalIds
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nickName
      `Prelude.seq` Prelude.rnf phoneNumbers
      `Prelude.seq` Prelude.rnf preferredLanguage
      `Prelude.seq` Prelude.rnf profileUrl
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf userType
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf identityStoreId
