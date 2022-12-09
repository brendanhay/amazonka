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
-- Module      : Amazonka.WorkDocs.Types.User
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.LocaleType
import Amazonka.WorkDocs.Types.UserStatusType
import Amazonka.WorkDocs.Types.UserStorageMetadata
import Amazonka.WorkDocs.Types.UserType

-- | Describes a user.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The time when the user was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The email address of the user.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The given name of the user.
    givenName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The locale of the user.
    locale :: Prelude.Maybe LocaleType,
    -- | The time when the user was modified.
    modifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the recycle bin folder.
    recycleBinFolderId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the root folder.
    rootFolderId :: Prelude.Maybe Prelude.Text,
    -- | The status of the user.
    status :: Prelude.Maybe UserStatusType,
    -- | The storage for the user.
    storage :: Prelude.Maybe UserStorageMetadata,
    -- | The surname of the user.
    surname :: Prelude.Maybe Prelude.Text,
    -- | The time zone ID of the user.
    timeZoneId :: Prelude.Maybe Prelude.Text,
    -- | The type of user.
    type' :: Prelude.Maybe UserType,
    -- | The login name of the user.
    username :: Prelude.Maybe Prelude.Text
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
-- 'createdTimestamp', 'user_createdTimestamp' - The time when the user was created.
--
-- 'emailAddress', 'user_emailAddress' - The email address of the user.
--
-- 'givenName', 'user_givenName' - The given name of the user.
--
-- 'id', 'user_id' - The ID of the user.
--
-- 'locale', 'user_locale' - The locale of the user.
--
-- 'modifiedTimestamp', 'user_modifiedTimestamp' - The time when the user was modified.
--
-- 'organizationId', 'user_organizationId' - The ID of the organization.
--
-- 'recycleBinFolderId', 'user_recycleBinFolderId' - The ID of the recycle bin folder.
--
-- 'rootFolderId', 'user_rootFolderId' - The ID of the root folder.
--
-- 'status', 'user_status' - The status of the user.
--
-- 'storage', 'user_storage' - The storage for the user.
--
-- 'surname', 'user_surname' - The surname of the user.
--
-- 'timeZoneId', 'user_timeZoneId' - The time zone ID of the user.
--
-- 'type'', 'user_type' - The type of user.
--
-- 'username', 'user_username' - The login name of the user.
newUser ::
  User
newUser =
  User'
    { createdTimestamp = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      givenName = Prelude.Nothing,
      id = Prelude.Nothing,
      locale = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      recycleBinFolderId = Prelude.Nothing,
      rootFolderId = Prelude.Nothing,
      status = Prelude.Nothing,
      storage = Prelude.Nothing,
      surname = Prelude.Nothing,
      timeZoneId = Prelude.Nothing,
      type' = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The time when the user was created.
user_createdTimestamp :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_createdTimestamp = Lens.lens (\User' {createdTimestamp} -> createdTimestamp) (\s@User' {} a -> s {createdTimestamp = a} :: User) Prelude.. Lens.mapping Data._Time

-- | The email address of the user.
user_emailAddress :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_emailAddress = Lens.lens (\User' {emailAddress} -> emailAddress) (\s@User' {} a -> s {emailAddress = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The given name of the user.
user_givenName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_givenName = Lens.lens (\User' {givenName} -> givenName) (\s@User' {} a -> s {givenName = a} :: User)

-- | The ID of the user.
user_id :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The locale of the user.
user_locale :: Lens.Lens' User (Prelude.Maybe LocaleType)
user_locale = Lens.lens (\User' {locale} -> locale) (\s@User' {} a -> s {locale = a} :: User)

-- | The time when the user was modified.
user_modifiedTimestamp :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_modifiedTimestamp = Lens.lens (\User' {modifiedTimestamp} -> modifiedTimestamp) (\s@User' {} a -> s {modifiedTimestamp = a} :: User) Prelude.. Lens.mapping Data._Time

-- | The ID of the organization.
user_organizationId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_organizationId = Lens.lens (\User' {organizationId} -> organizationId) (\s@User' {} a -> s {organizationId = a} :: User)

-- | The ID of the recycle bin folder.
user_recycleBinFolderId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_recycleBinFolderId = Lens.lens (\User' {recycleBinFolderId} -> recycleBinFolderId) (\s@User' {} a -> s {recycleBinFolderId = a} :: User)

-- | The ID of the root folder.
user_rootFolderId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_rootFolderId = Lens.lens (\User' {rootFolderId} -> rootFolderId) (\s@User' {} a -> s {rootFolderId = a} :: User)

-- | The status of the user.
user_status :: Lens.Lens' User (Prelude.Maybe UserStatusType)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The storage for the user.
user_storage :: Lens.Lens' User (Prelude.Maybe UserStorageMetadata)
user_storage = Lens.lens (\User' {storage} -> storage) (\s@User' {} a -> s {storage = a} :: User)

-- | The surname of the user.
user_surname :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_surname = Lens.lens (\User' {surname} -> surname) (\s@User' {} a -> s {surname = a} :: User)

-- | The time zone ID of the user.
user_timeZoneId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_timeZoneId = Lens.lens (\User' {timeZoneId} -> timeZoneId) (\s@User' {} a -> s {timeZoneId = a} :: User)

-- | The type of user.
user_type :: Lens.Lens' User (Prelude.Maybe UserType)
user_type = Lens.lens (\User' {type'} -> type') (\s@User' {} a -> s {type' = a} :: User)

-- | The login name of the user.
user_username :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "EmailAddress")
            Prelude.<*> (x Data..:? "GivenName")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Locale")
            Prelude.<*> (x Data..:? "ModifiedTimestamp")
            Prelude.<*> (x Data..:? "OrganizationId")
            Prelude.<*> (x Data..:? "RecycleBinFolderId")
            Prelude.<*> (x Data..:? "RootFolderId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Storage")
            Prelude.<*> (x Data..:? "Surname")
            Prelude.<*> (x Data..:? "TimeZoneId")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` givenName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` modifiedTimestamp
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` recycleBinFolderId
      `Prelude.hashWithSalt` rootFolderId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storage
      `Prelude.hashWithSalt` surname
      `Prelude.hashWithSalt` timeZoneId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` username

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf givenName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf modifiedTimestamp
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf recycleBinFolderId
      `Prelude.seq` Prelude.rnf rootFolderId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storage
      `Prelude.seq` Prelude.rnf surname
      `Prelude.seq` Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf username
