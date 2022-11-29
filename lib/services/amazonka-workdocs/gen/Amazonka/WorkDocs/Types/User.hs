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
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.LocaleType
import Amazonka.WorkDocs.Types.UserStatusType
import Amazonka.WorkDocs.Types.UserStorageMetadata
import Amazonka.WorkDocs.Types.UserType

-- | Describes a user.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The ID of the recycle bin folder.
    recycleBinFolderId :: Prelude.Maybe Prelude.Text,
    -- | The type of user.
    type' :: Prelude.Maybe UserType,
    -- | The time zone ID of the user.
    timeZoneId :: Prelude.Maybe Prelude.Text,
    -- | The time when the user was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The login name of the user.
    username :: Prelude.Maybe Prelude.Text,
    -- | The locale of the user.
    locale :: Prelude.Maybe LocaleType,
    -- | The given name of the user.
    givenName :: Prelude.Maybe Prelude.Text,
    -- | The storage for the user.
    storage :: Prelude.Maybe UserStorageMetadata,
    -- | The status of the user.
    status :: Prelude.Maybe UserStatusType,
    -- | The ID of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The surname of the user.
    surname :: Prelude.Maybe Prelude.Text,
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The email address of the user.
    emailAddress :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the root folder.
    rootFolderId :: Prelude.Maybe Prelude.Text,
    -- | The time when the user was modified.
    modifiedTimestamp :: Prelude.Maybe Core.POSIX
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
-- 'recycleBinFolderId', 'user_recycleBinFolderId' - The ID of the recycle bin folder.
--
-- 'type'', 'user_type' - The type of user.
--
-- 'timeZoneId', 'user_timeZoneId' - The time zone ID of the user.
--
-- 'createdTimestamp', 'user_createdTimestamp' - The time when the user was created.
--
-- 'username', 'user_username' - The login name of the user.
--
-- 'locale', 'user_locale' - The locale of the user.
--
-- 'givenName', 'user_givenName' - The given name of the user.
--
-- 'storage', 'user_storage' - The storage for the user.
--
-- 'status', 'user_status' - The status of the user.
--
-- 'id', 'user_id' - The ID of the user.
--
-- 'surname', 'user_surname' - The surname of the user.
--
-- 'organizationId', 'user_organizationId' - The ID of the organization.
--
-- 'emailAddress', 'user_emailAddress' - The email address of the user.
--
-- 'rootFolderId', 'user_rootFolderId' - The ID of the root folder.
--
-- 'modifiedTimestamp', 'user_modifiedTimestamp' - The time when the user was modified.
newUser ::
  User
newUser =
  User'
    { recycleBinFolderId = Prelude.Nothing,
      type' = Prelude.Nothing,
      timeZoneId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      username = Prelude.Nothing,
      locale = Prelude.Nothing,
      givenName = Prelude.Nothing,
      storage = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      surname = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      rootFolderId = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing
    }

-- | The ID of the recycle bin folder.
user_recycleBinFolderId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_recycleBinFolderId = Lens.lens (\User' {recycleBinFolderId} -> recycleBinFolderId) (\s@User' {} a -> s {recycleBinFolderId = a} :: User)

-- | The type of user.
user_type :: Lens.Lens' User (Prelude.Maybe UserType)
user_type = Lens.lens (\User' {type'} -> type') (\s@User' {} a -> s {type' = a} :: User)

-- | The time zone ID of the user.
user_timeZoneId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_timeZoneId = Lens.lens (\User' {timeZoneId} -> timeZoneId) (\s@User' {} a -> s {timeZoneId = a} :: User)

-- | The time when the user was created.
user_createdTimestamp :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_createdTimestamp = Lens.lens (\User' {createdTimestamp} -> createdTimestamp) (\s@User' {} a -> s {createdTimestamp = a} :: User) Prelude.. Lens.mapping Core._Time

-- | The login name of the user.
user_username :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

-- | The locale of the user.
user_locale :: Lens.Lens' User (Prelude.Maybe LocaleType)
user_locale = Lens.lens (\User' {locale} -> locale) (\s@User' {} a -> s {locale = a} :: User)

-- | The given name of the user.
user_givenName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_givenName = Lens.lens (\User' {givenName} -> givenName) (\s@User' {} a -> s {givenName = a} :: User)

-- | The storage for the user.
user_storage :: Lens.Lens' User (Prelude.Maybe UserStorageMetadata)
user_storage = Lens.lens (\User' {storage} -> storage) (\s@User' {} a -> s {storage = a} :: User)

-- | The status of the user.
user_status :: Lens.Lens' User (Prelude.Maybe UserStatusType)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The ID of the user.
user_id :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The surname of the user.
user_surname :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_surname = Lens.lens (\User' {surname} -> surname) (\s@User' {} a -> s {surname = a} :: User)

-- | The ID of the organization.
user_organizationId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_organizationId = Lens.lens (\User' {organizationId} -> organizationId) (\s@User' {} a -> s {organizationId = a} :: User)

-- | The email address of the user.
user_emailAddress :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_emailAddress = Lens.lens (\User' {emailAddress} -> emailAddress) (\s@User' {} a -> s {emailAddress = a} :: User) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the root folder.
user_rootFolderId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_rootFolderId = Lens.lens (\User' {rootFolderId} -> rootFolderId) (\s@User' {} a -> s {rootFolderId = a} :: User)

-- | The time when the user was modified.
user_modifiedTimestamp :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_modifiedTimestamp = Lens.lens (\User' {modifiedTimestamp} -> modifiedTimestamp) (\s@User' {} a -> s {modifiedTimestamp = a} :: User) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Core..:? "RecycleBinFolderId")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "TimeZoneId")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "Locale")
            Prelude.<*> (x Core..:? "GivenName")
            Prelude.<*> (x Core..:? "Storage")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Surname")
            Prelude.<*> (x Core..:? "OrganizationId")
            Prelude.<*> (x Core..:? "EmailAddress")
            Prelude.<*> (x Core..:? "RootFolderId")
            Prelude.<*> (x Core..:? "ModifiedTimestamp")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` recycleBinFolderId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` timeZoneId
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` givenName
      `Prelude.hashWithSalt` storage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` surname
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` rootFolderId
      `Prelude.hashWithSalt` modifiedTimestamp

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf recycleBinFolderId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf givenName
      `Prelude.seq` Prelude.rnf storage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf surname
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf rootFolderId
      `Prelude.seq` Prelude.rnf modifiedTimestamp
