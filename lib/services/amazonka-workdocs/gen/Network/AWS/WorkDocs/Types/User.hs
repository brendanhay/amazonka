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
-- Module      : Network.AWS.WorkDocs.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.User where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.LocaleType
import Network.AWS.WorkDocs.Types.UserStatusType
import Network.AWS.WorkDocs.Types.UserStorageMetadata
import Network.AWS.WorkDocs.Types.UserType

-- | Describes a user.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The given name of the user.
    givenName :: Prelude.Maybe Prelude.Text,
    -- | The status of the user.
    status :: Prelude.Maybe UserStatusType,
    -- | The locale of the user.
    locale :: Prelude.Maybe LocaleType,
    -- | The login name of the user.
    username :: Prelude.Maybe Prelude.Text,
    -- | The storage for the user.
    storage :: Prelude.Maybe UserStorageMetadata,
    -- | The time when the user was modified.
    modifiedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The email address of the user.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the root folder.
    rootFolderId :: Prelude.Maybe Prelude.Text,
    -- | The type of user.
    type' :: Prelude.Maybe UserType,
    -- | The surname of the user.
    surname :: Prelude.Maybe Prelude.Text,
    -- | The time zone ID of the user.
    timeZoneId :: Prelude.Maybe Prelude.Text,
    -- | The time when the user was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the recycle bin folder.
    recycleBinFolderId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'givenName', 'user_givenName' - The given name of the user.
--
-- 'status', 'user_status' - The status of the user.
--
-- 'locale', 'user_locale' - The locale of the user.
--
-- 'username', 'user_username' - The login name of the user.
--
-- 'storage', 'user_storage' - The storage for the user.
--
-- 'modifiedTimestamp', 'user_modifiedTimestamp' - The time when the user was modified.
--
-- 'emailAddress', 'user_emailAddress' - The email address of the user.
--
-- 'id', 'user_id' - The ID of the user.
--
-- 'rootFolderId', 'user_rootFolderId' - The ID of the root folder.
--
-- 'type'', 'user_type' - The type of user.
--
-- 'surname', 'user_surname' - The surname of the user.
--
-- 'timeZoneId', 'user_timeZoneId' - The time zone ID of the user.
--
-- 'createdTimestamp', 'user_createdTimestamp' - The time when the user was created.
--
-- 'organizationId', 'user_organizationId' - The ID of the organization.
--
-- 'recycleBinFolderId', 'user_recycleBinFolderId' - The ID of the recycle bin folder.
newUser ::
  User
newUser =
  User'
    { givenName = Prelude.Nothing,
      status = Prelude.Nothing,
      locale = Prelude.Nothing,
      username = Prelude.Nothing,
      storage = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      id = Prelude.Nothing,
      rootFolderId = Prelude.Nothing,
      type' = Prelude.Nothing,
      surname = Prelude.Nothing,
      timeZoneId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      recycleBinFolderId = Prelude.Nothing
    }

-- | The given name of the user.
user_givenName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_givenName = Lens.lens (\User' {givenName} -> givenName) (\s@User' {} a -> s {givenName = a} :: User)

-- | The status of the user.
user_status :: Lens.Lens' User (Prelude.Maybe UserStatusType)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The locale of the user.
user_locale :: Lens.Lens' User (Prelude.Maybe LocaleType)
user_locale = Lens.lens (\User' {locale} -> locale) (\s@User' {} a -> s {locale = a} :: User)

-- | The login name of the user.
user_username :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

-- | The storage for the user.
user_storage :: Lens.Lens' User (Prelude.Maybe UserStorageMetadata)
user_storage = Lens.lens (\User' {storage} -> storage) (\s@User' {} a -> s {storage = a} :: User)

-- | The time when the user was modified.
user_modifiedTimestamp :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_modifiedTimestamp = Lens.lens (\User' {modifiedTimestamp} -> modifiedTimestamp) (\s@User' {} a -> s {modifiedTimestamp = a} :: User) Prelude.. Lens.mapping Core._Time

-- | The email address of the user.
user_emailAddress :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_emailAddress = Lens.lens (\User' {emailAddress} -> emailAddress) (\s@User' {} a -> s {emailAddress = a} :: User)

-- | The ID of the user.
user_id :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The ID of the root folder.
user_rootFolderId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_rootFolderId = Lens.lens (\User' {rootFolderId} -> rootFolderId) (\s@User' {} a -> s {rootFolderId = a} :: User)

-- | The type of user.
user_type :: Lens.Lens' User (Prelude.Maybe UserType)
user_type = Lens.lens (\User' {type'} -> type') (\s@User' {} a -> s {type' = a} :: User)

-- | The surname of the user.
user_surname :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_surname = Lens.lens (\User' {surname} -> surname) (\s@User' {} a -> s {surname = a} :: User)

-- | The time zone ID of the user.
user_timeZoneId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_timeZoneId = Lens.lens (\User' {timeZoneId} -> timeZoneId) (\s@User' {} a -> s {timeZoneId = a} :: User)

-- | The time when the user was created.
user_createdTimestamp :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_createdTimestamp = Lens.lens (\User' {createdTimestamp} -> createdTimestamp) (\s@User' {} a -> s {createdTimestamp = a} :: User) Prelude.. Lens.mapping Core._Time

-- | The ID of the organization.
user_organizationId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_organizationId = Lens.lens (\User' {organizationId} -> organizationId) (\s@User' {} a -> s {organizationId = a} :: User)

-- | The ID of the recycle bin folder.
user_recycleBinFolderId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_recycleBinFolderId = Lens.lens (\User' {recycleBinFolderId} -> recycleBinFolderId) (\s@User' {} a -> s {recycleBinFolderId = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Core..:? "GivenName")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Locale")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "Storage")
            Prelude.<*> (x Core..:? "ModifiedTimestamp")
            Prelude.<*> (x Core..:? "EmailAddress")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "RootFolderId")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Surname")
            Prelude.<*> (x Core..:? "TimeZoneId")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "OrganizationId")
            Prelude.<*> (x Core..:? "RecycleBinFolderId")
      )

instance Prelude.Hashable User

instance Prelude.NFData User
