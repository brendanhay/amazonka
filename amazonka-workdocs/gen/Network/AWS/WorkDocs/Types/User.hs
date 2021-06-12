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
import Network.AWS.WorkDocs.Types.LocaleType
import Network.AWS.WorkDocs.Types.UserStatusType
import Network.AWS.WorkDocs.Types.UserStorageMetadata
import Network.AWS.WorkDocs.Types.UserType

-- | Describes a user.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The time when the user was modified.
    modifiedTimestamp :: Core.Maybe Core.POSIX,
    -- | The status of the user.
    status :: Core.Maybe UserStatusType,
    -- | The ID of the organization.
    organizationId :: Core.Maybe Core.Text,
    -- | The time when the user was created.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The time zone ID of the user.
    timeZoneId :: Core.Maybe Core.Text,
    -- | The surname of the user.
    surname :: Core.Maybe Core.Text,
    -- | The locale of the user.
    locale :: Core.Maybe LocaleType,
    -- | The ID of the user.
    id :: Core.Maybe Core.Text,
    -- | The ID of the root folder.
    rootFolderId :: Core.Maybe Core.Text,
    -- | The given name of the user.
    givenName :: Core.Maybe Core.Text,
    -- | The ID of the recycle bin folder.
    recycleBinFolderId :: Core.Maybe Core.Text,
    -- | The storage for the user.
    storage :: Core.Maybe UserStorageMetadata,
    -- | The login name of the user.
    username :: Core.Maybe Core.Text,
    -- | The type of user.
    type' :: Core.Maybe UserType,
    -- | The email address of the user.
    emailAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modifiedTimestamp', 'user_modifiedTimestamp' - The time when the user was modified.
--
-- 'status', 'user_status' - The status of the user.
--
-- 'organizationId', 'user_organizationId' - The ID of the organization.
--
-- 'createdTimestamp', 'user_createdTimestamp' - The time when the user was created.
--
-- 'timeZoneId', 'user_timeZoneId' - The time zone ID of the user.
--
-- 'surname', 'user_surname' - The surname of the user.
--
-- 'locale', 'user_locale' - The locale of the user.
--
-- 'id', 'user_id' - The ID of the user.
--
-- 'rootFolderId', 'user_rootFolderId' - The ID of the root folder.
--
-- 'givenName', 'user_givenName' - The given name of the user.
--
-- 'recycleBinFolderId', 'user_recycleBinFolderId' - The ID of the recycle bin folder.
--
-- 'storage', 'user_storage' - The storage for the user.
--
-- 'username', 'user_username' - The login name of the user.
--
-- 'type'', 'user_type' - The type of user.
--
-- 'emailAddress', 'user_emailAddress' - The email address of the user.
newUser ::
  User
newUser =
  User'
    { modifiedTimestamp = Core.Nothing,
      status = Core.Nothing,
      organizationId = Core.Nothing,
      createdTimestamp = Core.Nothing,
      timeZoneId = Core.Nothing,
      surname = Core.Nothing,
      locale = Core.Nothing,
      id = Core.Nothing,
      rootFolderId = Core.Nothing,
      givenName = Core.Nothing,
      recycleBinFolderId = Core.Nothing,
      storage = Core.Nothing,
      username = Core.Nothing,
      type' = Core.Nothing,
      emailAddress = Core.Nothing
    }

-- | The time when the user was modified.
user_modifiedTimestamp :: Lens.Lens' User (Core.Maybe Core.UTCTime)
user_modifiedTimestamp = Lens.lens (\User' {modifiedTimestamp} -> modifiedTimestamp) (\s@User' {} a -> s {modifiedTimestamp = a} :: User) Core.. Lens.mapping Core._Time

-- | The status of the user.
user_status :: Lens.Lens' User (Core.Maybe UserStatusType)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The ID of the organization.
user_organizationId :: Lens.Lens' User (Core.Maybe Core.Text)
user_organizationId = Lens.lens (\User' {organizationId} -> organizationId) (\s@User' {} a -> s {organizationId = a} :: User)

-- | The time when the user was created.
user_createdTimestamp :: Lens.Lens' User (Core.Maybe Core.UTCTime)
user_createdTimestamp = Lens.lens (\User' {createdTimestamp} -> createdTimestamp) (\s@User' {} a -> s {createdTimestamp = a} :: User) Core.. Lens.mapping Core._Time

-- | The time zone ID of the user.
user_timeZoneId :: Lens.Lens' User (Core.Maybe Core.Text)
user_timeZoneId = Lens.lens (\User' {timeZoneId} -> timeZoneId) (\s@User' {} a -> s {timeZoneId = a} :: User)

-- | The surname of the user.
user_surname :: Lens.Lens' User (Core.Maybe Core.Text)
user_surname = Lens.lens (\User' {surname} -> surname) (\s@User' {} a -> s {surname = a} :: User)

-- | The locale of the user.
user_locale :: Lens.Lens' User (Core.Maybe LocaleType)
user_locale = Lens.lens (\User' {locale} -> locale) (\s@User' {} a -> s {locale = a} :: User)

-- | The ID of the user.
user_id :: Lens.Lens' User (Core.Maybe Core.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The ID of the root folder.
user_rootFolderId :: Lens.Lens' User (Core.Maybe Core.Text)
user_rootFolderId = Lens.lens (\User' {rootFolderId} -> rootFolderId) (\s@User' {} a -> s {rootFolderId = a} :: User)

-- | The given name of the user.
user_givenName :: Lens.Lens' User (Core.Maybe Core.Text)
user_givenName = Lens.lens (\User' {givenName} -> givenName) (\s@User' {} a -> s {givenName = a} :: User)

-- | The ID of the recycle bin folder.
user_recycleBinFolderId :: Lens.Lens' User (Core.Maybe Core.Text)
user_recycleBinFolderId = Lens.lens (\User' {recycleBinFolderId} -> recycleBinFolderId) (\s@User' {} a -> s {recycleBinFolderId = a} :: User)

-- | The storage for the user.
user_storage :: Lens.Lens' User (Core.Maybe UserStorageMetadata)
user_storage = Lens.lens (\User' {storage} -> storage) (\s@User' {} a -> s {storage = a} :: User)

-- | The login name of the user.
user_username :: Lens.Lens' User (Core.Maybe Core.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

-- | The type of user.
user_type :: Lens.Lens' User (Core.Maybe UserType)
user_type = Lens.lens (\User' {type'} -> type') (\s@User' {} a -> s {type' = a} :: User)

-- | The email address of the user.
user_emailAddress :: Lens.Lens' User (Core.Maybe Core.Text)
user_emailAddress = Lens.lens (\User' {emailAddress} -> emailAddress) (\s@User' {} a -> s {emailAddress = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Core.<$> (x Core..:? "ModifiedTimestamp")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "OrganizationId")
            Core.<*> (x Core..:? "CreatedTimestamp")
            Core.<*> (x Core..:? "TimeZoneId")
            Core.<*> (x Core..:? "Surname")
            Core.<*> (x Core..:? "Locale")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "RootFolderId")
            Core.<*> (x Core..:? "GivenName")
            Core.<*> (x Core..:? "RecycleBinFolderId")
            Core.<*> (x Core..:? "Storage")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "EmailAddress")
      )

instance Core.Hashable User

instance Core.NFData User
