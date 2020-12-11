-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uGivenName,
    uStatus,
    uLocale,
    uUsername,
    uStorage,
    uModifiedTimestamp,
    uEmailAddress,
    uId,
    uRootFolderId,
    uType,
    uSurname,
    uTimeZoneId,
    uCreatedTimestamp,
    uOrganizationId,
    uRecycleBinFolderId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.LocaleType
import Network.AWS.WorkDocs.Types.UserStatusType
import Network.AWS.WorkDocs.Types.UserStorageMetadata
import Network.AWS.WorkDocs.Types.UserType

-- | Describes a user.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { givenName :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe UserStatusType,
    locale :: Lude.Maybe LocaleType,
    username :: Lude.Maybe Lude.Text,
    storage :: Lude.Maybe UserStorageMetadata,
    modifiedTimestamp :: Lude.Maybe Lude.Timestamp,
    emailAddress :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    rootFolderId :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe UserType,
    surname :: Lude.Maybe Lude.Text,
    timeZoneId :: Lude.Maybe Lude.Text,
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    organizationId :: Lude.Maybe Lude.Text,
    recycleBinFolderId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- * 'createdTimestamp' - The time when the user was created.
-- * 'emailAddress' - The email address of the user.
-- * 'givenName' - The given name of the user.
-- * 'id' - The ID of the user.
-- * 'locale' - The locale of the user.
-- * 'modifiedTimestamp' - The time when the user was modified.
-- * 'organizationId' - The ID of the organization.
-- * 'recycleBinFolderId' - The ID of the recycle bin folder.
-- * 'rootFolderId' - The ID of the root folder.
-- * 'status' - The status of the user.
-- * 'storage' - The storage for the user.
-- * 'surname' - The surname of the user.
-- * 'timeZoneId' - The time zone ID of the user.
-- * 'type'' - The type of user.
-- * 'username' - The login name of the user.
mkUser ::
  User
mkUser =
  User'
    { givenName = Lude.Nothing,
      status = Lude.Nothing,
      locale = Lude.Nothing,
      username = Lude.Nothing,
      storage = Lude.Nothing,
      modifiedTimestamp = Lude.Nothing,
      emailAddress = Lude.Nothing,
      id = Lude.Nothing,
      rootFolderId = Lude.Nothing,
      type' = Lude.Nothing,
      surname = Lude.Nothing,
      timeZoneId = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      organizationId = Lude.Nothing,
      recycleBinFolderId = Lude.Nothing
    }

-- | The given name of the user.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uGivenName :: Lens.Lens' User (Lude.Maybe Lude.Text)
uGivenName = Lens.lens (givenName :: User -> Lude.Maybe Lude.Text) (\s a -> s {givenName = a} :: User)
{-# DEPRECATED uGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | The status of the user.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' User (Lude.Maybe UserStatusType)
uStatus = Lens.lens (status :: User -> Lude.Maybe UserStatusType) (\s a -> s {status = a} :: User)
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The locale of the user.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLocale :: Lens.Lens' User (Lude.Maybe LocaleType)
uLocale = Lens.lens (locale :: User -> Lude.Maybe LocaleType) (\s a -> s {locale = a} :: User)
{-# DEPRECATED uLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The login name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsername :: Lens.Lens' User (Lude.Maybe Lude.Text)
uUsername = Lens.lens (username :: User -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: User)
{-# DEPRECATED uUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The storage for the user.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStorage :: Lens.Lens' User (Lude.Maybe UserStorageMetadata)
uStorage = Lens.lens (storage :: User -> Lude.Maybe UserStorageMetadata) (\s a -> s {storage = a} :: User)
{-# DEPRECATED uStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | The time when the user was modified.
--
-- /Note:/ Consider using 'modifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uModifiedTimestamp :: Lens.Lens' User (Lude.Maybe Lude.Timestamp)
uModifiedTimestamp = Lens.lens (modifiedTimestamp :: User -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedTimestamp = a} :: User)
{-# DEPRECATED uModifiedTimestamp "Use generic-lens or generic-optics with 'modifiedTimestamp' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEmailAddress :: Lens.Lens' User (Lude.Maybe Lude.Text)
uEmailAddress = Lens.lens (emailAddress :: User -> Lude.Maybe Lude.Text) (\s a -> s {emailAddress = a} :: User)
{-# DEPRECATED uEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uId = Lens.lens (id :: User -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: User)
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the root folder.
--
-- /Note:/ Consider using 'rootFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRootFolderId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uRootFolderId = Lens.lens (rootFolderId :: User -> Lude.Maybe Lude.Text) (\s a -> s {rootFolderId = a} :: User)
{-# DEPRECATED uRootFolderId "Use generic-lens or generic-optics with 'rootFolderId' instead." #-}

-- | The type of user.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uType :: Lens.Lens' User (Lude.Maybe UserType)
uType = Lens.lens (type' :: User -> Lude.Maybe UserType) (\s a -> s {type' = a} :: User)
{-# DEPRECATED uType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSurname :: Lens.Lens' User (Lude.Maybe Lude.Text)
uSurname = Lens.lens (surname :: User -> Lude.Maybe Lude.Text) (\s a -> s {surname = a} :: User)
{-# DEPRECATED uSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | The time zone ID of the user.
--
-- /Note:/ Consider using 'timeZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTimeZoneId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uTimeZoneId = Lens.lens (timeZoneId :: User -> Lude.Maybe Lude.Text) (\s a -> s {timeZoneId = a} :: User)
{-# DEPRECATED uTimeZoneId "Use generic-lens or generic-optics with 'timeZoneId' instead." #-}

-- | The time when the user was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreatedTimestamp :: Lens.Lens' User (Lude.Maybe Lude.Timestamp)
uCreatedTimestamp = Lens.lens (createdTimestamp :: User -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: User)
{-# DEPRECATED uCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uOrganizationId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uOrganizationId = Lens.lens (organizationId :: User -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: User)
{-# DEPRECATED uOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The ID of the recycle bin folder.
--
-- /Note:/ Consider using 'recycleBinFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRecycleBinFolderId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uRecycleBinFolderId = Lens.lens (recycleBinFolderId :: User -> Lude.Maybe Lude.Text) (\s a -> s {recycleBinFolderId = a} :: User)
{-# DEPRECATED uRecycleBinFolderId "Use generic-lens or generic-optics with 'recycleBinFolderId' instead." #-}

instance Lude.FromJSON User where
  parseJSON =
    Lude.withObject
      "User"
      ( \x ->
          User'
            Lude.<$> (x Lude..:? "GivenName")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Locale")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Storage")
            Lude.<*> (x Lude..:? "ModifiedTimestamp")
            Lude.<*> (x Lude..:? "EmailAddress")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "RootFolderId")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Surname")
            Lude.<*> (x Lude..:? "TimeZoneId")
            Lude.<*> (x Lude..:? "CreatedTimestamp")
            Lude.<*> (x Lude..:? "OrganizationId")
            Lude.<*> (x Lude..:? "RecycleBinFolderId")
      )
