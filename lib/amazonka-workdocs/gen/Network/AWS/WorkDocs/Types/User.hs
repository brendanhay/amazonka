{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    uCreatedTimestamp,
    uEmailAddress,
    uGivenName,
    uId,
    uLocale,
    uModifiedTimestamp,
    uOrganizationId,
    uRecycleBinFolderId,
    uRootFolderId,
    uStatus,
    uStorage,
    uSurname,
    uTimeZoneId,
    uType,
    uUsername,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.EmailAddressType as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.LocaleType as Types
import qualified Network.AWS.WorkDocs.Types.RecycleBinFolderId as Types
import qualified Network.AWS.WorkDocs.Types.RootFolderId as Types
import qualified Network.AWS.WorkDocs.Types.TimeZoneIdType as Types
import qualified Network.AWS.WorkDocs.Types.UserAttributeValueType as Types
import qualified Network.AWS.WorkDocs.Types.UserStatusType as Types
import qualified Network.AWS.WorkDocs.Types.UserStorageMetadata as Types
import qualified Network.AWS.WorkDocs.Types.UserType as Types
import qualified Network.AWS.WorkDocs.Types.Username as Types

-- | Describes a user.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | The time when the user was created.
    createdTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The email address of the user.
    emailAddress :: Core.Maybe Types.EmailAddressType,
    -- | The given name of the user.
    givenName :: Core.Maybe Types.UserAttributeValueType,
    -- | The ID of the user.
    id :: Core.Maybe Types.IdType,
    -- | The locale of the user.
    locale :: Core.Maybe Types.LocaleType,
    -- | The time when the user was modified.
    modifiedTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the organization.
    organizationId :: Core.Maybe Types.IdType,
    -- | The ID of the recycle bin folder.
    recycleBinFolderId :: Core.Maybe Types.RecycleBinFolderId,
    -- | The ID of the root folder.
    rootFolderId :: Core.Maybe Types.RootFolderId,
    -- | The status of the user.
    status :: Core.Maybe Types.UserStatusType,
    -- | The storage for the user.
    storage :: Core.Maybe Types.UserStorageMetadata,
    -- | The surname of the user.
    surname :: Core.Maybe Types.UserAttributeValueType,
    -- | The time zone ID of the user.
    timeZoneId :: Core.Maybe Types.TimeZoneIdType,
    -- | The type of user.
    type' :: Core.Maybe Types.UserType,
    -- | The login name of the user.
    username :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser ::
  User
mkUser =
  User'
    { createdTimestamp = Core.Nothing,
      emailAddress = Core.Nothing,
      givenName = Core.Nothing,
      id = Core.Nothing,
      locale = Core.Nothing,
      modifiedTimestamp = Core.Nothing,
      organizationId = Core.Nothing,
      recycleBinFolderId = Core.Nothing,
      rootFolderId = Core.Nothing,
      status = Core.Nothing,
      storage = Core.Nothing,
      surname = Core.Nothing,
      timeZoneId = Core.Nothing,
      type' = Core.Nothing,
      username = Core.Nothing
    }

-- | The time when the user was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreatedTimestamp :: Lens.Lens' User (Core.Maybe Core.NominalDiffTime)
uCreatedTimestamp = Lens.field @"createdTimestamp"
{-# DEPRECATED uCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEmailAddress :: Lens.Lens' User (Core.Maybe Types.EmailAddressType)
uEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED uEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The given name of the user.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uGivenName :: Lens.Lens' User (Core.Maybe Types.UserAttributeValueType)
uGivenName = Lens.field @"givenName"
{-# DEPRECATED uGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' User (Core.Maybe Types.IdType)
uId = Lens.field @"id"
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The locale of the user.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLocale :: Lens.Lens' User (Core.Maybe Types.LocaleType)
uLocale = Lens.field @"locale"
{-# DEPRECATED uLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The time when the user was modified.
--
-- /Note:/ Consider using 'modifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uModifiedTimestamp :: Lens.Lens' User (Core.Maybe Core.NominalDiffTime)
uModifiedTimestamp = Lens.field @"modifiedTimestamp"
{-# DEPRECATED uModifiedTimestamp "Use generic-lens or generic-optics with 'modifiedTimestamp' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uOrganizationId :: Lens.Lens' User (Core.Maybe Types.IdType)
uOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED uOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The ID of the recycle bin folder.
--
-- /Note:/ Consider using 'recycleBinFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRecycleBinFolderId :: Lens.Lens' User (Core.Maybe Types.RecycleBinFolderId)
uRecycleBinFolderId = Lens.field @"recycleBinFolderId"
{-# DEPRECATED uRecycleBinFolderId "Use generic-lens or generic-optics with 'recycleBinFolderId' instead." #-}

-- | The ID of the root folder.
--
-- /Note:/ Consider using 'rootFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRootFolderId :: Lens.Lens' User (Core.Maybe Types.RootFolderId)
uRootFolderId = Lens.field @"rootFolderId"
{-# DEPRECATED uRootFolderId "Use generic-lens or generic-optics with 'rootFolderId' instead." #-}

-- | The status of the user.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' User (Core.Maybe Types.UserStatusType)
uStatus = Lens.field @"status"
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The storage for the user.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStorage :: Lens.Lens' User (Core.Maybe Types.UserStorageMetadata)
uStorage = Lens.field @"storage"
{-# DEPRECATED uStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSurname :: Lens.Lens' User (Core.Maybe Types.UserAttributeValueType)
uSurname = Lens.field @"surname"
{-# DEPRECATED uSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | The time zone ID of the user.
--
-- /Note:/ Consider using 'timeZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTimeZoneId :: Lens.Lens' User (Core.Maybe Types.TimeZoneIdType)
uTimeZoneId = Lens.field @"timeZoneId"
{-# DEPRECATED uTimeZoneId "Use generic-lens or generic-optics with 'timeZoneId' instead." #-}

-- | The type of user.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uType :: Lens.Lens' User (Core.Maybe Types.UserType)
uType = Lens.field @"type'"
{-# DEPRECATED uType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The login name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsername :: Lens.Lens' User (Core.Maybe Types.Username)
uUsername = Lens.field @"username"
{-# DEPRECATED uUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON User where
  parseJSON =
    Core.withObject "User" Core.$
      \x ->
        User'
          Core.<$> (x Core..:? "CreatedTimestamp")
          Core.<*> (x Core..:? "EmailAddress")
          Core.<*> (x Core..:? "GivenName")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Locale")
          Core.<*> (x Core..:? "ModifiedTimestamp")
          Core.<*> (x Core..:? "OrganizationId")
          Core.<*> (x Core..:? "RecycleBinFolderId")
          Core.<*> (x Core..:? "RootFolderId")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Storage")
          Core.<*> (x Core..:? "Surname")
          Core.<*> (x Core..:? "TimeZoneId")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "Username")
