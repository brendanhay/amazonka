{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserMetadata
  ( UserMetadata (..),

    -- * Smart constructor
    mkUserMetadata,

    -- * Lenses
    umEmailAddress,
    umGivenName,
    umId,
    umSurname,
    umUsername,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.EmailAddressType as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.UserAttributeValueType as Types
import qualified Network.AWS.WorkDocs.Types.Username as Types

-- | Describes the metadata of the user.
--
-- /See:/ 'mkUserMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { -- | The email address of the user.
    emailAddress :: Core.Maybe Types.EmailAddressType,
    -- | The given name of the user before a rename operation.
    givenName :: Core.Maybe Types.UserAttributeValueType,
    -- | The ID of the user.
    id :: Core.Maybe Types.IdType,
    -- | The surname of the user.
    surname :: Core.Maybe Types.UserAttributeValueType,
    -- | The name of the user.
    username :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserMetadata' value with any optional fields omitted.
mkUserMetadata ::
  UserMetadata
mkUserMetadata =
  UserMetadata'
    { emailAddress = Core.Nothing,
      givenName = Core.Nothing,
      id = Core.Nothing,
      surname = Core.Nothing,
      username = Core.Nothing
    }

-- | The email address of the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umEmailAddress :: Lens.Lens' UserMetadata (Core.Maybe Types.EmailAddressType)
umEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED umEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The given name of the user before a rename operation.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umGivenName :: Lens.Lens' UserMetadata (Core.Maybe Types.UserAttributeValueType)
umGivenName = Lens.field @"givenName"
{-# DEPRECATED umGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umId :: Lens.Lens' UserMetadata (Core.Maybe Types.IdType)
umId = Lens.field @"id"
{-# DEPRECATED umId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umSurname :: Lens.Lens' UserMetadata (Core.Maybe Types.UserAttributeValueType)
umSurname = Lens.field @"surname"
{-# DEPRECATED umSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umUsername :: Lens.Lens' UserMetadata (Core.Maybe Types.Username)
umUsername = Lens.field @"username"
{-# DEPRECATED umUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON UserMetadata where
  parseJSON =
    Core.withObject "UserMetadata" Core.$
      \x ->
        UserMetadata'
          Core.<$> (x Core..:? "EmailAddress")
          Core.<*> (x Core..:? "GivenName")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Surname")
          Core.<*> (x Core..:? "Username")
