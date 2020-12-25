{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserIdentityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserIdentityInfo
  ( UserIdentityInfo (..),

    -- * Smart constructor
    mkUserIdentityInfo,

    -- * Lenses
    uiiEmail,
    uiiFirstName,
    uiiLastName,
  )
where

import qualified Network.AWS.Connect.Types.AgentFirstName as Types
import qualified Network.AWS.Connect.Types.AgentLastName as Types
import qualified Network.AWS.Connect.Types.Email as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the identity of a user.
--
-- /See:/ 'mkUserIdentityInfo' smart constructor.
data UserIdentityInfo = UserIdentityInfo'
  { -- | The email address. If you are using SAML for identity management and include this parameter, an error is returned.
    email :: Core.Maybe Types.Email,
    -- | The first name. This is required if you are using Amazon Connect or SAML for identity management.
    firstName :: Core.Maybe Types.AgentFirstName,
    -- | The last name. This is required if you are using Amazon Connect or SAML for identity management.
    lastName :: Core.Maybe Types.AgentLastName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserIdentityInfo' value with any optional fields omitted.
mkUserIdentityInfo ::
  UserIdentityInfo
mkUserIdentityInfo =
  UserIdentityInfo'
    { email = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing
    }

-- | The email address. If you are using SAML for identity management and include this parameter, an error is returned.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiEmail :: Lens.Lens' UserIdentityInfo (Core.Maybe Types.Email)
uiiEmail = Lens.field @"email"
{-# DEPRECATED uiiEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The first name. This is required if you are using Amazon Connect or SAML for identity management.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiFirstName :: Lens.Lens' UserIdentityInfo (Core.Maybe Types.AgentFirstName)
uiiFirstName = Lens.field @"firstName"
{-# DEPRECATED uiiFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The last name. This is required if you are using Amazon Connect or SAML for identity management.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiLastName :: Lens.Lens' UserIdentityInfo (Core.Maybe Types.AgentLastName)
uiiLastName = Lens.field @"lastName"
{-# DEPRECATED uiiLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

instance Core.FromJSON UserIdentityInfo where
  toJSON UserIdentityInfo {..} =
    Core.object
      ( Core.catMaybes
          [ ("Email" Core..=) Core.<$> email,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName
          ]
      )

instance Core.FromJSON UserIdentityInfo where
  parseJSON =
    Core.withObject "UserIdentityInfo" Core.$
      \x ->
        UserIdentityInfo'
          Core.<$> (x Core..:? "Email")
          Core.<*> (x Core..:? "FirstName")
          Core.<*> (x Core..:? "LastName")
