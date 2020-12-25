{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
  ( AdminCreateUserConfigType (..),

    -- * Smart constructor
    mkAdminCreateUserConfigType,

    -- * Lenses
    acuctAllowAdminCreateUserOnly,
    acuctInviteMessageTemplate,
    acuctUnusedAccountValidityDays,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration for creating a new user profile.
--
-- /See:/ 'mkAdminCreateUserConfigType' smart constructor.
data AdminCreateUserConfigType = AdminCreateUserConfigType'
  { -- | Set to @True@ if only the administrator is allowed to create user profiles. Set to @False@ if users can sign themselves up via an app.
    allowAdminCreateUserOnly :: Core.Maybe Core.Bool,
    -- | The message template to be used for the welcome message to new users.
    --
    -- See also <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages> .
    inviteMessageTemplate :: Core.Maybe Types.MessageTemplateType,
    -- | The user account expiration limit, in days, after which the account is no longer usable. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter. The default value for this parameter is 7.
    unusedAccountValidityDays :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminCreateUserConfigType' value with any optional fields omitted.
mkAdminCreateUserConfigType ::
  AdminCreateUserConfigType
mkAdminCreateUserConfigType =
  AdminCreateUserConfigType'
    { allowAdminCreateUserOnly =
        Core.Nothing,
      inviteMessageTemplate = Core.Nothing,
      unusedAccountValidityDays = Core.Nothing
    }

-- | Set to @True@ if only the administrator is allowed to create user profiles. Set to @False@ if users can sign themselves up via an app.
--
-- /Note:/ Consider using 'allowAdminCreateUserOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuctAllowAdminCreateUserOnly :: Lens.Lens' AdminCreateUserConfigType (Core.Maybe Core.Bool)
acuctAllowAdminCreateUserOnly = Lens.field @"allowAdminCreateUserOnly"
{-# DEPRECATED acuctAllowAdminCreateUserOnly "Use generic-lens or generic-optics with 'allowAdminCreateUserOnly' instead." #-}

-- | The message template to be used for the welcome message to new users.
--
-- See also <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages> .
--
-- /Note:/ Consider using 'inviteMessageTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuctInviteMessageTemplate :: Lens.Lens' AdminCreateUserConfigType (Core.Maybe Types.MessageTemplateType)
acuctInviteMessageTemplate = Lens.field @"inviteMessageTemplate"
{-# DEPRECATED acuctInviteMessageTemplate "Use generic-lens or generic-optics with 'inviteMessageTemplate' instead." #-}

-- | The user account expiration limit, in days, after which the account is no longer usable. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter. The default value for this parameter is 7.
--
-- /Note:/ Consider using 'unusedAccountValidityDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuctUnusedAccountValidityDays :: Lens.Lens' AdminCreateUserConfigType (Core.Maybe Core.Natural)
acuctUnusedAccountValidityDays = Lens.field @"unusedAccountValidityDays"
{-# DEPRECATED acuctUnusedAccountValidityDays "Use generic-lens or generic-optics with 'unusedAccountValidityDays' instead." #-}

instance Core.FromJSON AdminCreateUserConfigType where
  toJSON AdminCreateUserConfigType {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowAdminCreateUserOnly" Core..=)
              Core.<$> allowAdminCreateUserOnly,
            ("InviteMessageTemplate" Core..=) Core.<$> inviteMessageTemplate,
            ("UnusedAccountValidityDays" Core..=)
              Core.<$> unusedAccountValidityDays
          ]
      )

instance Core.FromJSON AdminCreateUserConfigType where
  parseJSON =
    Core.withObject "AdminCreateUserConfigType" Core.$
      \x ->
        AdminCreateUserConfigType'
          Core.<$> (x Core..:? "AllowAdminCreateUserOnly")
          Core.<*> (x Core..:? "InviteMessageTemplate")
          Core.<*> (x Core..:? "UnusedAccountValidityDays")
