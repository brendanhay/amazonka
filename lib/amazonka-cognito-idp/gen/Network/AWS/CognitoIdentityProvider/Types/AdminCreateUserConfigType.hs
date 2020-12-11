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
    acuctUnusedAccountValidityDays,
    acuctInviteMessageTemplate,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration for creating a new user profile.
--
-- /See:/ 'mkAdminCreateUserConfigType' smart constructor.
data AdminCreateUserConfigType = AdminCreateUserConfigType'
  { allowAdminCreateUserOnly ::
      Lude.Maybe Lude.Bool,
    unusedAccountValidityDays ::
      Lude.Maybe Lude.Natural,
    inviteMessageTemplate ::
      Lude.Maybe MessageTemplateType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminCreateUserConfigType' with the minimum fields required to make a request.
--
-- * 'allowAdminCreateUserOnly' - Set to @True@ if only the administrator is allowed to create user profiles. Set to @False@ if users can sign themselves up via an app.
-- * 'inviteMessageTemplate' - The message template to be used for the welcome message to new users.
--
-- See also <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages> .
-- * 'unusedAccountValidityDays' - The user account expiration limit, in days, after which the account is no longer usable. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter. The default value for this parameter is 7.
mkAdminCreateUserConfigType ::
  AdminCreateUserConfigType
mkAdminCreateUserConfigType =
  AdminCreateUserConfigType'
    { allowAdminCreateUserOnly =
        Lude.Nothing,
      unusedAccountValidityDays = Lude.Nothing,
      inviteMessageTemplate = Lude.Nothing
    }

-- | Set to @True@ if only the administrator is allowed to create user profiles. Set to @False@ if users can sign themselves up via an app.
--
-- /Note:/ Consider using 'allowAdminCreateUserOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuctAllowAdminCreateUserOnly :: Lens.Lens' AdminCreateUserConfigType (Lude.Maybe Lude.Bool)
acuctAllowAdminCreateUserOnly = Lens.lens (allowAdminCreateUserOnly :: AdminCreateUserConfigType -> Lude.Maybe Lude.Bool) (\s a -> s {allowAdminCreateUserOnly = a} :: AdminCreateUserConfigType)
{-# DEPRECATED acuctAllowAdminCreateUserOnly "Use generic-lens or generic-optics with 'allowAdminCreateUserOnly' instead." #-}

-- | The user account expiration limit, in days, after which the account is no longer usable. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter. The default value for this parameter is 7.
--
-- /Note:/ Consider using 'unusedAccountValidityDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuctUnusedAccountValidityDays :: Lens.Lens' AdminCreateUserConfigType (Lude.Maybe Lude.Natural)
acuctUnusedAccountValidityDays = Lens.lens (unusedAccountValidityDays :: AdminCreateUserConfigType -> Lude.Maybe Lude.Natural) (\s a -> s {unusedAccountValidityDays = a} :: AdminCreateUserConfigType)
{-# DEPRECATED acuctUnusedAccountValidityDays "Use generic-lens or generic-optics with 'unusedAccountValidityDays' instead." #-}

-- | The message template to be used for the welcome message to new users.
--
-- See also <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages> .
--
-- /Note:/ Consider using 'inviteMessageTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuctInviteMessageTemplate :: Lens.Lens' AdminCreateUserConfigType (Lude.Maybe MessageTemplateType)
acuctInviteMessageTemplate = Lens.lens (inviteMessageTemplate :: AdminCreateUserConfigType -> Lude.Maybe MessageTemplateType) (\s a -> s {inviteMessageTemplate = a} :: AdminCreateUserConfigType)
{-# DEPRECATED acuctInviteMessageTemplate "Use generic-lens or generic-optics with 'inviteMessageTemplate' instead." #-}

instance Lude.FromJSON AdminCreateUserConfigType where
  parseJSON =
    Lude.withObject
      "AdminCreateUserConfigType"
      ( \x ->
          AdminCreateUserConfigType'
            Lude.<$> (x Lude..:? "AllowAdminCreateUserOnly")
            Lude.<*> (x Lude..:? "UnusedAccountValidityDays")
            Lude.<*> (x Lude..:? "InviteMessageTemplate")
      )

instance Lude.ToJSON AdminCreateUserConfigType where
  toJSON AdminCreateUserConfigType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AllowAdminCreateUserOnly" Lude..=)
              Lude.<$> allowAdminCreateUserOnly,
            ("UnusedAccountValidityDays" Lude..=)
              Lude.<$> unusedAccountValidityDays,
            ("InviteMessageTemplate" Lude..=) Lude.<$> inviteMessageTemplate
          ]
      )
