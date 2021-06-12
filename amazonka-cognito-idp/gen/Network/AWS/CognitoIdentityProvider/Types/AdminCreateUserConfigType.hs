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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType where

import Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The configuration for creating a new user profile.
--
-- /See:/ 'newAdminCreateUserConfigType' smart constructor.
data AdminCreateUserConfigType = AdminCreateUserConfigType'
  { -- | Set to @True@ if only the administrator is allowed to create user
    -- profiles. Set to @False@ if users can sign themselves up via an app.
    allowAdminCreateUserOnly :: Core.Maybe Core.Bool,
    -- | The message template to be used for the welcome message to new users.
    --
    -- See also
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages>.
    inviteMessageTemplate :: Core.Maybe MessageTemplateType,
    -- | The user account expiration limit, in days, after which the account is
    -- no longer usable. To reset the account after that time limit, you must
    -- call @AdminCreateUser@ again, specifying @\"RESEND\"@ for the
    -- @MessageAction@ parameter. The default value for this parameter is 7.
    --
    -- If you set a value for @TemporaryPasswordValidityDays@ in
    -- @PasswordPolicy@, that value will be used and
    -- @UnusedAccountValidityDays@ will be deprecated for that user pool.
    unusedAccountValidityDays :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminCreateUserConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowAdminCreateUserOnly', 'adminCreateUserConfigType_allowAdminCreateUserOnly' - Set to @True@ if only the administrator is allowed to create user
-- profiles. Set to @False@ if users can sign themselves up via an app.
--
-- 'inviteMessageTemplate', 'adminCreateUserConfigType_inviteMessageTemplate' - The message template to be used for the welcome message to new users.
--
-- See also
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages>.
--
-- 'unusedAccountValidityDays', 'adminCreateUserConfigType_unusedAccountValidityDays' - The user account expiration limit, in days, after which the account is
-- no longer usable. To reset the account after that time limit, you must
-- call @AdminCreateUser@ again, specifying @\"RESEND\"@ for the
-- @MessageAction@ parameter. The default value for this parameter is 7.
--
-- If you set a value for @TemporaryPasswordValidityDays@ in
-- @PasswordPolicy@, that value will be used and
-- @UnusedAccountValidityDays@ will be deprecated for that user pool.
newAdminCreateUserConfigType ::
  AdminCreateUserConfigType
newAdminCreateUserConfigType =
  AdminCreateUserConfigType'
    { allowAdminCreateUserOnly =
        Core.Nothing,
      inviteMessageTemplate = Core.Nothing,
      unusedAccountValidityDays = Core.Nothing
    }

-- | Set to @True@ if only the administrator is allowed to create user
-- profiles. Set to @False@ if users can sign themselves up via an app.
adminCreateUserConfigType_allowAdminCreateUserOnly :: Lens.Lens' AdminCreateUserConfigType (Core.Maybe Core.Bool)
adminCreateUserConfigType_allowAdminCreateUserOnly = Lens.lens (\AdminCreateUserConfigType' {allowAdminCreateUserOnly} -> allowAdminCreateUserOnly) (\s@AdminCreateUserConfigType' {} a -> s {allowAdminCreateUserOnly = a} :: AdminCreateUserConfigType)

-- | The message template to be used for the welcome message to new users.
--
-- See also
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages>.
adminCreateUserConfigType_inviteMessageTemplate :: Lens.Lens' AdminCreateUserConfigType (Core.Maybe MessageTemplateType)
adminCreateUserConfigType_inviteMessageTemplate = Lens.lens (\AdminCreateUserConfigType' {inviteMessageTemplate} -> inviteMessageTemplate) (\s@AdminCreateUserConfigType' {} a -> s {inviteMessageTemplate = a} :: AdminCreateUserConfigType)

-- | The user account expiration limit, in days, after which the account is
-- no longer usable. To reset the account after that time limit, you must
-- call @AdminCreateUser@ again, specifying @\"RESEND\"@ for the
-- @MessageAction@ parameter. The default value for this parameter is 7.
--
-- If you set a value for @TemporaryPasswordValidityDays@ in
-- @PasswordPolicy@, that value will be used and
-- @UnusedAccountValidityDays@ will be deprecated for that user pool.
adminCreateUserConfigType_unusedAccountValidityDays :: Lens.Lens' AdminCreateUserConfigType (Core.Maybe Core.Natural)
adminCreateUserConfigType_unusedAccountValidityDays = Lens.lens (\AdminCreateUserConfigType' {unusedAccountValidityDays} -> unusedAccountValidityDays) (\s@AdminCreateUserConfigType' {} a -> s {unusedAccountValidityDays = a} :: AdminCreateUserConfigType)

instance Core.FromJSON AdminCreateUserConfigType where
  parseJSON =
    Core.withObject
      "AdminCreateUserConfigType"
      ( \x ->
          AdminCreateUserConfigType'
            Core.<$> (x Core..:? "AllowAdminCreateUserOnly")
            Core.<*> (x Core..:? "InviteMessageTemplate")
            Core.<*> (x Core..:? "UnusedAccountValidityDays")
      )

instance Core.Hashable AdminCreateUserConfigType

instance Core.NFData AdminCreateUserConfigType

instance Core.ToJSON AdminCreateUserConfigType where
  toJSON AdminCreateUserConfigType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowAdminCreateUserOnly" Core..=)
              Core.<$> allowAdminCreateUserOnly,
            ("InviteMessageTemplate" Core..=)
              Core.<$> inviteMessageTemplate,
            ("UnusedAccountValidityDays" Core..=)
              Core.<$> unusedAccountValidityDays
          ]
      )
