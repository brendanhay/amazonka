{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType where

import Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration for creating a new user profile.
--
--
--
-- /See:/ 'adminCreateUserConfigType' smart constructor.
data AdminCreateUserConfigType = AdminCreateUserConfigType'
  { _acuctAllowAdminCreateUserOnly ::
      !(Maybe Bool),
    _acuctUnusedAccountValidityDays ::
      !(Maybe Nat),
    _acuctInviteMessageTemplate ::
      !(Maybe MessageTemplateType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminCreateUserConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acuctAllowAdminCreateUserOnly' - Set to @True@ if only the administrator is allowed to create user profiles. Set to @False@ if users can sign themselves up via an app.
--
-- * 'acuctUnusedAccountValidityDays' - The user account expiration limit, in days, after which the account is no longer usable. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter. The default value for this parameter is 7.
--
-- * 'acuctInviteMessageTemplate' - The message template to be used for the welcome message to new users. See also <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages> .
adminCreateUserConfigType ::
  AdminCreateUserConfigType
adminCreateUserConfigType =
  AdminCreateUserConfigType'
    { _acuctAllowAdminCreateUserOnly =
        Nothing,
      _acuctUnusedAccountValidityDays = Nothing,
      _acuctInviteMessageTemplate = Nothing
    }

-- | Set to @True@ if only the administrator is allowed to create user profiles. Set to @False@ if users can sign themselves up via an app.
acuctAllowAdminCreateUserOnly :: Lens' AdminCreateUserConfigType (Maybe Bool)
acuctAllowAdminCreateUserOnly = lens _acuctAllowAdminCreateUserOnly (\s a -> s {_acuctAllowAdminCreateUserOnly = a})

-- | The user account expiration limit, in days, after which the account is no longer usable. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter. The default value for this parameter is 7.
acuctUnusedAccountValidityDays :: Lens' AdminCreateUserConfigType (Maybe Natural)
acuctUnusedAccountValidityDays = lens _acuctUnusedAccountValidityDays (\s a -> s {_acuctUnusedAccountValidityDays = a}) . mapping _Nat

-- | The message template to be used for the welcome message to new users. See also <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages> .
acuctInviteMessageTemplate :: Lens' AdminCreateUserConfigType (Maybe MessageTemplateType)
acuctInviteMessageTemplate = lens _acuctInviteMessageTemplate (\s a -> s {_acuctInviteMessageTemplate = a})

instance FromJSON AdminCreateUserConfigType where
  parseJSON =
    withObject
      "AdminCreateUserConfigType"
      ( \x ->
          AdminCreateUserConfigType'
            <$> (x .:? "AllowAdminCreateUserOnly")
            <*> (x .:? "UnusedAccountValidityDays")
            <*> (x .:? "InviteMessageTemplate")
      )

instance Hashable AdminCreateUserConfigType

instance NFData AdminCreateUserConfigType

instance ToJSON AdminCreateUserConfigType where
  toJSON AdminCreateUserConfigType' {..} =
    object
      ( catMaybes
          [ ("AllowAdminCreateUserOnly" .=)
              <$> _acuctAllowAdminCreateUserOnly,
            ("UnusedAccountValidityDays" .=)
              <$> _acuctUnusedAccountValidityDays,
            ("InviteMessageTemplate" .=) <$> _acuctInviteMessageTemplate
          ]
      )
