{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserType
  ( UserType (..),

    -- * Smart constructor
    mkUserType,

    -- * Lenses
    utEnabled,
    utUserStatus,
    utUsername,
    utUserCreateDate,
    utAttributes,
    utMFAOptions,
    utUserLastModifiedDate,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
import Network.AWS.CognitoIdentityProvider.Types.UserStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user type.
--
-- /See:/ 'mkUserType' smart constructor.
data UserType = UserType'
  { enabled :: Lude.Maybe Lude.Bool,
    userStatus :: Lude.Maybe UserStatusType,
    username :: Lude.Maybe (Lude.Sensitive Lude.Text),
    userCreateDate :: Lude.Maybe Lude.Timestamp,
    attributes :: Lude.Maybe [AttributeType],
    mfaOptions :: Lude.Maybe [MFAOptionType],
    userLastModifiedDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserType' with the minimum fields required to make a request.
--
-- * 'attributes' - A container with information about the user type attributes.
-- * 'enabled' - Specifies whether the user is enabled.
-- * 'mfaOptions' - The MFA options for the user.
-- * 'userCreateDate' - The creation date of the user.
-- * 'userLastModifiedDate' - The last modified date of the user.
-- * 'userStatus' - The user status. Can be one of the following:
--
--
--     * UNCONFIRMED - User has been created but not confirmed.
--
--
--     * CONFIRMED - User has been confirmed.
--
--
--     * ARCHIVED - User is no longer active.
--
--
--     * COMPROMISED - User is disabled due to a potential security threat.
--
--
--     * UNKNOWN - User status is not known.
--
--
--     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.
--
--
--     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else.
--
--
-- * 'username' - The user name of the user you wish to describe.
mkUserType ::
  UserType
mkUserType =
  UserType'
    { enabled = Lude.Nothing,
      userStatus = Lude.Nothing,
      username = Lude.Nothing,
      userCreateDate = Lude.Nothing,
      attributes = Lude.Nothing,
      mfaOptions = Lude.Nothing,
      userLastModifiedDate = Lude.Nothing
    }

-- | Specifies whether the user is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEnabled :: Lens.Lens' UserType (Lude.Maybe Lude.Bool)
utEnabled = Lens.lens (enabled :: UserType -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UserType)
{-# DEPRECATED utEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The user status. Can be one of the following:
--
--
--     * UNCONFIRMED - User has been created but not confirmed.
--
--
--     * CONFIRMED - User has been confirmed.
--
--
--     * ARCHIVED - User is no longer active.
--
--
--     * COMPROMISED - User is disabled due to a potential security threat.
--
--
--     * UNKNOWN - User status is not known.
--
--
--     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.
--
--
--     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else.
--
--
--
-- /Note:/ Consider using 'userStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUserStatus :: Lens.Lens' UserType (Lude.Maybe UserStatusType)
utUserStatus = Lens.lens (userStatus :: UserType -> Lude.Maybe UserStatusType) (\s a -> s {userStatus = a} :: UserType)
{-# DEPRECATED utUserStatus "Use generic-lens or generic-optics with 'userStatus' instead." #-}

-- | The user name of the user you wish to describe.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUsername :: Lens.Lens' UserType (Lude.Maybe (Lude.Sensitive Lude.Text))
utUsername = Lens.lens (username :: UserType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {username = a} :: UserType)
{-# DEPRECATED utUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The creation date of the user.
--
-- /Note:/ Consider using 'userCreateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUserCreateDate :: Lens.Lens' UserType (Lude.Maybe Lude.Timestamp)
utUserCreateDate = Lens.lens (userCreateDate :: UserType -> Lude.Maybe Lude.Timestamp) (\s a -> s {userCreateDate = a} :: UserType)
{-# DEPRECATED utUserCreateDate "Use generic-lens or generic-optics with 'userCreateDate' instead." #-}

-- | A container with information about the user type attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAttributes :: Lens.Lens' UserType (Lude.Maybe [AttributeType])
utAttributes = Lens.lens (attributes :: UserType -> Lude.Maybe [AttributeType]) (\s a -> s {attributes = a} :: UserType)
{-# DEPRECATED utAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The MFA options for the user.
--
-- /Note:/ Consider using 'mfaOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utMFAOptions :: Lens.Lens' UserType (Lude.Maybe [MFAOptionType])
utMFAOptions = Lens.lens (mfaOptions :: UserType -> Lude.Maybe [MFAOptionType]) (\s a -> s {mfaOptions = a} :: UserType)
{-# DEPRECATED utMFAOptions "Use generic-lens or generic-optics with 'mfaOptions' instead." #-}

-- | The last modified date of the user.
--
-- /Note:/ Consider using 'userLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utUserLastModifiedDate :: Lens.Lens' UserType (Lude.Maybe Lude.Timestamp)
utUserLastModifiedDate = Lens.lens (userLastModifiedDate :: UserType -> Lude.Maybe Lude.Timestamp) (\s a -> s {userLastModifiedDate = a} :: UserType)
{-# DEPRECATED utUserLastModifiedDate "Use generic-lens or generic-optics with 'userLastModifiedDate' instead." #-}

instance Lude.FromJSON UserType where
  parseJSON =
    Lude.withObject
      "UserType"
      ( \x ->
          UserType'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "UserStatus")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "UserCreateDate")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MFAOptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UserLastModifiedDate")
      )
