{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uStatus,
    uEnabled,
    uLastName,
    uARN,
    uCreatedTime,
    uUserName,
    uFirstName,
    uAuthenticationType,
  )
where

import Network.AWS.AppStream.Types.AuthenticationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a user in the user pool.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | The status of the user in the user pool. The status can be one of the following:
    --
    --
    --     * UNCONFIRMED – The user is created but not confirmed.
    --
    --
    --     * CONFIRMED – The user is confirmed.
    --
    --
    --     * ARCHIVED – The user is no longer active.
    --
    --
    --     * COMPROMISED – The user is disabled because of a potential security threat.
    --
    --
    --     * UNKNOWN – The user status is not known.
    status :: Lude.Maybe Lude.Text,
    -- | Specifies whether the user in the user pool is enabled.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The last name, or surname, of the user.
    lastName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ARN of the user.
    arn :: Lude.Maybe Lude.Text,
    -- | The date and time the user was created in the user pool.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | The email address of the user.
    userName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The first name, or given name, of the user.
    firstName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The authentication type for the user.
    authenticationType :: AuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- * 'status' - The status of the user in the user pool. The status can be one of the following:
--
--
--     * UNCONFIRMED – The user is created but not confirmed.
--
--
--     * CONFIRMED – The user is confirmed.
--
--
--     * ARCHIVED – The user is no longer active.
--
--
--     * COMPROMISED – The user is disabled because of a potential security threat.
--
--
--     * UNKNOWN – The user status is not known.
--
--
-- * 'enabled' - Specifies whether the user in the user pool is enabled.
-- * 'lastName' - The last name, or surname, of the user.
-- * 'arn' - The ARN of the user.
-- * 'createdTime' - The date and time the user was created in the user pool.
-- * 'userName' - The email address of the user.
-- * 'firstName' - The first name, or given name, of the user.
-- * 'authenticationType' - The authentication type for the user.
mkUser ::
  -- | 'authenticationType'
  AuthenticationType ->
  User
mkUser pAuthenticationType_ =
  User'
    { status = Lude.Nothing,
      enabled = Lude.Nothing,
      lastName = Lude.Nothing,
      arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      userName = Lude.Nothing,
      firstName = Lude.Nothing,
      authenticationType = pAuthenticationType_
    }

-- | The status of the user in the user pool. The status can be one of the following:
--
--
--     * UNCONFIRMED – The user is created but not confirmed.
--
--
--     * CONFIRMED – The user is confirmed.
--
--
--     * ARCHIVED – The user is no longer active.
--
--
--     * COMPROMISED – The user is disabled because of a potential security threat.
--
--
--     * UNKNOWN – The user status is not known.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' User (Lude.Maybe Lude.Text)
uStatus = Lens.lens (status :: User -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: User)
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether the user in the user pool is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEnabled :: Lens.Lens' User (Lude.Maybe Lude.Bool)
uEnabled = Lens.lens (enabled :: User -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: User)
{-# DEPRECATED uEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The last name, or surname, of the user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLastName :: Lens.Lens' User (Lude.Maybe (Lude.Sensitive Lude.Text))
uLastName = Lens.lens (lastName :: User -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {lastName = a} :: User)
{-# DEPRECATED uLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The ARN of the user.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uARN :: Lens.Lens' User (Lude.Maybe Lude.Text)
uARN = Lens.lens (arn :: User -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: User)
{-# DEPRECATED uARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time the user was created in the user pool.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreatedTime :: Lens.Lens' User (Lude.Maybe Lude.Timestamp)
uCreatedTime = Lens.lens (createdTime :: User -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: User)
{-# DEPRECATED uCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserName :: Lens.Lens' User (Lude.Maybe (Lude.Sensitive Lude.Text))
uUserName = Lens.lens (userName :: User -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {userName = a} :: User)
{-# DEPRECATED uUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The first name, or given name, of the user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uFirstName :: Lens.Lens' User (Lude.Maybe (Lude.Sensitive Lude.Text))
uFirstName = Lens.lens (firstName :: User -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {firstName = a} :: User)
{-# DEPRECATED uFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The authentication type for the user.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAuthenticationType :: Lens.Lens' User AuthenticationType
uAuthenticationType = Lens.lens (authenticationType :: User -> AuthenticationType) (\s a -> s {authenticationType = a} :: User)
{-# DEPRECATED uAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Lude.FromJSON User where
  parseJSON =
    Lude.withObject
      "User"
      ( \x ->
          User'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "LastName")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "UserName")
            Lude.<*> (x Lude..:? "FirstName")
            Lude.<*> (x Lude..: "AuthenticationType")
      )
