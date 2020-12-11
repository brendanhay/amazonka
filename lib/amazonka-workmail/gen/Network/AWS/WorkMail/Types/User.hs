-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uEmail,
    uState,
    uDisabledDate,
    uName,
    uId,
    uDisplayName,
    uUserRole,
    uEnabledDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.UserRole

-- | The representation of an Amazon WorkMail user.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { email :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe EntityState,
    disabledDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    userRole :: Lude.Maybe UserRole,
    enabledDate :: Lude.Maybe Lude.Timestamp
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
-- * 'disabledDate' - The date indicating when the user was disabled from Amazon WorkMail use.
-- * 'displayName' - The display name of the user.
-- * 'email' - The email of the user.
-- * 'enabledDate' - The date indicating when the user was enabled for Amazon WorkMail use.
-- * 'id' - The identifier of the user.
-- * 'name' - The name of the user.
-- * 'state' - The state of the user, which can be ENABLED, DISABLED, or DELETED.
-- * 'userRole' - The role of the user.
mkUser ::
  User
mkUser =
  User'
    { email = Lude.Nothing,
      state = Lude.Nothing,
      disabledDate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      displayName = Lude.Nothing,
      userRole = Lude.Nothing,
      enabledDate = Lude.Nothing
    }

-- | The email of the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEmail :: Lens.Lens' User (Lude.Maybe Lude.Text)
uEmail = Lens.lens (email :: User -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: User)
{-# DEPRECATED uEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uState :: Lens.Lens' User (Lude.Maybe EntityState)
uState = Lens.lens (state :: User -> Lude.Maybe EntityState) (\s a -> s {state = a} :: User)
{-# DEPRECATED uState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date indicating when the user was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDisabledDate :: Lens.Lens' User (Lude.Maybe Lude.Timestamp)
uDisabledDate = Lens.lens (disabledDate :: User -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: User)
{-# DEPRECATED uDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' User (Lude.Maybe Lude.Text)
uName = Lens.lens (name :: User -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: User)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' User (Lude.Maybe Lude.Text)
uId = Lens.lens (id :: User -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: User)
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The display name of the user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDisplayName :: Lens.Lens' User (Lude.Maybe Lude.Text)
uDisplayName = Lens.lens (displayName :: User -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: User)
{-# DEPRECATED uDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The role of the user.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserRole :: Lens.Lens' User (Lude.Maybe UserRole)
uUserRole = Lens.lens (userRole :: User -> Lude.Maybe UserRole) (\s a -> s {userRole = a} :: User)
{-# DEPRECATED uUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The date indicating when the user was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEnabledDate :: Lens.Lens' User (Lude.Maybe Lude.Timestamp)
uEnabledDate = Lens.lens (enabledDate :: User -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: User)
{-# DEPRECATED uEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

instance Lude.FromJSON User where
  parseJSON =
    Lude.withObject
      "User"
      ( \x ->
          User'
            Lude.<$> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "DisabledDate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "UserRole")
            Lude.<*> (x Lude..:? "EnabledDate")
      )
