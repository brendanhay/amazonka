{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uGroups,
    uConsoleAccess,
    uUsername,
    uPassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A user associated with the broker.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    groups :: Lude.Maybe [Lude.Text],
    -- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does not apply to RabbitMQ brokers).
    consoleAccess :: Lude.Maybe Lude.Bool,
    -- | Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    username :: Lude.Maybe Lude.Text,
    -- | Required. The password of the broker user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
    password :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- * 'groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'consoleAccess' - Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does not apply to RabbitMQ brokers).
-- * 'username' - Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'password' - Required. The password of the broker user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
mkUser ::
  User
mkUser =
  User'
    { groups = Lude.Nothing,
      consoleAccess = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uGroups :: Lens.Lens' User (Lude.Maybe [Lude.Text])
uGroups = Lens.lens (groups :: User -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: User)
{-# DEPRECATED uGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does not apply to RabbitMQ brokers).
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConsoleAccess :: Lens.Lens' User (Lude.Maybe Lude.Bool)
uConsoleAccess = Lens.lens (consoleAccess :: User -> Lude.Maybe Lude.Bool) (\s a -> s {consoleAccess = a} :: User)
{-# DEPRECATED uConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsername :: Lens.Lens' User (Lude.Maybe Lude.Text)
uUsername = Lens.lens (username :: User -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: User)
{-# DEPRECATED uUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Required. The password of the broker user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPassword :: Lens.Lens' User (Lude.Maybe Lude.Text)
uPassword = Lens.lens (password :: User -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: User)
{-# DEPRECATED uPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.ToJSON User where
  toJSON User' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("groups" Lude..=) Lude.<$> groups,
            ("consoleAccess" Lude..=) Lude.<$> consoleAccess,
            ("username" Lude..=) Lude.<$> username,
            ("password" Lude..=) Lude.<$> password
          ]
      )
