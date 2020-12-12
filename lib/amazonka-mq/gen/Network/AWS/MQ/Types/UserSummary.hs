{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.UserSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.UserSummary
  ( UserSummary (..),

    -- * Smart constructor
    mkUserSummary,

    -- * Lenses
    usUsername,
    usPendingChange,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.ChangeType
import qualified Network.AWS.Prelude as Lude

-- | Returns a list of all broker users.
--
-- /See:/ 'mkUserSummary' smart constructor.
data UserSummary = UserSummary'
  { username :: Lude.Maybe Lude.Text,
    pendingChange :: Lude.Maybe ChangeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserSummary' with the minimum fields required to make a request.
--
-- * 'pendingChange' - The type of change pending for the broker user.
-- * 'username' - Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
mkUserSummary ::
  UserSummary
mkUserSummary =
  UserSummary'
    { username = Lude.Nothing,
      pendingChange = Lude.Nothing
    }

-- | Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUsername :: Lens.Lens' UserSummary (Lude.Maybe Lude.Text)
usUsername = Lens.lens (username :: UserSummary -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: UserSummary)
{-# DEPRECATED usUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The type of change pending for the broker user.
--
-- /Note:/ Consider using 'pendingChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPendingChange :: Lens.Lens' UserSummary (Lude.Maybe ChangeType)
usPendingChange = Lens.lens (pendingChange :: UserSummary -> Lude.Maybe ChangeType) (\s a -> s {pendingChange = a} :: UserSummary)
{-# DEPRECATED usPendingChange "Use generic-lens or generic-optics with 'pendingChange' instead." #-}

instance Lude.FromJSON UserSummary where
  parseJSON =
    Lude.withObject
      "UserSummary"
      ( \x ->
          UserSummary'
            Lude.<$> (x Lude..:? "username") Lude.<*> (x Lude..:? "pendingChange")
      )
