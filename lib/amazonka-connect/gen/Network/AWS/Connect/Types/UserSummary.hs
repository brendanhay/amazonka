{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserSummary
  ( UserSummary (..),

    -- * Smart constructor
    mkUserSummary,

    -- * Lenses
    usARN,
    usUsername,
    usId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a user.
--
-- /See:/ 'mkUserSummary' smart constructor.
data UserSummary = UserSummary'
  { arn :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
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
-- * 'arn' - The Amazon Resource Name (ARN) of the user account.
-- * 'id' - The identifier of the user account.
-- * 'username' - The Amazon Connect user name of the user account.
mkUserSummary ::
  UserSummary
mkUserSummary =
  UserSummary'
    { arn = Lude.Nothing,
      username = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usARN :: Lens.Lens' UserSummary (Lude.Maybe Lude.Text)
usARN = Lens.lens (arn :: UserSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UserSummary)
{-# DEPRECATED usARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The Amazon Connect user name of the user account.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUsername :: Lens.Lens' UserSummary (Lude.Maybe Lude.Text)
usUsername = Lens.lens (username :: UserSummary -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: UserSummary)
{-# DEPRECATED usUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usId :: Lens.Lens' UserSummary (Lude.Maybe Lude.Text)
usId = Lens.lens (id :: UserSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UserSummary)
{-# DEPRECATED usId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON UserSummary where
  parseJSON =
    Lude.withObject
      "UserSummary"
      ( \x ->
          UserSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Id")
      )
