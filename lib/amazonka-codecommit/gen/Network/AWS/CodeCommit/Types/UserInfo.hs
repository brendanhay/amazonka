-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.UserInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.UserInfo
  ( UserInfo (..),

    -- * Smart constructor
    mkUserInfo,

    -- * Lenses
    uiEmail,
    uiDate,
    uiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the user who made a specified commit.
--
-- /See:/ 'mkUserInfo' smart constructor.
data UserInfo = UserInfo'
  { email :: Lude.Maybe Lude.Text,
    date :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserInfo' with the minimum fields required to make a request.
--
-- * 'date' - The date when the specified commit was commited, in timestamp format with GMT offset.
-- * 'email' - The email address associated with the user who made the commit, if any.
-- * 'name' - The name of the user who made the specified commit.
mkUserInfo ::
  UserInfo
mkUserInfo =
  UserInfo'
    { email = Lude.Nothing,
      date = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The email address associated with the user who made the commit, if any.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiEmail :: Lens.Lens' UserInfo (Lude.Maybe Lude.Text)
uiEmail = Lens.lens (email :: UserInfo -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: UserInfo)
{-# DEPRECATED uiEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The date when the specified commit was commited, in timestamp format with GMT offset.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDate :: Lens.Lens' UserInfo (Lude.Maybe Lude.Text)
uiDate = Lens.lens (date :: UserInfo -> Lude.Maybe Lude.Text) (\s a -> s {date = a} :: UserInfo)
{-# DEPRECATED uiDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The name of the user who made the specified commit.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiName :: Lens.Lens' UserInfo (Lude.Maybe Lude.Text)
uiName = Lens.lens (name :: UserInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UserInfo)
{-# DEPRECATED uiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON UserInfo where
  parseJSON =
    Lude.withObject
      "UserInfo"
      ( \x ->
          UserInfo'
            Lude.<$> (x Lude..:? "email")
            Lude.<*> (x Lude..:? "date")
            Lude.<*> (x Lude..:? "name")
      )
