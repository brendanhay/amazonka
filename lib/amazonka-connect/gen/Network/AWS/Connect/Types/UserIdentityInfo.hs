{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserIdentityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserIdentityInfo
  ( UserIdentityInfo (..),

    -- * Smart constructor
    mkUserIdentityInfo,

    -- * Lenses
    uiiEmail,
    uiiLastName,
    uiiFirstName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the identity of a user.
--
-- /See:/ 'mkUserIdentityInfo' smart constructor.
data UserIdentityInfo = UserIdentityInfo'
  { email ::
      Lude.Maybe Lude.Text,
    lastName :: Lude.Maybe Lude.Text,
    firstName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserIdentityInfo' with the minimum fields required to make a request.
--
-- * 'email' - The email address. If you are using SAML for identity management and include this parameter, an error is returned.
-- * 'firstName' - The first name. This is required if you are using Amazon Connect or SAML for identity management.
-- * 'lastName' - The last name. This is required if you are using Amazon Connect or SAML for identity management.
mkUserIdentityInfo ::
  UserIdentityInfo
mkUserIdentityInfo =
  UserIdentityInfo'
    { email = Lude.Nothing,
      lastName = Lude.Nothing,
      firstName = Lude.Nothing
    }

-- | The email address. If you are using SAML for identity management and include this parameter, an error is returned.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiEmail :: Lens.Lens' UserIdentityInfo (Lude.Maybe Lude.Text)
uiiEmail = Lens.lens (email :: UserIdentityInfo -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: UserIdentityInfo)
{-# DEPRECATED uiiEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The last name. This is required if you are using Amazon Connect or SAML for identity management.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiLastName :: Lens.Lens' UserIdentityInfo (Lude.Maybe Lude.Text)
uiiLastName = Lens.lens (lastName :: UserIdentityInfo -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: UserIdentityInfo)
{-# DEPRECATED uiiLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The first name. This is required if you are using Amazon Connect or SAML for identity management.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiFirstName :: Lens.Lens' UserIdentityInfo (Lude.Maybe Lude.Text)
uiiFirstName = Lens.lens (firstName :: UserIdentityInfo -> Lude.Maybe Lude.Text) (\s a -> s {firstName = a} :: UserIdentityInfo)
{-# DEPRECATED uiiFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

instance Lude.FromJSON UserIdentityInfo where
  parseJSON =
    Lude.withObject
      "UserIdentityInfo"
      ( \x ->
          UserIdentityInfo'
            Lude.<$> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "LastName")
            Lude.<*> (x Lude..:? "FirstName")
      )

instance Lude.ToJSON UserIdentityInfo where
  toJSON UserIdentityInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Email" Lude..=) Lude.<$> email,
            ("LastName" Lude..=) Lude.<$> lastName,
            ("FirstName" Lude..=) Lude.<$> firstName
          ]
      )
