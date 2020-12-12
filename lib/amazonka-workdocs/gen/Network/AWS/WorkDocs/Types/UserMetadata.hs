{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserMetadata
  ( UserMetadata (..),

    -- * Smart constructor
    mkUserMetadata,

    -- * Lenses
    umGivenName,
    umUsername,
    umEmailAddress,
    umId,
    umSurname,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the metadata of the user.
--
-- /See:/ 'mkUserMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { givenName ::
      Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    emailAddress :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    surname :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserMetadata' with the minimum fields required to make a request.
--
-- * 'emailAddress' - The email address of the user.
-- * 'givenName' - The given name of the user before a rename operation.
-- * 'id' - The ID of the user.
-- * 'surname' - The surname of the user.
-- * 'username' - The name of the user.
mkUserMetadata ::
  UserMetadata
mkUserMetadata =
  UserMetadata'
    { givenName = Lude.Nothing,
      username = Lude.Nothing,
      emailAddress = Lude.Nothing,
      id = Lude.Nothing,
      surname = Lude.Nothing
    }

-- | The given name of the user before a rename operation.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umGivenName :: Lens.Lens' UserMetadata (Lude.Maybe Lude.Text)
umGivenName = Lens.lens (givenName :: UserMetadata -> Lude.Maybe Lude.Text) (\s a -> s {givenName = a} :: UserMetadata)
{-# DEPRECATED umGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umUsername :: Lens.Lens' UserMetadata (Lude.Maybe Lude.Text)
umUsername = Lens.lens (username :: UserMetadata -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: UserMetadata)
{-# DEPRECATED umUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umEmailAddress :: Lens.Lens' UserMetadata (Lude.Maybe Lude.Text)
umEmailAddress = Lens.lens (emailAddress :: UserMetadata -> Lude.Maybe Lude.Text) (\s a -> s {emailAddress = a} :: UserMetadata)
{-# DEPRECATED umEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umId :: Lens.Lens' UserMetadata (Lude.Maybe Lude.Text)
umId = Lens.lens (id :: UserMetadata -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UserMetadata)
{-# DEPRECATED umId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umSurname :: Lens.Lens' UserMetadata (Lude.Maybe Lude.Text)
umSurname = Lens.lens (surname :: UserMetadata -> Lude.Maybe Lude.Text) (\s a -> s {surname = a} :: UserMetadata)
{-# DEPRECATED umSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

instance Lude.FromJSON UserMetadata where
  parseJSON =
    Lude.withObject
      "UserMetadata"
      ( \x ->
          UserMetadata'
            Lude.<$> (x Lude..:? "GivenName")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "EmailAddress")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Surname")
      )
