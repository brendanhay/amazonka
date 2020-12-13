{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityDescription
  ( IdentityDescription (..),

    -- * Smart constructor
    mkIdentityDescription,

    -- * Lenses
    idLastModifiedDate,
    idCreationDate,
    idLogins,
    idIdentityId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A description of the identity.
--
-- /See:/ 'mkIdentityDescription' smart constructor.
data IdentityDescription = IdentityDescription'
  { -- | Date on which the identity was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | Date on which the identity was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The provider names.
    logins :: Lude.Maybe [Lude.Text],
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityDescription' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - Date on which the identity was last modified.
-- * 'creationDate' - Date on which the identity was created.
-- * 'logins' - The provider names.
-- * 'identityId' - A unique identifier in the format REGION:GUID.
mkIdentityDescription ::
  IdentityDescription
mkIdentityDescription =
  IdentityDescription'
    { lastModifiedDate = Lude.Nothing,
      creationDate = Lude.Nothing,
      logins = Lude.Nothing,
      identityId = Lude.Nothing
    }

-- | Date on which the identity was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idLastModifiedDate :: Lens.Lens' IdentityDescription (Lude.Maybe Lude.Timestamp)
idLastModifiedDate = Lens.lens (lastModifiedDate :: IdentityDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: IdentityDescription)
{-# DEPRECATED idLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Date on which the identity was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idCreationDate :: Lens.Lens' IdentityDescription (Lude.Maybe Lude.Timestamp)
idCreationDate = Lens.lens (creationDate :: IdentityDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: IdentityDescription)
{-# DEPRECATED idCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The provider names.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idLogins :: Lens.Lens' IdentityDescription (Lude.Maybe [Lude.Text])
idLogins = Lens.lens (logins :: IdentityDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {logins = a} :: IdentityDescription)
{-# DEPRECATED idLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idIdentityId :: Lens.Lens' IdentityDescription (Lude.Maybe Lude.Text)
idIdentityId = Lens.lens (identityId :: IdentityDescription -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: IdentityDescription)
{-# DEPRECATED idIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.FromJSON IdentityDescription where
  parseJSON =
    Lude.withObject
      "IdentityDescription"
      ( \x ->
          IdentityDescription'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "Logins" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IdentityId")
      )
