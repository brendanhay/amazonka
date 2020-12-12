{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Grantee
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Grantee
  ( Grantee (..),

    -- * Smart constructor
    mkGrantee,

    -- * Lenses
    gURI,
    gEmailAddress,
    gDisplayName,
    gId,
    gType,
  )
where

import Network.AWS.Glacier.Types.Type
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the grantee.
--
-- /See:/ 'mkGrantee' smart constructor.
data Grantee = Grantee'
  { uri :: Lude.Maybe Lude.Text,
    emailAddress :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Type
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Grantee' with the minimum fields required to make a request.
--
-- * 'displayName' - Screen name of the grantee.
-- * 'emailAddress' - Email address of the grantee.
-- * 'id' - The canonical user ID of the grantee.
-- * 'type'' - Type of grantee
-- * 'uri' - URI of the grantee group.
mkGrantee ::
  -- | 'type''
  Type ->
  Grantee
mkGrantee pType_ =
  Grantee'
    { uri = Lude.Nothing,
      emailAddress = Lude.Nothing,
      displayName = Lude.Nothing,
      id = Lude.Nothing,
      type' = pType_
    }

-- | URI of the grantee group.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gURI :: Lens.Lens' Grantee (Lude.Maybe Lude.Text)
gURI = Lens.lens (uri :: Grantee -> Lude.Maybe Lude.Text) (\s a -> s {uri = a} :: Grantee)
{-# DEPRECATED gURI "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | Email address of the grantee.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEmailAddress :: Lens.Lens' Grantee (Lude.Maybe Lude.Text)
gEmailAddress = Lens.lens (emailAddress :: Grantee -> Lude.Maybe Lude.Text) (\s a -> s {emailAddress = a} :: Grantee)
{-# DEPRECATED gEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | Screen name of the grantee.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDisplayName :: Lens.Lens' Grantee (Lude.Maybe Lude.Text)
gDisplayName = Lens.lens (displayName :: Grantee -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Grantee)
{-# DEPRECATED gDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The canonical user ID of the grantee.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gId :: Lens.Lens' Grantee (Lude.Maybe Lude.Text)
gId = Lens.lens (id :: Grantee -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Grantee)
{-# DEPRECATED gId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Type of grantee
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gType :: Lens.Lens' Grantee Type
gType = Lens.lens (type' :: Grantee -> Type) (\s a -> s {type' = a} :: Grantee)
{-# DEPRECATED gType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Grantee where
  parseJSON =
    Lude.withObject
      "Grantee"
      ( \x ->
          Grantee'
            Lude.<$> (x Lude..:? "URI")
            Lude.<*> (x Lude..:? "EmailAddress")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "ID")
            Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON Grantee where
  toJSON Grantee' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("URI" Lude..=) Lude.<$> uri,
            ("EmailAddress" Lude..=) Lude.<$> emailAddress,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("ID" Lude..=) Lude.<$> id,
            Lude.Just ("Type" Lude..= type')
          ]
      )
