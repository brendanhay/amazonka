-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Owner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Owner
  ( Owner (..),

    -- * Smart constructor
    mkOwner,

    -- * Lenses
    oDisplayName,
    oId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for the owner's display name and ID.
--
-- /See:/ 'mkOwner' smart constructor.
data Owner = Owner'
  { displayName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'Owner' with the minimum fields required to make a request.
--
-- * 'displayName' - Container for the display name of the owner.
-- * 'id' - Container for the ID of the owner.
mkOwner ::
  Owner
mkOwner = Owner' {displayName = Lude.Nothing, id = Lude.Nothing}

-- | Container for the display name of the owner.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDisplayName :: Lens.Lens' Owner (Lude.Maybe Lude.Text)
oDisplayName = Lens.lens (displayName :: Owner -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Owner)
{-# DEPRECATED oDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Container for the ID of the owner.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Owner (Lude.Maybe Lude.Text)
oId = Lens.lens (id :: Owner -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Owner)
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML Owner where
  parseXML x =
    Owner'
      Lude.<$> (x Lude..@? "DisplayName") Lude.<*> (x Lude..@? "ID")

instance Lude.ToXML Owner where
  toXML Owner' {..} =
    Lude.mconcat ["DisplayName" Lude.@= displayName, "ID" Lude.@= id]
