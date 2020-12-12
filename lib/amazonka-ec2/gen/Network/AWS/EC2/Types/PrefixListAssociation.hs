{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListAssociation
  ( PrefixListAssociation (..),

    -- * Smart constructor
    mkPrefixListAssociation,

    -- * Lenses
    plaResourceId,
    plaResourceOwner,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the resource with which a prefix list is associated.
--
-- /See:/ 'mkPrefixListAssociation' smart constructor.
data PrefixListAssociation = PrefixListAssociation'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceOwner :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrefixListAssociation' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceOwner' - The owner of the resource.
mkPrefixListAssociation ::
  PrefixListAssociation
mkPrefixListAssociation =
  PrefixListAssociation'
    { resourceId = Lude.Nothing,
      resourceOwner = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaResourceId :: Lens.Lens' PrefixListAssociation (Lude.Maybe Lude.Text)
plaResourceId = Lens.lens (resourceId :: PrefixListAssociation -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: PrefixListAssociation)
{-# DEPRECATED plaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The owner of the resource.
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaResourceOwner :: Lens.Lens' PrefixListAssociation (Lude.Maybe Lude.Text)
plaResourceOwner = Lens.lens (resourceOwner :: PrefixListAssociation -> Lude.Maybe Lude.Text) (\s a -> s {resourceOwner = a} :: PrefixListAssociation)
{-# DEPRECATED plaResourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead." #-}

instance Lude.FromXML PrefixListAssociation where
  parseXML x =
    PrefixListAssociation'
      Lude.<$> (x Lude..@? "resourceId") Lude.<*> (x Lude..@? "resourceOwner")
