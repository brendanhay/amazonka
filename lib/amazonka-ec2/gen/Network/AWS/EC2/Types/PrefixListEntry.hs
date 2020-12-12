{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListEntry
  ( PrefixListEntry (..),

    -- * Smart constructor
    mkPrefixListEntry,

    -- * Lenses
    pleCidr,
    pleDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a prefix list entry.
--
-- /See:/ 'mkPrefixListEntry' smart constructor.
data PrefixListEntry = PrefixListEntry'
  { cidr ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrefixListEntry' with the minimum fields required to make a request.
--
-- * 'cidr' - The CIDR block.
-- * 'description' - The description.
mkPrefixListEntry ::
  PrefixListEntry
mkPrefixListEntry =
  PrefixListEntry' {cidr = Lude.Nothing, description = Lude.Nothing}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleCidr :: Lens.Lens' PrefixListEntry (Lude.Maybe Lude.Text)
pleCidr = Lens.lens (cidr :: PrefixListEntry -> Lude.Maybe Lude.Text) (\s a -> s {cidr = a} :: PrefixListEntry)
{-# DEPRECATED pleCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleDescription :: Lens.Lens' PrefixListEntry (Lude.Maybe Lude.Text)
pleDescription = Lens.lens (description :: PrefixListEntry -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PrefixListEntry)
{-# DEPRECATED pleDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML PrefixListEntry where
  parseXML x =
    PrefixListEntry'
      Lude.<$> (x Lude..@? "cidr") Lude.<*> (x Lude..@? "description")
