-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RemovePrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RemovePrefixListEntry
  ( RemovePrefixListEntry (..),

    -- * Smart constructor
    mkRemovePrefixListEntry,

    -- * Lenses
    rpleCidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An entry for a prefix list.
--
-- /See:/ 'mkRemovePrefixListEntry' smart constructor.
newtype RemovePrefixListEntry = RemovePrefixListEntry'
  { cidr ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePrefixListEntry' with the minimum fields required to make a request.
--
-- * 'cidr' - The CIDR block.
mkRemovePrefixListEntry ::
  -- | 'cidr'
  Lude.Text ->
  RemovePrefixListEntry
mkRemovePrefixListEntry pCidr_ =
  RemovePrefixListEntry' {cidr = pCidr_}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpleCidr :: Lens.Lens' RemovePrefixListEntry Lude.Text
rpleCidr = Lens.lens (cidr :: RemovePrefixListEntry -> Lude.Text) (\s a -> s {cidr = a} :: RemovePrefixListEntry)
{-# DEPRECATED rpleCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.ToQuery RemovePrefixListEntry where
  toQuery RemovePrefixListEntry' {..} =
    Lude.mconcat ["Cidr" Lude.=: cidr]
