{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrefixListEntry
  ( PrefixListEntry (..)
  -- * Smart constructor
  , mkPrefixListEntry
  -- * Lenses
  , pleCidr
  , pleDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a prefix list entry.
--
-- /See:/ 'mkPrefixListEntry' smart constructor.
data PrefixListEntry = PrefixListEntry'
  { cidr :: Core.Maybe Core.Text
    -- ^ The CIDR block.
  , description :: Core.Maybe Core.Text
    -- ^ The description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrefixListEntry' value with any optional fields omitted.
mkPrefixListEntry
    :: PrefixListEntry
mkPrefixListEntry
  = PrefixListEntry'{cidr = Core.Nothing, description = Core.Nothing}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleCidr :: Lens.Lens' PrefixListEntry (Core.Maybe Core.Text)
pleCidr = Lens.field @"cidr"
{-# INLINEABLE pleCidr #-}
{-# DEPRECATED cidr "Use generic-lens or generic-optics with 'cidr' instead"  #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleDescription :: Lens.Lens' PrefixListEntry (Core.Maybe Core.Text)
pleDescription = Lens.field @"description"
{-# INLINEABLE pleDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromXML PrefixListEntry where
        parseXML x
          = PrefixListEntry' Core.<$>
              (x Core..@? "cidr") Core.<*> x Core..@? "description"
