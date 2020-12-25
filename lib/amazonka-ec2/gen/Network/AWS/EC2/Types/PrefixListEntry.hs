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

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a prefix list entry.
--
-- /See:/ 'mkPrefixListEntry' smart constructor.
data PrefixListEntry = PrefixListEntry'
  { -- | The CIDR block.
    cidr :: Core.Maybe Types.String,
    -- | The description.
    description :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrefixListEntry' value with any optional fields omitted.
mkPrefixListEntry ::
  PrefixListEntry
mkPrefixListEntry =
  PrefixListEntry' {cidr = Core.Nothing, description = Core.Nothing}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleCidr :: Lens.Lens' PrefixListEntry (Core.Maybe Types.String)
pleCidr = Lens.field @"cidr"
{-# DEPRECATED pleCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleDescription :: Lens.Lens' PrefixListEntry (Core.Maybe Types.String)
pleDescription = Lens.field @"description"
{-# DEPRECATED pleDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromXML PrefixListEntry where
  parseXML x =
    PrefixListEntry'
      Core.<$> (x Core..@? "cidr") Core.<*> (x Core..@? "description")
