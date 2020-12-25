{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AddPrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AddPrefixListEntry
  ( AddPrefixListEntry (..),

    -- * Smart constructor
    mkAddPrefixListEntry,

    -- * Lenses
    apleCidr,
    apleDescription,
  )
where

import qualified Network.AWS.EC2.Types.Cidr as Types
import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An entry for a prefix list.
--
-- /See:/ 'mkAddPrefixListEntry' smart constructor.
data AddPrefixListEntry = AddPrefixListEntry'
  { -- | The CIDR block.
    cidr :: Types.Cidr,
    -- | A description for the entry.
    --
    -- Constraints: Up to 255 characters in length.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddPrefixListEntry' value with any optional fields omitted.
mkAddPrefixListEntry ::
  -- | 'cidr'
  Types.Cidr ->
  AddPrefixListEntry
mkAddPrefixListEntry cidr =
  AddPrefixListEntry' {cidr, description = Core.Nothing}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apleCidr :: Lens.Lens' AddPrefixListEntry Types.Cidr
apleCidr = Lens.field @"cidr"
{-# DEPRECATED apleCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | A description for the entry.
--
-- Constraints: Up to 255 characters in length.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apleDescription :: Lens.Lens' AddPrefixListEntry (Core.Maybe Types.Description)
apleDescription = Lens.field @"description"
{-# DEPRECATED apleDescription "Use generic-lens or generic-optics with 'description' instead." #-}
