{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An entry for a prefix list.
--
-- /See:/ 'mkRemovePrefixListEntry' smart constructor.
newtype RemovePrefixListEntry = RemovePrefixListEntry'
  { -- | The CIDR block.
    cidr :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePrefixListEntry' value with any optional fields omitted.
mkRemovePrefixListEntry ::
  -- | 'cidr'
  Types.String ->
  RemovePrefixListEntry
mkRemovePrefixListEntry cidr = RemovePrefixListEntry' {cidr}

-- | The CIDR block.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpleCidr :: Lens.Lens' RemovePrefixListEntry Types.String
rpleCidr = Lens.field @"cidr"
{-# DEPRECATED rpleCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}
