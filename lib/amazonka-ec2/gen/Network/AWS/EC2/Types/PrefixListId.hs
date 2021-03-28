{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrefixListId
  ( PrefixListId (..)
  -- * Smart constructor
  , mkPrefixListId
  -- * Lenses
  , pliDescription
  , pliPrefixListId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a prefix list ID.
--
-- /See:/ 'mkPrefixListId' smart constructor.
data PrefixListId = PrefixListId'
  { description :: Core.Maybe Core.Text
    -- ^ A description for the security group rule that references this prefix list ID.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
  , prefixListId :: Core.Maybe Core.Text
    -- ^ The ID of the prefix.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrefixListId' value with any optional fields omitted.
mkPrefixListId
    :: PrefixListId
mkPrefixListId
  = PrefixListId'{description = Core.Nothing,
                  prefixListId = Core.Nothing}

-- | A description for the security group rule that references this prefix list ID.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pliDescription :: Lens.Lens' PrefixListId (Core.Maybe Core.Text)
pliDescription = Lens.field @"description"
{-# INLINEABLE pliDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the prefix.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pliPrefixListId :: Lens.Lens' PrefixListId (Core.Maybe Core.Text)
pliPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE pliPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

instance Core.ToQuery PrefixListId where
        toQuery PrefixListId{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Description")
              description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrefixListId")
                prefixListId

instance Core.FromXML PrefixListId where
        parseXML x
          = PrefixListId' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "prefixListId"
