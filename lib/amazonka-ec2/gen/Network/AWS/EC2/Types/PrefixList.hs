{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrefixList
  ( PrefixList (..)
  -- * Smart constructor
  , mkPrefixList
  -- * Lenses
  , plCidrs
  , plPrefixListId
  , plPrefixListName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes prefixes for AWS services.
--
-- /See:/ 'mkPrefixList' smart constructor.
data PrefixList = PrefixList'
  { cidrs :: Core.Maybe [Core.Text]
    -- ^ The IP address range of the AWS service.
  , prefixListId :: Core.Maybe Core.Text
    -- ^ The ID of the prefix.
  , prefixListName :: Core.Maybe Core.Text
    -- ^ The name of the prefix.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrefixList' value with any optional fields omitted.
mkPrefixList
    :: PrefixList
mkPrefixList
  = PrefixList'{cidrs = Core.Nothing, prefixListId = Core.Nothing,
                prefixListName = Core.Nothing}

-- | The IP address range of the AWS service.
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plCidrs :: Lens.Lens' PrefixList (Core.Maybe [Core.Text])
plCidrs = Lens.field @"cidrs"
{-# INLINEABLE plCidrs #-}
{-# DEPRECATED cidrs "Use generic-lens or generic-optics with 'cidrs' instead"  #-}

-- | The ID of the prefix.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plPrefixListId :: Lens.Lens' PrefixList (Core.Maybe Core.Text)
plPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE plPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | The name of the prefix.
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plPrefixListName :: Lens.Lens' PrefixList (Core.Maybe Core.Text)
plPrefixListName = Lens.field @"prefixListName"
{-# INLINEABLE plPrefixListName #-}
{-# DEPRECATED prefixListName "Use generic-lens or generic-optics with 'prefixListName' instead"  #-}

instance Core.FromXML PrefixList where
        parseXML x
          = PrefixList' Core.<$>
              (x Core..@? "cidrSet" Core..<@> Core.parseXMLList "item") Core.<*>
                x Core..@? "prefixListId"
                Core.<*> x Core..@? "prefixListName"
