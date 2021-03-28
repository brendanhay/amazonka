{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.IPRange
  ( IPRange (..)
  -- * Smart constructor
  , mkIPRange
  -- * Lenses
  , iprCIDRIP
  , iprStatus
  , iprTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes an IP range used in a security group.
--
-- /See:/ 'mkIPRange' smart constructor.
data IPRange = IPRange'
  { cidrip :: Core.Maybe Core.Text
    -- ^ The IP range in Classless Inter-Domain Routing (CIDR) notation.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the IP range, for example, "authorized".
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the IP range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPRange' value with any optional fields omitted.
mkIPRange
    :: IPRange
mkIPRange
  = IPRange'{cidrip = Core.Nothing, status = Core.Nothing,
             tags = Core.Nothing}

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprCIDRIP :: Lens.Lens' IPRange (Core.Maybe Core.Text)
iprCIDRIP = Lens.field @"cidrip"
{-# INLINEABLE iprCIDRIP #-}
{-# DEPRECATED cidrip "Use generic-lens or generic-optics with 'cidrip' instead"  #-}

-- | The status of the IP range, for example, "authorized".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprStatus :: Lens.Lens' IPRange (Core.Maybe Core.Text)
iprStatus = Lens.field @"status"
{-# INLINEABLE iprStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The list of tags for the IP range.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprTags :: Lens.Lens' IPRange (Core.Maybe [Types.Tag])
iprTags = Lens.field @"tags"
{-# INLINEABLE iprTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML IPRange where
        parseXML x
          = IPRange' Core.<$>
              (x Core..@? "CIDRIP") Core.<*> x Core..@? "Status" Core.<*>
                x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
