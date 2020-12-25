{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.IPRange
  ( IPRange (..),

    -- * Smart constructor
    mkIPRange,

    -- * Lenses
    iprCIDRIP,
    iprStatus,
    iprTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes an IP range used in a security group.
--
-- /See:/ 'mkIPRange' smart constructor.
data IPRange = IPRange'
  { -- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
    cidrip :: Core.Maybe Types.String,
    -- | The status of the IP range, for example, "authorized".
    status :: Core.Maybe Types.String,
    -- | The list of tags for the IP range.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPRange' value with any optional fields omitted.
mkIPRange ::
  IPRange
mkIPRange =
  IPRange'
    { cidrip = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing
    }

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprCIDRIP :: Lens.Lens' IPRange (Core.Maybe Types.String)
iprCIDRIP = Lens.field @"cidrip"
{-# DEPRECATED iprCIDRIP "Use generic-lens or generic-optics with 'cidrip' instead." #-}

-- | The status of the IP range, for example, "authorized".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprStatus :: Lens.Lens' IPRange (Core.Maybe Types.String)
iprStatus = Lens.field @"status"
{-# DEPRECATED iprStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The list of tags for the IP range.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprTags :: Lens.Lens' IPRange (Core.Maybe [Types.Tag])
iprTags = Lens.field @"tags"
{-# DEPRECATED iprTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML IPRange where
  parseXML x =
    IPRange'
      Core.<$> (x Core..@? "CIDRIP")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag")
