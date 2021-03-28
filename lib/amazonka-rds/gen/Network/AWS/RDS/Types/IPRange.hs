{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.IPRange
  ( IPRange (..)
  -- * Smart constructor
  , mkIPRange
  -- * Lenses
  , iprCIDRIP
  , iprStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used as a response element in the @DescribeDBSecurityGroups@ action. 
--
-- /See:/ 'mkIPRange' smart constructor.
data IPRange = IPRange'
  { cidrip :: Core.Maybe Core.Text
    -- ^ Specifies the IP range.
  , status :: Core.Maybe Core.Text
    -- ^ Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPRange' value with any optional fields omitted.
mkIPRange
    :: IPRange
mkIPRange = IPRange'{cidrip = Core.Nothing, status = Core.Nothing}

-- | Specifies the IP range.
--
-- /Note:/ Consider using 'cidrip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprCIDRIP :: Lens.Lens' IPRange (Core.Maybe Core.Text)
iprCIDRIP = Lens.field @"cidrip"
{-# INLINEABLE iprCIDRIP #-}
{-# DEPRECATED cidrip "Use generic-lens or generic-optics with 'cidrip' instead"  #-}

-- | Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprStatus :: Lens.Lens' IPRange (Core.Maybe Core.Text)
iprStatus = Lens.field @"status"
{-# INLINEABLE iprStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML IPRange where
        parseXML x
          = IPRange' Core.<$>
              (x Core..@? "CIDRIP") Core.<*> x Core..@? "Status"
