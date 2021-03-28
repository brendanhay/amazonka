{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IpRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.IpRoute
  ( IpRoute (..)
  -- * Smart constructor
  , mkIpRoute
  -- * Lenses
  , irCidrIp
  , irDescription
  ) where

import qualified Network.AWS.DirectoryService.Types.CidrIp as Types
import qualified Network.AWS.DirectoryService.Types.Description as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | IP address block. This is often the address block of the DNS server used for your on-premises domain. 
--
-- /See:/ 'mkIpRoute' smart constructor.
data IpRoute = IpRoute'
  { cidrIp :: Core.Maybe Types.CidrIp
    -- ^ IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
  , description :: Core.Maybe Types.Description
    -- ^ Description of the address block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IpRoute' value with any optional fields omitted.
mkIpRoute
    :: IpRoute
mkIpRoute
  = IpRoute'{cidrIp = Core.Nothing, description = Core.Nothing}

-- | IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irCidrIp :: Lens.Lens' IpRoute (Core.Maybe Types.CidrIp)
irCidrIp = Lens.field @"cidrIp"
{-# INLINEABLE irCidrIp #-}
{-# DEPRECATED cidrIp "Use generic-lens or generic-optics with 'cidrIp' instead"  #-}

-- | Description of the address block.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irDescription :: Lens.Lens' IpRoute (Core.Maybe Types.Description)
irDescription = Lens.field @"description"
{-# INLINEABLE irDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromJSON IpRoute where
        toJSON IpRoute{..}
          = Core.object
              (Core.catMaybes
                 [("CidrIp" Core..=) Core.<$> cidrIp,
                  ("Description" Core..=) Core.<$> description])
