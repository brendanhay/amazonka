{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SourceIpConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.SourceIpConditionConfig
  ( SourceIpConditionConfig (..)
  -- * Smart constructor
  , mkSourceIpConditionConfig
  -- * Lenses
  , siccValues
  ) where

import qualified Network.AWS.ELBv2.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a source IP condition.
--
-- You can use this condition to route based on the IP address of the source that connects to the load balancer. If a client is behind a proxy, this is the IP address of the proxy not the IP address of the client.
--
-- /See:/ 'mkSourceIpConditionConfig' smart constructor.
newtype SourceIpConditionConfig = SourceIpConditionConfig'
  { values :: Core.Maybe [Types.StringValue]
    -- ^ One or more source IP addresses, in CIDR format. You can use both IPv4 and IPv6 addresses. Wildcards are not supported.
--
-- If you specify multiple addresses, the condition is satisfied if the source IP address of the request matches one of the CIDR blocks. This condition is not satisfied by the addresses in the X-Forwarded-For header. To search for addresses in the X-Forwarded-For header, use 'HttpHeaderConditionConfig' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SourceIpConditionConfig' value with any optional fields omitted.
mkSourceIpConditionConfig
    :: SourceIpConditionConfig
mkSourceIpConditionConfig
  = SourceIpConditionConfig'{values = Core.Nothing}

-- | One or more source IP addresses, in CIDR format. You can use both IPv4 and IPv6 addresses. Wildcards are not supported.
--
-- If you specify multiple addresses, the condition is satisfied if the source IP address of the request matches one of the CIDR blocks. This condition is not satisfied by the addresses in the X-Forwarded-For header. To search for addresses in the X-Forwarded-For header, use 'HttpHeaderConditionConfig' .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siccValues :: Lens.Lens' SourceIpConditionConfig (Core.Maybe [Types.StringValue])
siccValues = Lens.field @"values"
{-# INLINEABLE siccValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery SourceIpConditionConfig where
        toQuery SourceIpConditionConfig{..}
          = Core.toQueryPair "Values"
              (Core.maybe Core.mempty (Core.toQueryList "member") values)

instance Core.FromXML SourceIpConditionConfig where
        parseXML x
          = SourceIpConditionConfig' Core.<$>
              (x Core..@? "Values" Core..<@> Core.parseXMLList "member")
