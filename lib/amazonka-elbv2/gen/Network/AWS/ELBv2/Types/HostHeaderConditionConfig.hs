{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HostHeaderConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.HostHeaderConditionConfig
  ( HostHeaderConditionConfig (..)
  -- * Smart constructor
  , mkHostHeaderConditionConfig
  -- * Lenses
  , hhccValues
  ) where

import qualified Network.AWS.ELBv2.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a host header condition.
--
-- /See:/ 'mkHostHeaderConditionConfig' smart constructor.
newtype HostHeaderConditionConfig = HostHeaderConditionConfig'
  { values :: Core.Maybe [Types.StringValue]
    -- ^ One or more host names. The maximum size of each name is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the host name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HostHeaderConditionConfig' value with any optional fields omitted.
mkHostHeaderConditionConfig
    :: HostHeaderConditionConfig
mkHostHeaderConditionConfig
  = HostHeaderConditionConfig'{values = Core.Nothing}

-- | One or more host names. The maximum size of each name is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the host name.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hhccValues :: Lens.Lens' HostHeaderConditionConfig (Core.Maybe [Types.StringValue])
hhccValues = Lens.field @"values"
{-# INLINEABLE hhccValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery HostHeaderConditionConfig where
        toQuery HostHeaderConditionConfig{..}
          = Core.toQueryPair "Values"
              (Core.maybe Core.mempty (Core.toQueryList "member") values)

instance Core.FromXML HostHeaderConditionConfig where
        parseXML x
          = HostHeaderConditionConfig' Core.<$>
              (x Core..@? "Values" Core..<@> Core.parseXMLList "member")
