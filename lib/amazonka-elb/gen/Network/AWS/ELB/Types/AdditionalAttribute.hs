{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AdditionalAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.AdditionalAttribute
  ( AdditionalAttribute (..)
  -- * Smart constructor
  , mkAdditionalAttribute
  -- * Lenses
  , aaKey
  , aaValue
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.AdditionalAttributeKey as Types
import qualified Network.AWS.ELB.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about additional load balancer attributes.
--
-- /See:/ 'mkAdditionalAttribute' smart constructor.
data AdditionalAttribute = AdditionalAttribute'
  { key :: Core.Maybe Types.AdditionalAttributeKey
    -- ^ The name of the attribute.
--
-- The following attribute is supported.
--
--     * @elb.http.desyncmitigationmode@ - Determines how the load balancer handles requests that might pose a security risk to your application. The possible values are @monitor@ , @defensive@ , and @strictest@ . The default is @defensive@ .
--
--
  , value :: Core.Maybe Types.Value
    -- ^ This value of the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdditionalAttribute' value with any optional fields omitted.
mkAdditionalAttribute
    :: AdditionalAttribute
mkAdditionalAttribute
  = AdditionalAttribute'{key = Core.Nothing, value = Core.Nothing}

-- | The name of the attribute.
--
-- The following attribute is supported.
--
--     * @elb.http.desyncmitigationmode@ - Determines how the load balancer handles requests that might pose a security risk to your application. The possible values are @monitor@ , @defensive@ , and @strictest@ . The default is @defensive@ .
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaKey :: Lens.Lens' AdditionalAttribute (Core.Maybe Types.AdditionalAttributeKey)
aaKey = Lens.field @"key"
{-# INLINEABLE aaKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | This value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaValue :: Lens.Lens' AdditionalAttribute (Core.Maybe Types.Value)
aaValue = Lens.field @"value"
{-# INLINEABLE aaValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery AdditionalAttribute where
        toQuery AdditionalAttribute{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Key") key Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Value") value

instance Core.FromXML AdditionalAttribute where
        parseXML x
          = AdditionalAttribute' Core.<$>
              (x Core..@? "Key") Core.<*> x Core..@? "Value"
