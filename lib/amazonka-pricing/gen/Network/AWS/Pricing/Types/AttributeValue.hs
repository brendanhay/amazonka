{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pricing.Types.AttributeValue
  ( AttributeValue (..)
  -- * Smart constructor
  , mkAttributeValue
  -- * Lenses
  , avValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The values of a given attribute, such as @Throughput Optimized HDD@ or @Provisioned IOPS@ for the @Amazon EC2@ @volumeType@ attribute.
--
-- /See:/ 'mkAttributeValue' smart constructor.
newtype AttributeValue = AttributeValue'
  { value :: Core.Maybe Core.Text
    -- ^ The specific value of an @attributeName@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeValue' value with any optional fields omitted.
mkAttributeValue
    :: AttributeValue
mkAttributeValue = AttributeValue'{value = Core.Nothing}

-- | The specific value of an @attributeName@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avValue :: Lens.Lens' AttributeValue (Core.Maybe Core.Text)
avValue = Lens.field @"value"
{-# INLINEABLE avValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON AttributeValue where
        parseJSON
          = Core.withObject "AttributeValue" Core.$
              \ x -> AttributeValue' Core.<$> (x Core..:? "Value")
