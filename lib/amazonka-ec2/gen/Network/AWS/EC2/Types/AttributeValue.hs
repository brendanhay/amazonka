{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AttributeValue
  ( AttributeValue (..)
  -- * Smart constructor
  , mkAttributeValue
  -- * Lenses
  , avValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a value for a resource attribute that is a String.
--
-- /See:/ 'mkAttributeValue' smart constructor.
newtype AttributeValue = AttributeValue'
  { value :: Core.Maybe Core.Text
    -- ^ The attribute value. The value is case-sensitive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeValue' value with any optional fields omitted.
mkAttributeValue
    :: AttributeValue
mkAttributeValue = AttributeValue'{value = Core.Nothing}

-- | The attribute value. The value is case-sensitive.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avValue :: Lens.Lens' AttributeValue (Core.Maybe Core.Text)
avValue = Lens.field @"value"
{-# INLINEABLE avValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery AttributeValue where
        toQuery AttributeValue{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Value") value

instance Core.FromXML AttributeValue where
        parseXML x = AttributeValue' Core.<$> (x Core..@? "value")
