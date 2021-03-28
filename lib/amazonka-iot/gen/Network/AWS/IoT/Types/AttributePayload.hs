{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AttributePayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AttributePayload
  ( AttributePayload (..)
  -- * Smart constructor
  , mkAttributePayload
  -- * Lenses
  , apAttributes
  , apMerge
  ) where

import qualified Network.AWS.IoT.Types.AttributeName as Types
import qualified Network.AWS.IoT.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The attribute payload.
--
-- /See:/ 'mkAttributePayload' smart constructor.
data AttributePayload = AttributePayload'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A JSON string containing up to three key-value pair in JSON format. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@ 
  , merge :: Core.Maybe Core.Bool
    -- ^ Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them.
--
-- To remove an attribute, call @UpdateThing@ with an empty attribute value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributePayload' value with any optional fields omitted.
mkAttributePayload
    :: AttributePayload
mkAttributePayload
  = AttributePayload'{attributes = Core.Nothing,
                      merge = Core.Nothing}

-- | A JSON string containing up to three key-value pair in JSON format. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@ 
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAttributes :: Lens.Lens' AttributePayload (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
apAttributes = Lens.field @"attributes"
{-# INLINEABLE apAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them.
--
-- To remove an attribute, call @UpdateThing@ with an empty attribute value.
--
-- /Note:/ Consider using 'merge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apMerge :: Lens.Lens' AttributePayload (Core.Maybe Core.Bool)
apMerge = Lens.field @"merge"
{-# INLINEABLE apMerge #-}
{-# DEPRECATED merge "Use generic-lens or generic-optics with 'merge' instead"  #-}

instance Core.FromJSON AttributePayload where
        toJSON AttributePayload{..}
          = Core.object
              (Core.catMaybes
                 [("attributes" Core..=) Core.<$> attributes,
                  ("merge" Core..=) Core.<$> merge])

instance Core.FromJSON AttributePayload where
        parseJSON
          = Core.withObject "AttributePayload" Core.$
              \ x ->
                AttributePayload' Core.<$>
                  (x Core..:? "attributes") Core.<*> x Core..:? "merge"
