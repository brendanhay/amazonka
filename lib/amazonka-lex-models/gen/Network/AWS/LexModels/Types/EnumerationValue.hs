{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.EnumerationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.EnumerationValue
  ( EnumerationValue (..)
  -- * Smart constructor
  , mkEnumerationValue
  -- * Lenses
  , evValue
  , evSynonyms
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Value as Types
import qualified Network.AWS.Prelude as Core

-- | Each slot type can have a set of values. Each enumeration value represents a value the slot type can take. 
--
-- For example, a pizza ordering bot could have a slot type that specifies the type of crust that the pizza should have. The slot type could include the values 
--
--     * thick
--
--
--     * thin
--
--
--     * stuffed
--
--
--
-- /See:/ 'mkEnumerationValue' smart constructor.
data EnumerationValue = EnumerationValue'
  { value :: Types.Value
    -- ^ The value of the slot type.
  , synonyms :: Core.Maybe [Types.Value]
    -- ^ Additional values related to the slot type value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnumerationValue' value with any optional fields omitted.
mkEnumerationValue
    :: Types.Value -- ^ 'value'
    -> EnumerationValue
mkEnumerationValue value
  = EnumerationValue'{value, synonyms = Core.Nothing}

-- | The value of the slot type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evValue :: Lens.Lens' EnumerationValue Types.Value
evValue = Lens.field @"value"
{-# INLINEABLE evValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | Additional values related to the slot type value.
--
-- /Note:/ Consider using 'synonyms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evSynonyms :: Lens.Lens' EnumerationValue (Core.Maybe [Types.Value])
evSynonyms = Lens.field @"synonyms"
{-# INLINEABLE evSynonyms #-}
{-# DEPRECATED synonyms "Use generic-lens or generic-optics with 'synonyms' instead"  #-}

instance Core.FromJSON EnumerationValue where
        toJSON EnumerationValue{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("value" Core..= value),
                  ("synonyms" Core..=) Core.<$> synonyms])

instance Core.FromJSON EnumerationValue where
        parseJSON
          = Core.withObject "EnumerationValue" Core.$
              \ x ->
                EnumerationValue' Core.<$>
                  (x Core..: "value") Core.<*> x Core..:? "synonyms"
