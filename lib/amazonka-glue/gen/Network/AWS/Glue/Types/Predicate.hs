{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Predicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Predicate
  ( Predicate (..)
  -- * Smart constructor
  , mkPredicate
  -- * Lenses
  , pConditions
  , pLogical
  ) where

import qualified Network.AWS.Glue.Types.Condition as Types
import qualified Network.AWS.Glue.Types.Logical as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines the predicate of the trigger, which determines when it fires.
--
-- /See:/ 'mkPredicate' smart constructor.
data Predicate = Predicate'
  { conditions :: Core.Maybe [Types.Condition]
    -- ^ A list of the conditions that determine when the trigger will fire.
  , logical :: Core.Maybe Types.Logical
    -- ^ An optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Predicate' value with any optional fields omitted.
mkPredicate
    :: Predicate
mkPredicate
  = Predicate'{conditions = Core.Nothing, logical = Core.Nothing}

-- | A list of the conditions that determine when the trigger will fire.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pConditions :: Lens.Lens' Predicate (Core.Maybe [Types.Condition])
pConditions = Lens.field @"conditions"
{-# INLINEABLE pConditions #-}
{-# DEPRECATED conditions "Use generic-lens or generic-optics with 'conditions' instead"  #-}

-- | An optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
--
-- /Note:/ Consider using 'logical' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLogical :: Lens.Lens' Predicate (Core.Maybe Types.Logical)
pLogical = Lens.field @"logical"
{-# INLINEABLE pLogical #-}
{-# DEPRECATED logical "Use generic-lens or generic-optics with 'logical' instead"  #-}

instance Core.FromJSON Predicate where
        toJSON Predicate{..}
          = Core.object
              (Core.catMaybes
                 [("Conditions" Core..=) Core.<$> conditions,
                  ("Logical" Core..=) Core.<$> logical])

instance Core.FromJSON Predicate where
        parseJSON
          = Core.withObject "Predicate" Core.$
              \ x ->
                Predicate' Core.<$>
                  (x Core..:? "Conditions") Core.<*> x Core..:? "Logical"
