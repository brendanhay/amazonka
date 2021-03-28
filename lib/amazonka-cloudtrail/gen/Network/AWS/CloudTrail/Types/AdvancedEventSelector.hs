{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.AdvancedEventSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.AdvancedEventSelector
  ( AdvancedEventSelector (..)
  -- * Smart constructor
  , mkAdvancedEventSelector
  -- * Lenses
  , aesName
  , aesFieldSelectors
  ) where

import qualified Network.AWS.CloudTrail.Types.AdvancedFieldSelector as Types
import qualified Network.AWS.CloudTrail.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkAdvancedEventSelector' smart constructor.
data AdvancedEventSelector = AdvancedEventSelector'
  { name :: Types.Name
  , fieldSelectors :: Core.NonEmpty Types.AdvancedFieldSelector
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdvancedEventSelector' value with any optional fields omitted.
mkAdvancedEventSelector
    :: Types.Name -- ^ 'name'
    -> Core.NonEmpty Types.AdvancedFieldSelector -- ^ 'fieldSelectors'
    -> AdvancedEventSelector
mkAdvancedEventSelector name fieldSelectors
  = AdvancedEventSelector'{name, fieldSelectors}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesName :: Lens.Lens' AdvancedEventSelector Types.Name
aesName = Lens.field @"name"
{-# INLINEABLE aesName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fieldSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesFieldSelectors :: Lens.Lens' AdvancedEventSelector (Core.NonEmpty Types.AdvancedFieldSelector)
aesFieldSelectors = Lens.field @"fieldSelectors"
{-# INLINEABLE aesFieldSelectors #-}
{-# DEPRECATED fieldSelectors "Use generic-lens or generic-optics with 'fieldSelectors' instead"  #-}

instance Core.FromJSON AdvancedEventSelector where
        toJSON AdvancedEventSelector{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("FieldSelectors" Core..= fieldSelectors)])

instance Core.FromJSON AdvancedEventSelector where
        parseJSON
          = Core.withObject "AdvancedEventSelector" Core.$
              \ x ->
                AdvancedEventSelector' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "FieldSelectors"
