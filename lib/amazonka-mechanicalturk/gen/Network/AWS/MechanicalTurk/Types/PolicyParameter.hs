{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.PolicyParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.PolicyParameter
  ( PolicyParameter (..)
  -- * Smart constructor
  , mkPolicyParameter
  -- * Lenses
  , ppKey
  , ppMapEntries
  , ppValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.ParameterMapEntry as Types
import qualified Network.AWS.Prelude as Core

-- | Name of the parameter from the Review policy. 
--
-- /See:/ 'mkPolicyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { key :: Core.Maybe Core.Text
    -- ^ Name of the parameter from the list of Review Polices. 
  , mapEntries :: Core.Maybe [Types.ParameterMapEntry]
    -- ^ List of ParameterMapEntry objects. 
  , values :: Core.Maybe [Core.Text]
    -- ^ The list of values of the Parameter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyParameter' value with any optional fields omitted.
mkPolicyParameter
    :: PolicyParameter
mkPolicyParameter
  = PolicyParameter'{key = Core.Nothing, mapEntries = Core.Nothing,
                     values = Core.Nothing}

-- | Name of the parameter from the list of Review Polices. 
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppKey :: Lens.Lens' PolicyParameter (Core.Maybe Core.Text)
ppKey = Lens.field @"key"
{-# INLINEABLE ppKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | List of ParameterMapEntry objects. 
--
-- /Note:/ Consider using 'mapEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppMapEntries :: Lens.Lens' PolicyParameter (Core.Maybe [Types.ParameterMapEntry])
ppMapEntries = Lens.field @"mapEntries"
{-# INLINEABLE ppMapEntries #-}
{-# DEPRECATED mapEntries "Use generic-lens or generic-optics with 'mapEntries' instead"  #-}

-- | The list of values of the Parameter
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValues :: Lens.Lens' PolicyParameter (Core.Maybe [Core.Text])
ppValues = Lens.field @"values"
{-# INLINEABLE ppValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON PolicyParameter where
        toJSON PolicyParameter{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key,
                  ("MapEntries" Core..=) Core.<$> mapEntries,
                  ("Values" Core..=) Core.<$> values])

instance Core.FromJSON PolicyParameter where
        parseJSON
          = Core.withObject "PolicyParameter" Core.$
              \ x ->
                PolicyParameter' Core.<$>
                  (x Core..:? "Key") Core.<*> x Core..:? "MapEntries" Core.<*>
                    x Core..:? "Values"
