{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.EngineAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.EngineAttribute
  ( EngineAttribute (..)
  -- * Smart constructor
  , mkEngineAttribute
  -- * Lenses
  , eaName
  , eaValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types.EngineAttributeName as Types
import qualified Network.AWS.OpsWorksCM.Types.Value as Types
import qualified Network.AWS.Prelude as Core

-- | A name and value pair that is specific to the engine of the server. 
--
-- /See:/ 'mkEngineAttribute' smart constructor.
data EngineAttribute = EngineAttribute'
  { name :: Core.Maybe Types.EngineAttributeName
    -- ^ The name of the engine attribute. 
  , value :: Core.Maybe Types.Value
    -- ^ The value of the engine attribute. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EngineAttribute' value with any optional fields omitted.
mkEngineAttribute
    :: EngineAttribute
mkEngineAttribute
  = EngineAttribute'{name = Core.Nothing, value = Core.Nothing}

-- | The name of the engine attribute. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaName :: Lens.Lens' EngineAttribute (Core.Maybe Types.EngineAttributeName)
eaName = Lens.field @"name"
{-# INLINEABLE eaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the engine attribute. 
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaValue :: Lens.Lens' EngineAttribute (Core.Maybe Types.Value)
eaValue = Lens.field @"value"
{-# INLINEABLE eaValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON EngineAttribute where
        toJSON EngineAttribute{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name, ("Value" Core..=) Core.<$> value])

instance Core.FromJSON EngineAttribute where
        parseJSON
          = Core.withObject "EngineAttribute" Core.$
              \ x ->
                EngineAttribute' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Value"
