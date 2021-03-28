{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UsageInstruction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.UsageInstruction
  ( UsageInstruction (..)
  -- * Smart constructor
  , mkUsageInstruction
  -- * Lenses
  , uiType
  , uiValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.InstructionType as Types
import qualified Network.AWS.ServiceCatalog.Types.Value as Types

-- | Additional information provided by the administrator.
--
-- /See:/ 'mkUsageInstruction' smart constructor.
data UsageInstruction = UsageInstruction'
  { type' :: Core.Maybe Types.InstructionType
    -- ^ The usage instruction type for the value.
  , value :: Core.Maybe Types.Value
    -- ^ The usage instruction value for this type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsageInstruction' value with any optional fields omitted.
mkUsageInstruction
    :: UsageInstruction
mkUsageInstruction
  = UsageInstruction'{type' = Core.Nothing, value = Core.Nothing}

-- | The usage instruction type for the value.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiType :: Lens.Lens' UsageInstruction (Core.Maybe Types.InstructionType)
uiType = Lens.field @"type'"
{-# INLINEABLE uiType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The usage instruction value for this type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiValue :: Lens.Lens' UsageInstruction (Core.Maybe Types.Value)
uiValue = Lens.field @"value"
{-# INLINEABLE uiValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON UsageInstruction where
        parseJSON
          = Core.withObject "UsageInstruction" Core.$
              \ x ->
                UsageInstruction' Core.<$>
                  (x Core..:? "Type") Core.<*> x Core..:? "Value"
