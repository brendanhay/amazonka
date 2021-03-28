{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
  ( MinimumEngineVersionPerAllowedValue (..)
  -- * Smart constructor
  , mkMinimumEngineVersionPerAllowedValue
  -- * Lenses
  , mevpavAllowedValue
  , mevpavMinimumEngineVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The minimum DB engine version required for each corresponding allowed value for an option setting.
--
-- /See:/ 'mkMinimumEngineVersionPerAllowedValue' smart constructor.
data MinimumEngineVersionPerAllowedValue = MinimumEngineVersionPerAllowedValue'
  { allowedValue :: Core.Maybe Core.Text
    -- ^ The allowed value for an option setting.
  , minimumEngineVersion :: Core.Maybe Core.Text
    -- ^ The minimum DB engine version required for the allowed value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MinimumEngineVersionPerAllowedValue' value with any optional fields omitted.
mkMinimumEngineVersionPerAllowedValue
    :: MinimumEngineVersionPerAllowedValue
mkMinimumEngineVersionPerAllowedValue
  = MinimumEngineVersionPerAllowedValue'{allowedValue = Core.Nothing,
                                         minimumEngineVersion = Core.Nothing}

-- | The allowed value for an option setting.
--
-- /Note:/ Consider using 'allowedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mevpavAllowedValue :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Core.Maybe Core.Text)
mevpavAllowedValue = Lens.field @"allowedValue"
{-# INLINEABLE mevpavAllowedValue #-}
{-# DEPRECATED allowedValue "Use generic-lens or generic-optics with 'allowedValue' instead"  #-}

-- | The minimum DB engine version required for the allowed value.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mevpavMinimumEngineVersion :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Core.Maybe Core.Text)
mevpavMinimumEngineVersion = Lens.field @"minimumEngineVersion"
{-# INLINEABLE mevpavMinimumEngineVersion #-}
{-# DEPRECATED minimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead"  #-}

instance Core.FromXML MinimumEngineVersionPerAllowedValue where
        parseXML x
          = MinimumEngineVersionPerAllowedValue' Core.<$>
              (x Core..@? "AllowedValue") Core.<*>
                x Core..@? "MinimumEngineVersion"
