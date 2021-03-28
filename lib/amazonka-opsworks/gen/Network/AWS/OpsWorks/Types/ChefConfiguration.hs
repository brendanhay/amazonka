{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ChefConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.ChefConfiguration
  ( ChefConfiguration (..)
  -- * Smart constructor
  , mkChefConfiguration
  -- * Lenses
  , ccBerkshelfVersion
  , ccManageBerkshelf
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Chef configuration.
--
-- /See:/ 'mkChefConfiguration' smart constructor.
data ChefConfiguration = ChefConfiguration'
  { berkshelfVersion :: Core.Maybe Core.Text
    -- ^ The Berkshelf version.
  , manageBerkshelf :: Core.Maybe Core.Bool
    -- ^ Whether to enable Berkshelf.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChefConfiguration' value with any optional fields omitted.
mkChefConfiguration
    :: ChefConfiguration
mkChefConfiguration
  = ChefConfiguration'{berkshelfVersion = Core.Nothing,
                       manageBerkshelf = Core.Nothing}

-- | The Berkshelf version.
--
-- /Note:/ Consider using 'berkshelfVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBerkshelfVersion :: Lens.Lens' ChefConfiguration (Core.Maybe Core.Text)
ccBerkshelfVersion = Lens.field @"berkshelfVersion"
{-# INLINEABLE ccBerkshelfVersion #-}
{-# DEPRECATED berkshelfVersion "Use generic-lens or generic-optics with 'berkshelfVersion' instead"  #-}

-- | Whether to enable Berkshelf.
--
-- /Note:/ Consider using 'manageBerkshelf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccManageBerkshelf :: Lens.Lens' ChefConfiguration (Core.Maybe Core.Bool)
ccManageBerkshelf = Lens.field @"manageBerkshelf"
{-# INLINEABLE ccManageBerkshelf #-}
{-# DEPRECATED manageBerkshelf "Use generic-lens or generic-optics with 'manageBerkshelf' instead"  #-}

instance Core.FromJSON ChefConfiguration where
        toJSON ChefConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("BerkshelfVersion" Core..=) Core.<$> berkshelfVersion,
                  ("ManageBerkshelf" Core..=) Core.<$> manageBerkshelf])

instance Core.FromJSON ChefConfiguration where
        parseJSON
          = Core.withObject "ChefConfiguration" Core.$
              \ x ->
                ChefConfiguration' Core.<$>
                  (x Core..:? "BerkshelfVersion") Core.<*>
                    x Core..:? "ManageBerkshelf"
