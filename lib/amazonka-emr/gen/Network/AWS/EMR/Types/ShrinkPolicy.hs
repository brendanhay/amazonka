{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ShrinkPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ShrinkPolicy
  ( ShrinkPolicy (..)
  -- * Smart constructor
  , mkShrinkPolicy
  -- * Lenses
  , spDecommissionTimeout
  , spInstanceResizePolicy
  ) where

import qualified Network.AWS.EMR.Types.InstanceResizePolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Policy for customizing shrink operations. Allows configuration of decommissioning timeout and targeted instance shrinking.
--
-- /See:/ 'mkShrinkPolicy' smart constructor.
data ShrinkPolicy = ShrinkPolicy'
  { decommissionTimeout :: Core.Maybe Core.Int
    -- ^ The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
  , instanceResizePolicy :: Core.Maybe Types.InstanceResizePolicy
    -- ^ Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShrinkPolicy' value with any optional fields omitted.
mkShrinkPolicy
    :: ShrinkPolicy
mkShrinkPolicy
  = ShrinkPolicy'{decommissionTimeout = Core.Nothing,
                  instanceResizePolicy = Core.Nothing}

-- | The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
--
-- /Note:/ Consider using 'decommissionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDecommissionTimeout :: Lens.Lens' ShrinkPolicy (Core.Maybe Core.Int)
spDecommissionTimeout = Lens.field @"decommissionTimeout"
{-# INLINEABLE spDecommissionTimeout #-}
{-# DEPRECATED decommissionTimeout "Use generic-lens or generic-optics with 'decommissionTimeout' instead"  #-}

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
--
-- /Note:/ Consider using 'instanceResizePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spInstanceResizePolicy :: Lens.Lens' ShrinkPolicy (Core.Maybe Types.InstanceResizePolicy)
spInstanceResizePolicy = Lens.field @"instanceResizePolicy"
{-# INLINEABLE spInstanceResizePolicy #-}
{-# DEPRECATED instanceResizePolicy "Use generic-lens or generic-optics with 'instanceResizePolicy' instead"  #-}

instance Core.FromJSON ShrinkPolicy where
        toJSON ShrinkPolicy{..}
          = Core.object
              (Core.catMaybes
                 [("DecommissionTimeout" Core..=) Core.<$> decommissionTimeout,
                  ("InstanceResizePolicy" Core..=) Core.<$> instanceResizePolicy])

instance Core.FromJSON ShrinkPolicy where
        parseJSON
          = Core.withObject "ShrinkPolicy" Core.$
              \ x ->
                ShrinkPolicy' Core.<$>
                  (x Core..:? "DecommissionTimeout") Core.<*>
                    x Core..:? "InstanceResizePolicy"
