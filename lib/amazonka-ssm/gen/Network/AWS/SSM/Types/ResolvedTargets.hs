{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResolvedTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResolvedTargets
  ( ResolvedTargets (..)
  -- * Smart constructor
  , mkResolvedTargets
  -- * Lenses
  , rtParameterValues
  , rtTruncated
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ParameterValue as Types

-- | Information about targets that resolved during the Automation execution.
--
-- /See:/ 'mkResolvedTargets' smart constructor.
data ResolvedTargets = ResolvedTargets'
  { parameterValues :: Core.Maybe [Types.ParameterValue]
    -- ^ A list of parameter values sent to targets that resolved during the Automation execution.
  , truncated :: Core.Maybe Core.Bool
    -- ^ A boolean value indicating whether the resolved target list is truncated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolvedTargets' value with any optional fields omitted.
mkResolvedTargets
    :: ResolvedTargets
mkResolvedTargets
  = ResolvedTargets'{parameterValues = Core.Nothing,
                     truncated = Core.Nothing}

-- | A list of parameter values sent to targets that resolved during the Automation execution.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtParameterValues :: Lens.Lens' ResolvedTargets (Core.Maybe [Types.ParameterValue])
rtParameterValues = Lens.field @"parameterValues"
{-# INLINEABLE rtParameterValues #-}
{-# DEPRECATED parameterValues "Use generic-lens or generic-optics with 'parameterValues' instead"  #-}

-- | A boolean value indicating whether the resolved target list is truncated.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTruncated :: Lens.Lens' ResolvedTargets (Core.Maybe Core.Bool)
rtTruncated = Lens.field @"truncated"
{-# INLINEABLE rtTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

instance Core.FromJSON ResolvedTargets where
        parseJSON
          = Core.withObject "ResolvedTargets" Core.$
              \ x ->
                ResolvedTargets' Core.<$>
                  (x Core..:? "ParameterValues") Core.<*> x Core..:? "Truncated"
