{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ExecutionControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ExecutionControls
  ( ExecutionControls (..)
  -- * Smart constructor
  , mkExecutionControls
  -- * Lenses
  , ecSsmControls
  ) where

import qualified Network.AWS.Config.Types.SsmControls as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The controls that AWS Config uses for executing remediations.
--
-- /See:/ 'mkExecutionControls' smart constructor.
newtype ExecutionControls = ExecutionControls'
  { ssmControls :: Core.Maybe Types.SsmControls
    -- ^ A SsmControls object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionControls' value with any optional fields omitted.
mkExecutionControls
    :: ExecutionControls
mkExecutionControls
  = ExecutionControls'{ssmControls = Core.Nothing}

-- | A SsmControls object.
--
-- /Note:/ Consider using 'ssmControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecSsmControls :: Lens.Lens' ExecutionControls (Core.Maybe Types.SsmControls)
ecSsmControls = Lens.field @"ssmControls"
{-# INLINEABLE ecSsmControls #-}
{-# DEPRECATED ssmControls "Use generic-lens or generic-optics with 'ssmControls' instead"  #-}

instance Core.FromJSON ExecutionControls where
        toJSON ExecutionControls{..}
          = Core.object
              (Core.catMaybes [("SsmControls" Core..=) Core.<$> ssmControls])

instance Core.FromJSON ExecutionControls where
        parseJSON
          = Core.withObject "ExecutionControls" Core.$
              \ x -> ExecutionControls' Core.<$> (x Core..:? "SsmControls")
