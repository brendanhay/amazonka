{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ExecutionProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExecutionProperty
  ( ExecutionProperty (..),

    -- * Smart constructor
    mkExecutionProperty,

    -- * Lenses
    epMaxConcurrentRuns,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An execution property of a job.
--
-- /See:/ 'mkExecutionProperty' smart constructor.
newtype ExecutionProperty = ExecutionProperty'
  { -- | The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
    maxConcurrentRuns :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionProperty' value with any optional fields omitted.
mkExecutionProperty ::
  ExecutionProperty
mkExecutionProperty =
  ExecutionProperty' {maxConcurrentRuns = Core.Nothing}

-- | The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
--
-- /Note:/ Consider using 'maxConcurrentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epMaxConcurrentRuns :: Lens.Lens' ExecutionProperty (Core.Maybe Core.Int)
epMaxConcurrentRuns = Lens.field @"maxConcurrentRuns"
{-# DEPRECATED epMaxConcurrentRuns "Use generic-lens or generic-optics with 'maxConcurrentRuns' instead." #-}

instance Core.FromJSON ExecutionProperty where
  toJSON ExecutionProperty {..} =
    Core.object
      ( Core.catMaybes
          [("MaxConcurrentRuns" Core..=) Core.<$> maxConcurrentRuns]
      )

instance Core.FromJSON ExecutionProperty where
  parseJSON =
    Core.withObject "ExecutionProperty" Core.$
      \x -> ExecutionProperty' Core.<$> (x Core..:? "MaxConcurrentRuns")
