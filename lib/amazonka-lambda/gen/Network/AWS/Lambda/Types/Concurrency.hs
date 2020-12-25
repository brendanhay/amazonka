{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Concurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Concurrency
  ( Concurrency (..),

    -- * Smart constructor
    mkConcurrency,

    -- * Lenses
    cReservedConcurrentExecutions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkConcurrency' smart constructor.
newtype Concurrency = Concurrency'
  { -- | The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
    reservedConcurrentExecutions :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Concurrency' value with any optional fields omitted.
mkConcurrency ::
  Concurrency
mkConcurrency =
  Concurrency' {reservedConcurrentExecutions = Core.Nothing}

-- | The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
--
-- /Note:/ Consider using 'reservedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReservedConcurrentExecutions :: Lens.Lens' Concurrency (Core.Maybe Core.Natural)
cReservedConcurrentExecutions = Lens.field @"reservedConcurrentExecutions"
{-# DEPRECATED cReservedConcurrentExecutions "Use generic-lens or generic-optics with 'reservedConcurrentExecutions' instead." #-}

instance Core.FromJSON Concurrency where
  parseJSON =
    Core.withObject "Concurrency" Core.$
      \x ->
        Concurrency' Core.<$> (x Core..:? "ReservedConcurrentExecutions")
