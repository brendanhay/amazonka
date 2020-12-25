{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageResourceResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageResourceResult
  ( UsageResourceResult (..),

    -- * Smart constructor
    mkUsageResourceResult,

    -- * Lenses
    urrResource,
    urrTotal,
  )
where

import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.GuardDuty.Types.Total as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the sum of usage based on an AWS resource.
--
-- /See:/ 'mkUsageResourceResult' smart constructor.
data UsageResourceResult = UsageResourceResult'
  { -- | The AWS resource that generated usage.
    resource :: Core.Maybe Types.String,
    -- | Represents the sum total of usage for the specified resource type.
    total :: Core.Maybe Types.Total
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsageResourceResult' value with any optional fields omitted.
mkUsageResourceResult ::
  UsageResourceResult
mkUsageResourceResult =
  UsageResourceResult'
    { resource = Core.Nothing,
      total = Core.Nothing
    }

-- | The AWS resource that generated usage.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrResource :: Lens.Lens' UsageResourceResult (Core.Maybe Types.String)
urrResource = Lens.field @"resource"
{-# DEPRECATED urrResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | Represents the sum total of usage for the specified resource type.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrTotal :: Lens.Lens' UsageResourceResult (Core.Maybe Types.Total)
urrTotal = Lens.field @"total"
{-# DEPRECATED urrTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Core.FromJSON UsageResourceResult where
  parseJSON =
    Core.withObject "UsageResourceResult" Core.$
      \x ->
        UsageResourceResult'
          Core.<$> (x Core..:? "resource") Core.<*> (x Core..:? "total")
