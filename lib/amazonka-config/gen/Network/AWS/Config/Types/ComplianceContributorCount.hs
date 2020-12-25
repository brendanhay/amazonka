{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceContributorCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceContributorCount
  ( ComplianceContributorCount (..),

    -- * Smart constructor
    mkComplianceContributorCount,

    -- * Lenses
    cccCapExceeded,
    cccCappedCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item, up to a maximum number.
--
-- /See:/ 'mkComplianceContributorCount' smart constructor.
data ComplianceContributorCount = ComplianceContributorCount'
  { -- | Indicates whether the maximum count is reached.
    capExceeded :: Core.Maybe Core.Bool,
    -- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
    cappedCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceContributorCount' value with any optional fields omitted.
mkComplianceContributorCount ::
  ComplianceContributorCount
mkComplianceContributorCount =
  ComplianceContributorCount'
    { capExceeded = Core.Nothing,
      cappedCount = Core.Nothing
    }

-- | Indicates whether the maximum count is reached.
--
-- /Note:/ Consider using 'capExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCapExceeded :: Lens.Lens' ComplianceContributorCount (Core.Maybe Core.Bool)
cccCapExceeded = Lens.field @"capExceeded"
{-# DEPRECATED cccCapExceeded "Use generic-lens or generic-optics with 'capExceeded' instead." #-}

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
--
-- /Note:/ Consider using 'cappedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCappedCount :: Lens.Lens' ComplianceContributorCount (Core.Maybe Core.Int)
cccCappedCount = Lens.field @"cappedCount"
{-# DEPRECATED cccCappedCount "Use generic-lens or generic-optics with 'cappedCount' instead." #-}

instance Core.FromJSON ComplianceContributorCount where
  parseJSON =
    Core.withObject "ComplianceContributorCount" Core.$
      \x ->
        ComplianceContributorCount'
          Core.<$> (x Core..:? "CapExceeded") Core.<*> (x Core..:? "CappedCount")
