{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
  ( ProvisionedThroughputOverride (..),

    -- * Smart constructor
    mkProvisionedThroughputOverride,

    -- * Lenses
    ptoReadCapacityUnits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Replica-specific provisioned throughput settings. If not specified, uses the source table's provisioned throughput settings.
--
-- /See:/ 'mkProvisionedThroughputOverride' smart constructor.
newtype ProvisionedThroughputOverride = ProvisionedThroughputOverride'
  { -- | Replica-specific read capacity units. If not specified, uses the source table's read capacity settings.
    readCapacityUnits :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionedThroughputOverride' value with any optional fields omitted.
mkProvisionedThroughputOverride ::
  ProvisionedThroughputOverride
mkProvisionedThroughputOverride =
  ProvisionedThroughputOverride' {readCapacityUnits = Core.Nothing}

-- | Replica-specific read capacity units. If not specified, uses the source table's read capacity settings.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptoReadCapacityUnits :: Lens.Lens' ProvisionedThroughputOverride (Core.Maybe Core.Natural)
ptoReadCapacityUnits = Lens.field @"readCapacityUnits"
{-# DEPRECATED ptoReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

instance Core.FromJSON ProvisionedThroughputOverride where
  toJSON ProvisionedThroughputOverride {..} =
    Core.object
      ( Core.catMaybes
          [("ReadCapacityUnits" Core..=) Core.<$> readCapacityUnits]
      )

instance Core.FromJSON ProvisionedThroughputOverride where
  parseJSON =
    Core.withObject "ProvisionedThroughputOverride" Core.$
      \x ->
        ProvisionedThroughputOverride'
          Core.<$> (x Core..:? "ReadCapacityUnits")
