{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ProvisionedCapacityDescription
  ( ProvisionedCapacityDescription (..),

    -- * Smart constructor
    mkProvisionedCapacityDescription,

    -- * Lenses
    pcdCapacityId,
    pcdExpirationDate,
    pcdStartDate,
  )
where

import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The definition for a provisioned capacity unit.
--
-- /See:/ 'mkProvisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { -- | The ID that identifies the provisioned capacity unit.
    capacityId :: Core.Maybe Types.String,
    -- | The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
    expirationDate :: Core.Maybe Types.String,
    -- | The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
    startDate :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionedCapacityDescription' value with any optional fields omitted.
mkProvisionedCapacityDescription ::
  ProvisionedCapacityDescription
mkProvisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { capacityId = Core.Nothing,
      expirationDate = Core.Nothing,
      startDate = Core.Nothing
    }

-- | The ID that identifies the provisioned capacity unit.
--
-- /Note:/ Consider using 'capacityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdCapacityId :: Lens.Lens' ProvisionedCapacityDescription (Core.Maybe Types.String)
pcdCapacityId = Lens.field @"capacityId"
{-# DEPRECATED pcdCapacityId "Use generic-lens or generic-optics with 'capacityId' instead." #-}

-- | The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdExpirationDate :: Lens.Lens' ProvisionedCapacityDescription (Core.Maybe Types.String)
pcdExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED pcdExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdStartDate :: Lens.Lens' ProvisionedCapacityDescription (Core.Maybe Types.String)
pcdStartDate = Lens.field @"startDate"
{-# DEPRECATED pcdStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

instance Core.FromJSON ProvisionedCapacityDescription where
  parseJSON =
    Core.withObject "ProvisionedCapacityDescription" Core.$
      \x ->
        ProvisionedCapacityDescription'
          Core.<$> (x Core..:? "CapacityId")
          Core.<*> (x Core..:? "ExpirationDate")
          Core.<*> (x Core..:? "StartDate")
