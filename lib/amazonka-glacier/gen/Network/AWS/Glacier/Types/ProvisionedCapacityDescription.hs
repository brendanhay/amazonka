{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.ProvisionedCapacityDescription
  ( ProvisionedCapacityDescription (..)
  -- * Smart constructor
  , mkProvisionedCapacityDescription
  -- * Lenses
  , pcdCapacityId
  , pcdExpirationDate
  , pcdStartDate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The definition for a provisioned capacity unit.
--
-- /See:/ 'mkProvisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { capacityId :: Core.Maybe Core.Text
    -- ^ The ID that identifies the provisioned capacity unit.
  , expirationDate :: Core.Maybe Core.Text
    -- ^ The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
  , startDate :: Core.Maybe Core.Text
    -- ^ The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionedCapacityDescription' value with any optional fields omitted.
mkProvisionedCapacityDescription
    :: ProvisionedCapacityDescription
mkProvisionedCapacityDescription
  = ProvisionedCapacityDescription'{capacityId = Core.Nothing,
                                    expirationDate = Core.Nothing, startDate = Core.Nothing}

-- | The ID that identifies the provisioned capacity unit.
--
-- /Note:/ Consider using 'capacityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdCapacityId :: Lens.Lens' ProvisionedCapacityDescription (Core.Maybe Core.Text)
pcdCapacityId = Lens.field @"capacityId"
{-# INLINEABLE pcdCapacityId #-}
{-# DEPRECATED capacityId "Use generic-lens or generic-optics with 'capacityId' instead"  #-}

-- | The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdExpirationDate :: Lens.Lens' ProvisionedCapacityDescription (Core.Maybe Core.Text)
pcdExpirationDate = Lens.field @"expirationDate"
{-# INLINEABLE pcdExpirationDate #-}
{-# DEPRECATED expirationDate "Use generic-lens or generic-optics with 'expirationDate' instead"  #-}

-- | The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdStartDate :: Lens.Lens' ProvisionedCapacityDescription (Core.Maybe Core.Text)
pcdStartDate = Lens.field @"startDate"
{-# INLINEABLE pcdStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

instance Core.FromJSON ProvisionedCapacityDescription where
        parseJSON
          = Core.withObject "ProvisionedCapacityDescription" Core.$
              \ x ->
                ProvisionedCapacityDescription' Core.<$>
                  (x Core..:? "CapacityId") Core.<*> x Core..:? "ExpirationDate"
                    Core.<*> x Core..:? "StartDate"
