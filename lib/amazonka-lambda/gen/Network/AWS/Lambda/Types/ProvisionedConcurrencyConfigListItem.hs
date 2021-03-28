{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
  ( ProvisionedConcurrencyConfigListItem (..)
  -- * Smart constructor
  , mkProvisionedConcurrencyConfigListItem
  -- * Lenses
  , pccliAllocatedProvisionedConcurrentExecutions
  , pccliAvailableProvisionedConcurrentExecutions
  , pccliFunctionArn
  , pccliLastModified
  , pccliRequestedProvisionedConcurrentExecutions
  , pccliStatus
  , pccliStatusReason
  ) where

import qualified Network.AWS.Lambda.Types.FunctionArn as Types
import qualified Network.AWS.Lambda.Types.LastModified as Types
import qualified Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the provisioned concurrency configuration for a function alias or version.
--
-- /See:/ 'mkProvisionedConcurrencyConfigListItem' smart constructor.
data ProvisionedConcurrencyConfigListItem = ProvisionedConcurrencyConfigListItem'
  { allocatedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural
    -- ^ The amount of provisioned concurrency allocated.
  , availableProvisionedConcurrentExecutions :: Core.Maybe Core.Natural
    -- ^ The amount of provisioned concurrency available.
  , functionArn :: Core.Maybe Types.FunctionArn
    -- ^ The Amazon Resource Name (ARN) of the alias or version.
  , lastModified :: Core.Maybe Types.LastModified
    -- ^ The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
  , requestedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural
    -- ^ The amount of provisioned concurrency requested.
  , status :: Core.Maybe Types.ProvisionedConcurrencyStatusEnum
    -- ^ The status of the allocation process.
  , statusReason :: Core.Maybe Core.Text
    -- ^ For failed allocations, the reason that provisioned concurrency could not be allocated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionedConcurrencyConfigListItem' value with any optional fields omitted.
mkProvisionedConcurrencyConfigListItem
    :: ProvisionedConcurrencyConfigListItem
mkProvisionedConcurrencyConfigListItem
  = ProvisionedConcurrencyConfigListItem'{allocatedProvisionedConcurrentExecutions
                                            = Core.Nothing,
                                          availableProvisionedConcurrentExecutions = Core.Nothing,
                                          functionArn = Core.Nothing, lastModified = Core.Nothing,
                                          requestedProvisionedConcurrentExecutions = Core.Nothing,
                                          status = Core.Nothing, statusReason = Core.Nothing}

-- | The amount of provisioned concurrency allocated.
--
-- /Note:/ Consider using 'allocatedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliAllocatedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
pccliAllocatedProvisionedConcurrentExecutions = Lens.field @"allocatedProvisionedConcurrentExecutions"
{-# INLINEABLE pccliAllocatedProvisionedConcurrentExecutions #-}
{-# DEPRECATED allocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead"  #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliAvailableProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
pccliAvailableProvisionedConcurrentExecutions = Lens.field @"availableProvisionedConcurrentExecutions"
{-# INLINEABLE pccliAvailableProvisionedConcurrentExecutions #-}
{-# DEPRECATED availableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead"  #-}

-- | The Amazon Resource Name (ARN) of the alias or version.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliFunctionArn :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Types.FunctionArn)
pccliFunctionArn = Lens.field @"functionArn"
{-# INLINEABLE pccliFunctionArn #-}
{-# DEPRECATED functionArn "Use generic-lens or generic-optics with 'functionArn' instead"  #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliLastModified :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Types.LastModified)
pccliLastModified = Lens.field @"lastModified"
{-# INLINEABLE pccliLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliRequestedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
pccliRequestedProvisionedConcurrentExecutions = Lens.field @"requestedProvisionedConcurrentExecutions"
{-# INLINEABLE pccliRequestedProvisionedConcurrentExecutions #-}
{-# DEPRECATED requestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead"  #-}

-- | The status of the allocation process.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliStatus :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Types.ProvisionedConcurrencyStatusEnum)
pccliStatus = Lens.field @"status"
{-# INLINEABLE pccliStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliStatusReason :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Text)
pccliStatusReason = Lens.field @"statusReason"
{-# INLINEABLE pccliStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

instance Core.FromJSON ProvisionedConcurrencyConfigListItem where
        parseJSON
          = Core.withObject "ProvisionedConcurrencyConfigListItem" Core.$
              \ x ->
                ProvisionedConcurrencyConfigListItem' Core.<$>
                  (x Core..:? "AllocatedProvisionedConcurrentExecutions") Core.<*>
                    x Core..:? "AvailableProvisionedConcurrentExecutions"
                    Core.<*> x Core..:? "FunctionArn"
                    Core.<*> x Core..:? "LastModified"
                    Core.<*> x Core..:? "RequestedProvisionedConcurrentExecutions"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusReason"
