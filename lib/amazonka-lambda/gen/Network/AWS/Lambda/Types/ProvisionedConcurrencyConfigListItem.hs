{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
  ( ProvisionedConcurrencyConfigListItem (..),

    -- * Smart constructor
    mkProvisionedConcurrencyConfigListItem,

    -- * Lenses
    pccliAllocatedProvisionedConcurrentExecutions,
    pccliAvailableProvisionedConcurrentExecutions,
    pccliFunctionArn,
    pccliLastModified,
    pccliRequestedProvisionedConcurrentExecutions,
    pccliStatus,
    pccliStatusReason,
  )
where

import qualified Network.AWS.Lambda.Types.FunctionArn as Types
import qualified Network.AWS.Lambda.Types.LastModified as Types
import qualified Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum as Types
import qualified Network.AWS.Lambda.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the provisioned concurrency configuration for a function alias or version.
--
-- /See:/ 'mkProvisionedConcurrencyConfigListItem' smart constructor.
data ProvisionedConcurrencyConfigListItem = ProvisionedConcurrencyConfigListItem'
  { -- | The amount of provisioned concurrency allocated.
    allocatedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the alias or version.
    functionArn :: Core.Maybe Types.FunctionArn,
    -- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
    lastModified :: Core.Maybe Types.LastModified,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The status of the allocation process.
    status :: Core.Maybe Types.ProvisionedConcurrencyStatusEnum,
    -- | For failed allocations, the reason that provisioned concurrency could not be allocated.
    statusReason :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionedConcurrencyConfigListItem' value with any optional fields omitted.
mkProvisionedConcurrencyConfigListItem ::
  ProvisionedConcurrencyConfigListItem
mkProvisionedConcurrencyConfigListItem =
  ProvisionedConcurrencyConfigListItem'
    { allocatedProvisionedConcurrentExecutions =
        Core.Nothing,
      availableProvisionedConcurrentExecutions = Core.Nothing,
      functionArn = Core.Nothing,
      lastModified = Core.Nothing,
      requestedProvisionedConcurrentExecutions = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The amount of provisioned concurrency allocated.
--
-- /Note:/ Consider using 'allocatedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliAllocatedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
pccliAllocatedProvisionedConcurrentExecutions = Lens.field @"allocatedProvisionedConcurrentExecutions"
{-# DEPRECATED pccliAllocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead." #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliAvailableProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
pccliAvailableProvisionedConcurrentExecutions = Lens.field @"availableProvisionedConcurrentExecutions"
{-# DEPRECATED pccliAvailableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead." #-}

-- | The Amazon Resource Name (ARN) of the alias or version.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliFunctionArn :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Types.FunctionArn)
pccliFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED pccliFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliLastModified :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Types.LastModified)
pccliLastModified = Lens.field @"lastModified"
{-# DEPRECATED pccliLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliRequestedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
pccliRequestedProvisionedConcurrentExecutions = Lens.field @"requestedProvisionedConcurrentExecutions"
{-# DEPRECATED pccliRequestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead." #-}

-- | The status of the allocation process.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliStatus :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Types.ProvisionedConcurrencyStatusEnum)
pccliStatus = Lens.field @"status"
{-# DEPRECATED pccliStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliStatusReason :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Types.String)
pccliStatusReason = Lens.field @"statusReason"
{-# DEPRECATED pccliStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Core.FromJSON ProvisionedConcurrencyConfigListItem where
  parseJSON =
    Core.withObject "ProvisionedConcurrencyConfigListItem" Core.$
      \x ->
        ProvisionedConcurrencyConfigListItem'
          Core.<$> (x Core..:? "AllocatedProvisionedConcurrentExecutions")
          Core.<*> (x Core..:? "AvailableProvisionedConcurrentExecutions")
          Core.<*> (x Core..:? "FunctionArn")
          Core.<*> (x Core..:? "LastModified")
          Core.<*> (x Core..:? "RequestedProvisionedConcurrentExecutions")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusReason")
