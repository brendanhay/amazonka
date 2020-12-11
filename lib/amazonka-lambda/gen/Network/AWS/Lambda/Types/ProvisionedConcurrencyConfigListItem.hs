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
    pccliStatus,
    pccliFunctionARN,
    pccliRequestedProvisionedConcurrentExecutions,
    pccliAvailableProvisionedConcurrentExecutions,
    pccliStatusReason,
    pccliAllocatedProvisionedConcurrentExecutions,
    pccliLastModified,
  )
where

import Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the provisioned concurrency configuration for a function alias or version.
--
-- /See:/ 'mkProvisionedConcurrencyConfigListItem' smart constructor.
data ProvisionedConcurrencyConfigListItem = ProvisionedConcurrencyConfigListItem'
  { status ::
      Lude.Maybe
        ProvisionedConcurrencyStatusEnum,
    functionARN ::
      Lude.Maybe
        Lude.Text,
    requestedProvisionedConcurrentExecutions ::
      Lude.Maybe
        Lude.Natural,
    availableProvisionedConcurrentExecutions ::
      Lude.Maybe
        Lude.Natural,
    statusReason ::
      Lude.Maybe
        Lude.Text,
    allocatedProvisionedConcurrentExecutions ::
      Lude.Maybe
        Lude.Natural,
    lastModified ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedConcurrencyConfigListItem' with the minimum fields required to make a request.
--
-- * 'allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
-- * 'availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
-- * 'functionARN' - The Amazon Resource Name (ARN) of the alias or version.
-- * 'lastModified' - The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
-- * 'requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
-- * 'status' - The status of the allocation process.
-- * 'statusReason' - For failed allocations, the reason that provisioned concurrency could not be allocated.
mkProvisionedConcurrencyConfigListItem ::
  ProvisionedConcurrencyConfigListItem
mkProvisionedConcurrencyConfigListItem =
  ProvisionedConcurrencyConfigListItem'
    { status = Lude.Nothing,
      functionARN = Lude.Nothing,
      requestedProvisionedConcurrentExecutions = Lude.Nothing,
      availableProvisionedConcurrentExecutions = Lude.Nothing,
      statusReason = Lude.Nothing,
      allocatedProvisionedConcurrentExecutions = Lude.Nothing,
      lastModified = Lude.Nothing
    }

-- | The status of the allocation process.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliStatus :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Lude.Maybe ProvisionedConcurrencyStatusEnum)
pccliStatus = Lens.lens (status :: ProvisionedConcurrencyConfigListItem -> Lude.Maybe ProvisionedConcurrencyStatusEnum) (\s a -> s {status = a} :: ProvisionedConcurrencyConfigListItem)
{-# DEPRECATED pccliStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the alias or version.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliFunctionARN :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Lude.Maybe Lude.Text)
pccliFunctionARN = Lens.lens (functionARN :: ProvisionedConcurrencyConfigListItem -> Lude.Maybe Lude.Text) (\s a -> s {functionARN = a} :: ProvisionedConcurrencyConfigListItem)
{-# DEPRECATED pccliFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliRequestedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Lude.Maybe Lude.Natural)
pccliRequestedProvisionedConcurrentExecutions = Lens.lens (requestedProvisionedConcurrentExecutions :: ProvisionedConcurrencyConfigListItem -> Lude.Maybe Lude.Natural) (\s a -> s {requestedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)
{-# DEPRECATED pccliRequestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead." #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliAvailableProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Lude.Maybe Lude.Natural)
pccliAvailableProvisionedConcurrentExecutions = Lens.lens (availableProvisionedConcurrentExecutions :: ProvisionedConcurrencyConfigListItem -> Lude.Maybe Lude.Natural) (\s a -> s {availableProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)
{-# DEPRECATED pccliAvailableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead." #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliStatusReason :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Lude.Maybe Lude.Text)
pccliStatusReason = Lens.lens (statusReason :: ProvisionedConcurrencyConfigListItem -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: ProvisionedConcurrencyConfigListItem)
{-# DEPRECATED pccliStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The amount of provisioned concurrency allocated.
--
-- /Note:/ Consider using 'allocatedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliAllocatedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Lude.Maybe Lude.Natural)
pccliAllocatedProvisionedConcurrentExecutions = Lens.lens (allocatedProvisionedConcurrentExecutions :: ProvisionedConcurrencyConfigListItem -> Lude.Maybe Lude.Natural) (\s a -> s {allocatedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)
{-# DEPRECATED pccliAllocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead." #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccliLastModified :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Lude.Maybe Lude.Text)
pccliLastModified = Lens.lens (lastModified :: ProvisionedConcurrencyConfigListItem -> Lude.Maybe Lude.Text) (\s a -> s {lastModified = a} :: ProvisionedConcurrencyConfigListItem)
{-# DEPRECATED pccliLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromJSON ProvisionedConcurrencyConfigListItem where
  parseJSON =
    Lude.withObject
      "ProvisionedConcurrencyConfigListItem"
      ( \x ->
          ProvisionedConcurrencyConfigListItem'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "FunctionArn")
            Lude.<*> (x Lude..:? "RequestedProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..:? "AvailableProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..:? "StatusReason")
            Lude.<*> (x Lude..:? "AllocatedProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..:? "LastModified")
      )
