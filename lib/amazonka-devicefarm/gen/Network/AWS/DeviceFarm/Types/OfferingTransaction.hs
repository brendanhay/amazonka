{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.OfferingTransaction
  ( OfferingTransaction (..)
  -- * Smart constructor
  , mkOfferingTransaction
  -- * Lenses
  , otCost
  , otCreatedOn
  , otOfferingPromotionId
  , otOfferingStatus
  , otTransactionId
  ) where

import qualified Network.AWS.DeviceFarm.Types.MonetaryAmount as Types
import qualified Network.AWS.DeviceFarm.Types.OfferingPromotionId as Types
import qualified Network.AWS.DeviceFarm.Types.OfferingStatus as Types
import qualified Network.AWS.DeviceFarm.Types.TransactionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the metadata of an offering transaction.
--
-- /See:/ 'mkOfferingTransaction' smart constructor.
data OfferingTransaction = OfferingTransaction'
  { cost :: Core.Maybe Types.MonetaryAmount
    -- ^ The cost of an offering transaction.
  , createdOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date on which an offering transaction was created.
  , offeringPromotionId :: Core.Maybe Types.OfferingPromotionId
    -- ^ The ID that corresponds to a device offering promotion.
  , offeringStatus :: Core.Maybe Types.OfferingStatus
    -- ^ The status of an offering transaction.
  , transactionId :: Core.Maybe Types.TransactionId
    -- ^ The transaction ID of the offering transaction.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OfferingTransaction' value with any optional fields omitted.
mkOfferingTransaction
    :: OfferingTransaction
mkOfferingTransaction
  = OfferingTransaction'{cost = Core.Nothing,
                         createdOn = Core.Nothing, offeringPromotionId = Core.Nothing,
                         offeringStatus = Core.Nothing, transactionId = Core.Nothing}

-- | The cost of an offering transaction.
--
-- /Note:/ Consider using 'cost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otCost :: Lens.Lens' OfferingTransaction (Core.Maybe Types.MonetaryAmount)
otCost = Lens.field @"cost"
{-# INLINEABLE otCost #-}
{-# DEPRECATED cost "Use generic-lens or generic-optics with 'cost' instead"  #-}

-- | The date on which an offering transaction was created.
--
-- /Note:/ Consider using 'createdOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otCreatedOn :: Lens.Lens' OfferingTransaction (Core.Maybe Core.NominalDiffTime)
otCreatedOn = Lens.field @"createdOn"
{-# INLINEABLE otCreatedOn #-}
{-# DEPRECATED createdOn "Use generic-lens or generic-optics with 'createdOn' instead"  #-}

-- | The ID that corresponds to a device offering promotion.
--
-- /Note:/ Consider using 'offeringPromotionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otOfferingPromotionId :: Lens.Lens' OfferingTransaction (Core.Maybe Types.OfferingPromotionId)
otOfferingPromotionId = Lens.field @"offeringPromotionId"
{-# INLINEABLE otOfferingPromotionId #-}
{-# DEPRECATED offeringPromotionId "Use generic-lens or generic-optics with 'offeringPromotionId' instead"  #-}

-- | The status of an offering transaction.
--
-- /Note:/ Consider using 'offeringStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otOfferingStatus :: Lens.Lens' OfferingTransaction (Core.Maybe Types.OfferingStatus)
otOfferingStatus = Lens.field @"offeringStatus"
{-# INLINEABLE otOfferingStatus #-}
{-# DEPRECATED offeringStatus "Use generic-lens or generic-optics with 'offeringStatus' instead"  #-}

-- | The transaction ID of the offering transaction.
--
-- /Note:/ Consider using 'transactionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otTransactionId :: Lens.Lens' OfferingTransaction (Core.Maybe Types.TransactionId)
otTransactionId = Lens.field @"transactionId"
{-# INLINEABLE otTransactionId #-}
{-# DEPRECATED transactionId "Use generic-lens or generic-optics with 'transactionId' instead"  #-}

instance Core.FromJSON OfferingTransaction where
        parseJSON
          = Core.withObject "OfferingTransaction" Core.$
              \ x ->
                OfferingTransaction' Core.<$>
                  (x Core..:? "cost") Core.<*> x Core..:? "createdOn" Core.<*>
                    x Core..:? "offeringPromotionId"
                    Core.<*> x Core..:? "offeringStatus"
                    Core.<*> x Core..:? "transactionId"
