-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingTransaction
  ( OfferingTransaction (..),

    -- * Smart constructor
    mkOfferingTransaction,

    -- * Lenses
    otOfferingStatus,
    otCost,
    otTransactionId,
    otOfferingPromotionId,
    otCreatedOn,
  )
where

import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.OfferingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the metadata of an offering transaction.
--
-- /See:/ 'mkOfferingTransaction' smart constructor.
data OfferingTransaction = OfferingTransaction'
  { offeringStatus ::
      Lude.Maybe OfferingStatus,
    cost :: Lude.Maybe MonetaryAmount,
    transactionId :: Lude.Maybe Lude.Text,
    offeringPromotionId :: Lude.Maybe Lude.Text,
    createdOn :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OfferingTransaction' with the minimum fields required to make a request.
--
-- * 'cost' - The cost of an offering transaction.
-- * 'createdOn' - The date on which an offering transaction was created.
-- * 'offeringPromotionId' - The ID that corresponds to a device offering promotion.
-- * 'offeringStatus' - The status of an offering transaction.
-- * 'transactionId' - The transaction ID of the offering transaction.
mkOfferingTransaction ::
  OfferingTransaction
mkOfferingTransaction =
  OfferingTransaction'
    { offeringStatus = Lude.Nothing,
      cost = Lude.Nothing,
      transactionId = Lude.Nothing,
      offeringPromotionId = Lude.Nothing,
      createdOn = Lude.Nothing
    }

-- | The status of an offering transaction.
--
-- /Note:/ Consider using 'offeringStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otOfferingStatus :: Lens.Lens' OfferingTransaction (Lude.Maybe OfferingStatus)
otOfferingStatus = Lens.lens (offeringStatus :: OfferingTransaction -> Lude.Maybe OfferingStatus) (\s a -> s {offeringStatus = a} :: OfferingTransaction)
{-# DEPRECATED otOfferingStatus "Use generic-lens or generic-optics with 'offeringStatus' instead." #-}

-- | The cost of an offering transaction.
--
-- /Note:/ Consider using 'cost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otCost :: Lens.Lens' OfferingTransaction (Lude.Maybe MonetaryAmount)
otCost = Lens.lens (cost :: OfferingTransaction -> Lude.Maybe MonetaryAmount) (\s a -> s {cost = a} :: OfferingTransaction)
{-# DEPRECATED otCost "Use generic-lens or generic-optics with 'cost' instead." #-}

-- | The transaction ID of the offering transaction.
--
-- /Note:/ Consider using 'transactionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otTransactionId :: Lens.Lens' OfferingTransaction (Lude.Maybe Lude.Text)
otTransactionId = Lens.lens (transactionId :: OfferingTransaction -> Lude.Maybe Lude.Text) (\s a -> s {transactionId = a} :: OfferingTransaction)
{-# DEPRECATED otTransactionId "Use generic-lens or generic-optics with 'transactionId' instead." #-}

-- | The ID that corresponds to a device offering promotion.
--
-- /Note:/ Consider using 'offeringPromotionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otOfferingPromotionId :: Lens.Lens' OfferingTransaction (Lude.Maybe Lude.Text)
otOfferingPromotionId = Lens.lens (offeringPromotionId :: OfferingTransaction -> Lude.Maybe Lude.Text) (\s a -> s {offeringPromotionId = a} :: OfferingTransaction)
{-# DEPRECATED otOfferingPromotionId "Use generic-lens or generic-optics with 'offeringPromotionId' instead." #-}

-- | The date on which an offering transaction was created.
--
-- /Note:/ Consider using 'createdOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otCreatedOn :: Lens.Lens' OfferingTransaction (Lude.Maybe Lude.Timestamp)
otCreatedOn = Lens.lens (createdOn :: OfferingTransaction -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdOn = a} :: OfferingTransaction)
{-# DEPRECATED otCreatedOn "Use generic-lens or generic-optics with 'createdOn' instead." #-}

instance Lude.FromJSON OfferingTransaction where
  parseJSON =
    Lude.withObject
      "OfferingTransaction"
      ( \x ->
          OfferingTransaction'
            Lude.<$> (x Lude..:? "offeringStatus")
            Lude.<*> (x Lude..:? "cost")
            Lude.<*> (x Lude..:? "transactionId")
            Lude.<*> (x Lude..:? "offeringPromotionId")
            Lude.<*> (x Lude..:? "createdOn")
      )
