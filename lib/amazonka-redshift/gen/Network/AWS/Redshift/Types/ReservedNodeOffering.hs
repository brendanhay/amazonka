{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ReservedNodeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ReservedNodeOffering
  ( ReservedNodeOffering (..),

    -- * Smart constructor
    mkReservedNodeOffering,

    -- * Lenses
    rnoReservedNodeOfferingType,
    rnoCurrencyCode,
    rnoReservedNodeOfferingId,
    rnoRecurringCharges,
    rnoOfferingType,
    rnoUsagePrice,
    rnoNodeType,
    rnoFixedPrice,
    rnoDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node offering.
--
-- /See:/ 'mkReservedNodeOffering' smart constructor.
data ReservedNodeOffering = ReservedNodeOffering'
  { -- |
    reservedNodeOfferingType :: Lude.Maybe ReservedNodeOfferingType,
    -- | The currency code for the compute nodes offering.
    currencyCode :: Lude.Maybe Lude.Text,
    -- | The offering identifier.
    reservedNodeOfferingId :: Lude.Maybe Lude.Text,
    -- | The charge to your account regardless of whether you are creating any clusters using the node offering. Recurring charges are only in effect for heavy-utilization reserved nodes.
    recurringCharges :: Lude.Maybe [RecurringCharge],
    -- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
    offeringType :: Lude.Maybe Lude.Text,
    -- | The rate you are charged for each hour the cluster that is using the offering is running.
    usagePrice :: Lude.Maybe Lude.Double,
    -- | The node type offered by the reserved node offering.
    nodeType :: Lude.Maybe Lude.Text,
    -- | The upfront fixed charge you will pay to purchase the specific reserved node offering.
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | The duration, in seconds, for which the offering will reserve the node.
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedNodeOffering' with the minimum fields required to make a request.
--
-- * 'reservedNodeOfferingType' -
-- * 'currencyCode' - The currency code for the compute nodes offering.
-- * 'reservedNodeOfferingId' - The offering identifier.
-- * 'recurringCharges' - The charge to your account regardless of whether you are creating any clusters using the node offering. Recurring charges are only in effect for heavy-utilization reserved nodes.
-- * 'offeringType' - The anticipated utilization of the reserved node, as defined in the reserved node offering.
-- * 'usagePrice' - The rate you are charged for each hour the cluster that is using the offering is running.
-- * 'nodeType' - The node type offered by the reserved node offering.
-- * 'fixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved node offering.
-- * 'duration' - The duration, in seconds, for which the offering will reserve the node.
mkReservedNodeOffering ::
  ReservedNodeOffering
mkReservedNodeOffering =
  ReservedNodeOffering'
    { reservedNodeOfferingType = Lude.Nothing,
      currencyCode = Lude.Nothing,
      reservedNodeOfferingId = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      nodeType = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'reservedNodeOfferingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoReservedNodeOfferingType :: Lens.Lens' ReservedNodeOffering (Lude.Maybe ReservedNodeOfferingType)
rnoReservedNodeOfferingType = Lens.lens (reservedNodeOfferingType :: ReservedNodeOffering -> Lude.Maybe ReservedNodeOfferingType) (\s a -> s {reservedNodeOfferingType = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoReservedNodeOfferingType "Use generic-lens or generic-optics with 'reservedNodeOfferingType' instead." #-}

-- | The currency code for the compute nodes offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoCurrencyCode :: Lens.Lens' ReservedNodeOffering (Lude.Maybe Lude.Text)
rnoCurrencyCode = Lens.lens (currencyCode :: ReservedNodeOffering -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoReservedNodeOfferingId :: Lens.Lens' ReservedNodeOffering (Lude.Maybe Lude.Text)
rnoReservedNodeOfferingId = Lens.lens (reservedNodeOfferingId :: ReservedNodeOffering -> Lude.Maybe Lude.Text) (\s a -> s {reservedNodeOfferingId = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoReservedNodeOfferingId "Use generic-lens or generic-optics with 'reservedNodeOfferingId' instead." #-}

-- | The charge to your account regardless of whether you are creating any clusters using the node offering. Recurring charges are only in effect for heavy-utilization reserved nodes.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoRecurringCharges :: Lens.Lens' ReservedNodeOffering (Lude.Maybe [RecurringCharge])
rnoRecurringCharges = Lens.lens (recurringCharges :: ReservedNodeOffering -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoOfferingType :: Lens.Lens' ReservedNodeOffering (Lude.Maybe Lude.Text)
rnoOfferingType = Lens.lens (offeringType :: ReservedNodeOffering -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The rate you are charged for each hour the cluster that is using the offering is running.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoUsagePrice :: Lens.Lens' ReservedNodeOffering (Lude.Maybe Lude.Double)
rnoUsagePrice = Lens.lens (usagePrice :: ReservedNodeOffering -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The node type offered by the reserved node offering.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoNodeType :: Lens.Lens' ReservedNodeOffering (Lude.Maybe Lude.Text)
rnoNodeType = Lens.lens (nodeType :: ReservedNodeOffering -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The upfront fixed charge you will pay to purchase the specific reserved node offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoFixedPrice :: Lens.Lens' ReservedNodeOffering (Lude.Maybe Lude.Double)
rnoFixedPrice = Lens.lens (fixedPrice :: ReservedNodeOffering -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration, in seconds, for which the offering will reserve the node.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnoDuration :: Lens.Lens' ReservedNodeOffering (Lude.Maybe Lude.Int)
rnoDuration = Lens.lens (duration :: ReservedNodeOffering -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedNodeOffering)
{-# DEPRECATED rnoDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromXML ReservedNodeOffering where
  parseXML x =
    ReservedNodeOffering'
      Lude.<$> (x Lude..@? "ReservedNodeOfferingType")
      Lude.<*> (x Lude..@? "CurrencyCode")
      Lude.<*> (x Lude..@? "ReservedNodeOfferingId")
      Lude.<*> ( x Lude..@? "RecurringCharges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "RecurringCharge")
               )
      Lude.<*> (x Lude..@? "OfferingType")
      Lude.<*> (x Lude..@? "UsagePrice")
      Lude.<*> (x Lude..@? "NodeType")
      Lude.<*> (x Lude..@? "FixedPrice")
      Lude.<*> (x Lude..@? "Duration")
