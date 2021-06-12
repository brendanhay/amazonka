{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ReservedNodeOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ReservedNodeOffering where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node offering.
--
-- /See:/ 'newReservedNodeOffering' smart constructor.
data ReservedNodeOffering = ReservedNodeOffering'
  { reservedNodeOfferingType :: Core.Maybe ReservedNodeOfferingType,
    -- | The offering identifier.
    reservedNodeOfferingId :: Core.Maybe Core.Text,
    -- | The duration, in seconds, for which the offering will reserve the node.
    duration :: Core.Maybe Core.Int,
    -- | The currency code for the compute nodes offering.
    currencyCode :: Core.Maybe Core.Text,
    -- | The upfront fixed charge you will pay to purchase the specific reserved
    -- node offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The rate you are charged for each hour the cluster that is using the
    -- offering is running.
    usagePrice :: Core.Maybe Core.Double,
    -- | The anticipated utilization of the reserved node, as defined in the
    -- reserved node offering.
    offeringType :: Core.Maybe Core.Text,
    -- | The node type offered by the reserved node offering.
    nodeType :: Core.Maybe Core.Text,
    -- | The charge to your account regardless of whether you are creating any
    -- clusters using the node offering. Recurring charges are only in effect
    -- for heavy-utilization reserved nodes.
    recurringCharges :: Core.Maybe [RecurringCharge]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedNodeOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeOfferingType', 'reservedNodeOffering_reservedNodeOfferingType' -
--
-- 'reservedNodeOfferingId', 'reservedNodeOffering_reservedNodeOfferingId' - The offering identifier.
--
-- 'duration', 'reservedNodeOffering_duration' - The duration, in seconds, for which the offering will reserve the node.
--
-- 'currencyCode', 'reservedNodeOffering_currencyCode' - The currency code for the compute nodes offering.
--
-- 'fixedPrice', 'reservedNodeOffering_fixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
--
-- 'usagePrice', 'reservedNodeOffering_usagePrice' - The rate you are charged for each hour the cluster that is using the
-- offering is running.
--
-- 'offeringType', 'reservedNodeOffering_offeringType' - The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
--
-- 'nodeType', 'reservedNodeOffering_nodeType' - The node type offered by the reserved node offering.
--
-- 'recurringCharges', 'reservedNodeOffering_recurringCharges' - The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect
-- for heavy-utilization reserved nodes.
newReservedNodeOffering ::
  ReservedNodeOffering
newReservedNodeOffering =
  ReservedNodeOffering'
    { reservedNodeOfferingType =
        Core.Nothing,
      reservedNodeOfferingId = Core.Nothing,
      duration = Core.Nothing,
      currencyCode = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      nodeType = Core.Nothing,
      recurringCharges = Core.Nothing
    }

-- |
reservedNodeOffering_reservedNodeOfferingType :: Lens.Lens' ReservedNodeOffering (Core.Maybe ReservedNodeOfferingType)
reservedNodeOffering_reservedNodeOfferingType = Lens.lens (\ReservedNodeOffering' {reservedNodeOfferingType} -> reservedNodeOfferingType) (\s@ReservedNodeOffering' {} a -> s {reservedNodeOfferingType = a} :: ReservedNodeOffering)

-- | The offering identifier.
reservedNodeOffering_reservedNodeOfferingId :: Lens.Lens' ReservedNodeOffering (Core.Maybe Core.Text)
reservedNodeOffering_reservedNodeOfferingId = Lens.lens (\ReservedNodeOffering' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@ReservedNodeOffering' {} a -> s {reservedNodeOfferingId = a} :: ReservedNodeOffering)

-- | The duration, in seconds, for which the offering will reserve the node.
reservedNodeOffering_duration :: Lens.Lens' ReservedNodeOffering (Core.Maybe Core.Int)
reservedNodeOffering_duration = Lens.lens (\ReservedNodeOffering' {duration} -> duration) (\s@ReservedNodeOffering' {} a -> s {duration = a} :: ReservedNodeOffering)

-- | The currency code for the compute nodes offering.
reservedNodeOffering_currencyCode :: Lens.Lens' ReservedNodeOffering (Core.Maybe Core.Text)
reservedNodeOffering_currencyCode = Lens.lens (\ReservedNodeOffering' {currencyCode} -> currencyCode) (\s@ReservedNodeOffering' {} a -> s {currencyCode = a} :: ReservedNodeOffering)

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
reservedNodeOffering_fixedPrice :: Lens.Lens' ReservedNodeOffering (Core.Maybe Core.Double)
reservedNodeOffering_fixedPrice = Lens.lens (\ReservedNodeOffering' {fixedPrice} -> fixedPrice) (\s@ReservedNodeOffering' {} a -> s {fixedPrice = a} :: ReservedNodeOffering)

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
reservedNodeOffering_usagePrice :: Lens.Lens' ReservedNodeOffering (Core.Maybe Core.Double)
reservedNodeOffering_usagePrice = Lens.lens (\ReservedNodeOffering' {usagePrice} -> usagePrice) (\s@ReservedNodeOffering' {} a -> s {usagePrice = a} :: ReservedNodeOffering)

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
reservedNodeOffering_offeringType :: Lens.Lens' ReservedNodeOffering (Core.Maybe Core.Text)
reservedNodeOffering_offeringType = Lens.lens (\ReservedNodeOffering' {offeringType} -> offeringType) (\s@ReservedNodeOffering' {} a -> s {offeringType = a} :: ReservedNodeOffering)

-- | The node type offered by the reserved node offering.
reservedNodeOffering_nodeType :: Lens.Lens' ReservedNodeOffering (Core.Maybe Core.Text)
reservedNodeOffering_nodeType = Lens.lens (\ReservedNodeOffering' {nodeType} -> nodeType) (\s@ReservedNodeOffering' {} a -> s {nodeType = a} :: ReservedNodeOffering)

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect
-- for heavy-utilization reserved nodes.
reservedNodeOffering_recurringCharges :: Lens.Lens' ReservedNodeOffering (Core.Maybe [RecurringCharge])
reservedNodeOffering_recurringCharges = Lens.lens (\ReservedNodeOffering' {recurringCharges} -> recurringCharges) (\s@ReservedNodeOffering' {} a -> s {recurringCharges = a} :: ReservedNodeOffering) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ReservedNodeOffering where
  parseXML x =
    ReservedNodeOffering'
      Core.<$> (x Core..@? "ReservedNodeOfferingType")
      Core.<*> (x Core..@? "ReservedNodeOfferingId")
      Core.<*> (x Core..@? "Duration")
      Core.<*> (x Core..@? "CurrencyCode")
      Core.<*> (x Core..@? "FixedPrice")
      Core.<*> (x Core..@? "UsagePrice")
      Core.<*> (x Core..@? "OfferingType")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> ( x Core..@? "RecurringCharges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "RecurringCharge")
               )

instance Core.Hashable ReservedNodeOffering

instance Core.NFData ReservedNodeOffering
