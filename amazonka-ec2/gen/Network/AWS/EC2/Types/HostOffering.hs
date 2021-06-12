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
-- Module      : Network.AWS.EC2.Types.HostOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostOffering where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import qualified Network.AWS.Lens as Lens

-- | Details about the Dedicated Host Reservation offering.
--
-- /See:/ 'newHostOffering' smart constructor.
data HostOffering = HostOffering'
  { -- | The instance family of the offering.
    instanceFamily :: Core.Maybe Core.Text,
    -- | The upfront price of the offering. Does not apply to No Upfront
    -- offerings.
    upfrontPrice :: Core.Maybe Core.Text,
    -- | The available payment option.
    paymentOption :: Core.Maybe PaymentOption,
    -- | The duration of the offering (in seconds).
    duration :: Core.Maybe Core.Int,
    -- | The ID of the offering.
    offeringId :: Core.Maybe Core.Text,
    -- | The currency of the offering.
    currencyCode :: Core.Maybe CurrencyCodeValues,
    -- | The hourly price of the offering.
    hourlyPrice :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HostOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'hostOffering_instanceFamily' - The instance family of the offering.
--
-- 'upfrontPrice', 'hostOffering_upfrontPrice' - The upfront price of the offering. Does not apply to No Upfront
-- offerings.
--
-- 'paymentOption', 'hostOffering_paymentOption' - The available payment option.
--
-- 'duration', 'hostOffering_duration' - The duration of the offering (in seconds).
--
-- 'offeringId', 'hostOffering_offeringId' - The ID of the offering.
--
-- 'currencyCode', 'hostOffering_currencyCode' - The currency of the offering.
--
-- 'hourlyPrice', 'hostOffering_hourlyPrice' - The hourly price of the offering.
newHostOffering ::
  HostOffering
newHostOffering =
  HostOffering'
    { instanceFamily = Core.Nothing,
      upfrontPrice = Core.Nothing,
      paymentOption = Core.Nothing,
      duration = Core.Nothing,
      offeringId = Core.Nothing,
      currencyCode = Core.Nothing,
      hourlyPrice = Core.Nothing
    }

-- | The instance family of the offering.
hostOffering_instanceFamily :: Lens.Lens' HostOffering (Core.Maybe Core.Text)
hostOffering_instanceFamily = Lens.lens (\HostOffering' {instanceFamily} -> instanceFamily) (\s@HostOffering' {} a -> s {instanceFamily = a} :: HostOffering)

-- | The upfront price of the offering. Does not apply to No Upfront
-- offerings.
hostOffering_upfrontPrice :: Lens.Lens' HostOffering (Core.Maybe Core.Text)
hostOffering_upfrontPrice = Lens.lens (\HostOffering' {upfrontPrice} -> upfrontPrice) (\s@HostOffering' {} a -> s {upfrontPrice = a} :: HostOffering)

-- | The available payment option.
hostOffering_paymentOption :: Lens.Lens' HostOffering (Core.Maybe PaymentOption)
hostOffering_paymentOption = Lens.lens (\HostOffering' {paymentOption} -> paymentOption) (\s@HostOffering' {} a -> s {paymentOption = a} :: HostOffering)

-- | The duration of the offering (in seconds).
hostOffering_duration :: Lens.Lens' HostOffering (Core.Maybe Core.Int)
hostOffering_duration = Lens.lens (\HostOffering' {duration} -> duration) (\s@HostOffering' {} a -> s {duration = a} :: HostOffering)

-- | The ID of the offering.
hostOffering_offeringId :: Lens.Lens' HostOffering (Core.Maybe Core.Text)
hostOffering_offeringId = Lens.lens (\HostOffering' {offeringId} -> offeringId) (\s@HostOffering' {} a -> s {offeringId = a} :: HostOffering)

-- | The currency of the offering.
hostOffering_currencyCode :: Lens.Lens' HostOffering (Core.Maybe CurrencyCodeValues)
hostOffering_currencyCode = Lens.lens (\HostOffering' {currencyCode} -> currencyCode) (\s@HostOffering' {} a -> s {currencyCode = a} :: HostOffering)

-- | The hourly price of the offering.
hostOffering_hourlyPrice :: Lens.Lens' HostOffering (Core.Maybe Core.Text)
hostOffering_hourlyPrice = Lens.lens (\HostOffering' {hourlyPrice} -> hourlyPrice) (\s@HostOffering' {} a -> s {hourlyPrice = a} :: HostOffering)

instance Core.FromXML HostOffering where
  parseXML x =
    HostOffering'
      Core.<$> (x Core..@? "instanceFamily")
      Core.<*> (x Core..@? "upfrontPrice")
      Core.<*> (x Core..@? "paymentOption")
      Core.<*> (x Core..@? "duration")
      Core.<*> (x Core..@? "offeringId")
      Core.<*> (x Core..@? "currencyCode")
      Core.<*> (x Core..@? "hourlyPrice")

instance Core.Hashable HostOffering

instance Core.NFData HostOffering
