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
import qualified Network.AWS.Prelude as Prelude

-- | Details about the Dedicated Host Reservation offering.
--
-- /See:/ 'newHostOffering' smart constructor.
data HostOffering = HostOffering'
  { -- | The instance family of the offering.
    instanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The currency of the offering.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The hourly price of the offering.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The upfront price of the offering. Does not apply to No Upfront
    -- offerings.
    upfrontPrice :: Prelude.Maybe Prelude.Text,
    -- | The ID of the offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The duration of the offering (in seconds).
    duration :: Prelude.Maybe Prelude.Int,
    -- | The available payment option.
    paymentOption :: Prelude.Maybe PaymentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'currencyCode', 'hostOffering_currencyCode' - The currency of the offering.
--
-- 'hourlyPrice', 'hostOffering_hourlyPrice' - The hourly price of the offering.
--
-- 'upfrontPrice', 'hostOffering_upfrontPrice' - The upfront price of the offering. Does not apply to No Upfront
-- offerings.
--
-- 'offeringId', 'hostOffering_offeringId' - The ID of the offering.
--
-- 'duration', 'hostOffering_duration' - The duration of the offering (in seconds).
--
-- 'paymentOption', 'hostOffering_paymentOption' - The available payment option.
newHostOffering ::
  HostOffering
newHostOffering =
  HostOffering'
    { instanceFamily = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      upfrontPrice = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      duration = Prelude.Nothing,
      paymentOption = Prelude.Nothing
    }

-- | The instance family of the offering.
hostOffering_instanceFamily :: Lens.Lens' HostOffering (Prelude.Maybe Prelude.Text)
hostOffering_instanceFamily = Lens.lens (\HostOffering' {instanceFamily} -> instanceFamily) (\s@HostOffering' {} a -> s {instanceFamily = a} :: HostOffering)

-- | The currency of the offering.
hostOffering_currencyCode :: Lens.Lens' HostOffering (Prelude.Maybe CurrencyCodeValues)
hostOffering_currencyCode = Lens.lens (\HostOffering' {currencyCode} -> currencyCode) (\s@HostOffering' {} a -> s {currencyCode = a} :: HostOffering)

-- | The hourly price of the offering.
hostOffering_hourlyPrice :: Lens.Lens' HostOffering (Prelude.Maybe Prelude.Text)
hostOffering_hourlyPrice = Lens.lens (\HostOffering' {hourlyPrice} -> hourlyPrice) (\s@HostOffering' {} a -> s {hourlyPrice = a} :: HostOffering)

-- | The upfront price of the offering. Does not apply to No Upfront
-- offerings.
hostOffering_upfrontPrice :: Lens.Lens' HostOffering (Prelude.Maybe Prelude.Text)
hostOffering_upfrontPrice = Lens.lens (\HostOffering' {upfrontPrice} -> upfrontPrice) (\s@HostOffering' {} a -> s {upfrontPrice = a} :: HostOffering)

-- | The ID of the offering.
hostOffering_offeringId :: Lens.Lens' HostOffering (Prelude.Maybe Prelude.Text)
hostOffering_offeringId = Lens.lens (\HostOffering' {offeringId} -> offeringId) (\s@HostOffering' {} a -> s {offeringId = a} :: HostOffering)

-- | The duration of the offering (in seconds).
hostOffering_duration :: Lens.Lens' HostOffering (Prelude.Maybe Prelude.Int)
hostOffering_duration = Lens.lens (\HostOffering' {duration} -> duration) (\s@HostOffering' {} a -> s {duration = a} :: HostOffering)

-- | The available payment option.
hostOffering_paymentOption :: Lens.Lens' HostOffering (Prelude.Maybe PaymentOption)
hostOffering_paymentOption = Lens.lens (\HostOffering' {paymentOption} -> paymentOption) (\s@HostOffering' {} a -> s {paymentOption = a} :: HostOffering)

instance Core.FromXML HostOffering where
  parseXML x =
    HostOffering'
      Prelude.<$> (x Core..@? "instanceFamily")
      Prelude.<*> (x Core..@? "currencyCode")
      Prelude.<*> (x Core..@? "hourlyPrice")
      Prelude.<*> (x Core..@? "upfrontPrice")
      Prelude.<*> (x Core..@? "offeringId")
      Prelude.<*> (x Core..@? "duration")
      Prelude.<*> (x Core..@? "paymentOption")

instance Prelude.Hashable HostOffering

instance Prelude.NFData HostOffering
