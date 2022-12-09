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
-- Module      : Amazonka.PrivateNetworks.Types.Order
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.Order where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.AcknowledgmentStatus
import Amazonka.PrivateNetworks.Types.Address
import Amazonka.PrivateNetworks.Types.TrackingInformation

-- | Information about an order.
--
-- /See:/ 'newOrder' smart constructor.
data Order = Order'
  { -- | The acknowledgement status of the order.
    acknowledgmentStatus :: Prelude.Maybe AcknowledgmentStatus,
    -- | The creation time of the order.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the network associated with this
    -- order.
    networkArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network site associated with this
    -- order.
    networkSiteArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the order.
    orderArn :: Prelude.Maybe Prelude.Text,
    -- | The shipping address of the order.
    shippingAddress :: Prelude.Maybe Address,
    -- | The tracking information of the order.
    trackingInformation :: Prelude.Maybe [TrackingInformation]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Order' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acknowledgmentStatus', 'order_acknowledgmentStatus' - The acknowledgement status of the order.
--
-- 'createdAt', 'order_createdAt' - The creation time of the order.
--
-- 'networkArn', 'order_networkArn' - The Amazon Resource Name (ARN) of the network associated with this
-- order.
--
-- 'networkSiteArn', 'order_networkSiteArn' - The Amazon Resource Name (ARN) of the network site associated with this
-- order.
--
-- 'orderArn', 'order_orderArn' - The Amazon Resource Name (ARN) of the order.
--
-- 'shippingAddress', 'order_shippingAddress' - The shipping address of the order.
--
-- 'trackingInformation', 'order_trackingInformation' - The tracking information of the order.
newOrder ::
  Order
newOrder =
  Order'
    { acknowledgmentStatus = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      networkArn = Prelude.Nothing,
      networkSiteArn = Prelude.Nothing,
      orderArn = Prelude.Nothing,
      shippingAddress = Prelude.Nothing,
      trackingInformation = Prelude.Nothing
    }

-- | The acknowledgement status of the order.
order_acknowledgmentStatus :: Lens.Lens' Order (Prelude.Maybe AcknowledgmentStatus)
order_acknowledgmentStatus = Lens.lens (\Order' {acknowledgmentStatus} -> acknowledgmentStatus) (\s@Order' {} a -> s {acknowledgmentStatus = a} :: Order)

-- | The creation time of the order.
order_createdAt :: Lens.Lens' Order (Prelude.Maybe Prelude.UTCTime)
order_createdAt = Lens.lens (\Order' {createdAt} -> createdAt) (\s@Order' {} a -> s {createdAt = a} :: Order) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the network associated with this
-- order.
order_networkArn :: Lens.Lens' Order (Prelude.Maybe Prelude.Text)
order_networkArn = Lens.lens (\Order' {networkArn} -> networkArn) (\s@Order' {} a -> s {networkArn = a} :: Order)

-- | The Amazon Resource Name (ARN) of the network site associated with this
-- order.
order_networkSiteArn :: Lens.Lens' Order (Prelude.Maybe Prelude.Text)
order_networkSiteArn = Lens.lens (\Order' {networkSiteArn} -> networkSiteArn) (\s@Order' {} a -> s {networkSiteArn = a} :: Order)

-- | The Amazon Resource Name (ARN) of the order.
order_orderArn :: Lens.Lens' Order (Prelude.Maybe Prelude.Text)
order_orderArn = Lens.lens (\Order' {orderArn} -> orderArn) (\s@Order' {} a -> s {orderArn = a} :: Order)

-- | The shipping address of the order.
order_shippingAddress :: Lens.Lens' Order (Prelude.Maybe Address)
order_shippingAddress = Lens.lens (\Order' {shippingAddress} -> shippingAddress) (\s@Order' {} a -> s {shippingAddress = a} :: Order)

-- | The tracking information of the order.
order_trackingInformation :: Lens.Lens' Order (Prelude.Maybe [TrackingInformation])
order_trackingInformation = Lens.lens (\Order' {trackingInformation} -> trackingInformation) (\s@Order' {} a -> s {trackingInformation = a} :: Order) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Order where
  parseJSON =
    Data.withObject
      "Order"
      ( \x ->
          Order'
            Prelude.<$> (x Data..:? "acknowledgmentStatus")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "networkArn")
            Prelude.<*> (x Data..:? "networkSiteArn")
            Prelude.<*> (x Data..:? "orderArn")
            Prelude.<*> (x Data..:? "shippingAddress")
            Prelude.<*> ( x Data..:? "trackingInformation"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Order where
  hashWithSalt _salt Order' {..} =
    _salt `Prelude.hashWithSalt` acknowledgmentStatus
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` networkArn
      `Prelude.hashWithSalt` networkSiteArn
      `Prelude.hashWithSalt` orderArn
      `Prelude.hashWithSalt` shippingAddress
      `Prelude.hashWithSalt` trackingInformation

instance Prelude.NFData Order where
  rnf Order' {..} =
    Prelude.rnf acknowledgmentStatus
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf networkArn
      `Prelude.seq` Prelude.rnf networkSiteArn
      `Prelude.seq` Prelude.rnf orderArn
      `Prelude.seq` Prelude.rnf shippingAddress
      `Prelude.seq` Prelude.rnf trackingInformation
