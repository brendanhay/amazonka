{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a Reserved Instance for use with your account. With Reserved
-- Instances, you pay a lower hourly rate compared to On-Demand instance
-- pricing.
--
-- Use DescribeReservedInstancesOfferings to get a list of Reserved
-- Instance offerings that match your specifications. After you\'ve
-- purchased a Reserved Instance, you can check for your new Reserved
-- Instance with DescribeReservedInstances.
--
-- To queue a purchase for a future date and time, specify a purchase time.
-- If you do not specify a purchase time, the default is the current time.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon EC2 User Guide/.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.PurchaseReservedInstancesOffering
  ( -- * Creating a Request
    PurchaseReservedInstancesOffering (..),
    newPurchaseReservedInstancesOffering,

    -- * Request Lenses
    purchaseReservedInstancesOffering_dryRun,
    purchaseReservedInstancesOffering_limitPrice,
    purchaseReservedInstancesOffering_purchaseTime,
    purchaseReservedInstancesOffering_instanceCount,
    purchaseReservedInstancesOffering_reservedInstancesOfferingId,

    -- * Destructuring the Response
    PurchaseReservedInstancesOfferingResponse (..),
    newPurchaseReservedInstancesOfferingResponse,

    -- * Response Lenses
    purchaseReservedInstancesOfferingResponse_reservedInstancesId,
    purchaseReservedInstancesOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for PurchaseReservedInstancesOffering.
--
-- /See:/ 'newPurchaseReservedInstancesOffering' smart constructor.
data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specified for Reserved Instance Marketplace offerings to limit the total
    -- order and ensure that the Reserved Instances are not purchased at
    -- unexpected prices.
    limitPrice :: Prelude.Maybe ReservedInstanceLimitPrice,
    -- | The time at which to purchase the Reserved Instance, in UTC format (for
    -- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    purchaseTime :: Prelude.Maybe Data.ISO8601,
    -- | The number of Reserved Instances to purchase.
    instanceCount :: Prelude.Int,
    -- | The ID of the Reserved Instance offering to purchase.
    reservedInstancesOfferingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedInstancesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'purchaseReservedInstancesOffering_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'limitPrice', 'purchaseReservedInstancesOffering_limitPrice' - Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
--
-- 'purchaseTime', 'purchaseReservedInstancesOffering_purchaseTime' - The time at which to purchase the Reserved Instance, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'instanceCount', 'purchaseReservedInstancesOffering_instanceCount' - The number of Reserved Instances to purchase.
--
-- 'reservedInstancesOfferingId', 'purchaseReservedInstancesOffering_reservedInstancesOfferingId' - The ID of the Reserved Instance offering to purchase.
newPurchaseReservedInstancesOffering ::
  -- | 'instanceCount'
  Prelude.Int ->
  -- | 'reservedInstancesOfferingId'
  Prelude.Text ->
  PurchaseReservedInstancesOffering
newPurchaseReservedInstancesOffering
  pInstanceCount_
  pReservedInstancesOfferingId_ =
    PurchaseReservedInstancesOffering'
      { dryRun =
          Prelude.Nothing,
        limitPrice = Prelude.Nothing,
        purchaseTime = Prelude.Nothing,
        instanceCount = pInstanceCount_,
        reservedInstancesOfferingId =
          pReservedInstancesOfferingId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
purchaseReservedInstancesOffering_dryRun :: Lens.Lens' PurchaseReservedInstancesOffering (Prelude.Maybe Prelude.Bool)
purchaseReservedInstancesOffering_dryRun = Lens.lens (\PurchaseReservedInstancesOffering' {dryRun} -> dryRun) (\s@PurchaseReservedInstancesOffering' {} a -> s {dryRun = a} :: PurchaseReservedInstancesOffering)

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
purchaseReservedInstancesOffering_limitPrice :: Lens.Lens' PurchaseReservedInstancesOffering (Prelude.Maybe ReservedInstanceLimitPrice)
purchaseReservedInstancesOffering_limitPrice = Lens.lens (\PurchaseReservedInstancesOffering' {limitPrice} -> limitPrice) (\s@PurchaseReservedInstancesOffering' {} a -> s {limitPrice = a} :: PurchaseReservedInstancesOffering)

-- | The time at which to purchase the Reserved Instance, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
purchaseReservedInstancesOffering_purchaseTime :: Lens.Lens' PurchaseReservedInstancesOffering (Prelude.Maybe Prelude.UTCTime)
purchaseReservedInstancesOffering_purchaseTime = Lens.lens (\PurchaseReservedInstancesOffering' {purchaseTime} -> purchaseTime) (\s@PurchaseReservedInstancesOffering' {} a -> s {purchaseTime = a} :: PurchaseReservedInstancesOffering) Prelude.. Lens.mapping Data._Time

-- | The number of Reserved Instances to purchase.
purchaseReservedInstancesOffering_instanceCount :: Lens.Lens' PurchaseReservedInstancesOffering Prelude.Int
purchaseReservedInstancesOffering_instanceCount = Lens.lens (\PurchaseReservedInstancesOffering' {instanceCount} -> instanceCount) (\s@PurchaseReservedInstancesOffering' {} a -> s {instanceCount = a} :: PurchaseReservedInstancesOffering)

-- | The ID of the Reserved Instance offering to purchase.
purchaseReservedInstancesOffering_reservedInstancesOfferingId :: Lens.Lens' PurchaseReservedInstancesOffering Prelude.Text
purchaseReservedInstancesOffering_reservedInstancesOfferingId = Lens.lens (\PurchaseReservedInstancesOffering' {reservedInstancesOfferingId} -> reservedInstancesOfferingId) (\s@PurchaseReservedInstancesOffering' {} a -> s {reservedInstancesOfferingId = a} :: PurchaseReservedInstancesOffering)

instance
  Core.AWSRequest
    PurchaseReservedInstancesOffering
  where
  type
    AWSResponse PurchaseReservedInstancesOffering =
      PurchaseReservedInstancesOfferingResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          PurchaseReservedInstancesOfferingResponse'
            Prelude.<$> (x Data..@? "reservedInstancesId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PurchaseReservedInstancesOffering
  where
  hashWithSalt
    _salt
    PurchaseReservedInstancesOffering' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` limitPrice
        `Prelude.hashWithSalt` purchaseTime
        `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` reservedInstancesOfferingId

instance
  Prelude.NFData
    PurchaseReservedInstancesOffering
  where
  rnf PurchaseReservedInstancesOffering' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf limitPrice `Prelude.seq`
        Prelude.rnf purchaseTime `Prelude.seq`
          Prelude.rnf instanceCount `Prelude.seq`
            Prelude.rnf reservedInstancesOfferingId

instance
  Data.ToHeaders
    PurchaseReservedInstancesOffering
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    PurchaseReservedInstancesOffering
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    PurchaseReservedInstancesOffering
  where
  toQuery PurchaseReservedInstancesOffering' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "PurchaseReservedInstancesOffering" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "LimitPrice" Data.=: limitPrice,
        "PurchaseTime" Data.=: purchaseTime,
        "InstanceCount" Data.=: instanceCount,
        "ReservedInstancesOfferingId"
          Data.=: reservedInstancesOfferingId
      ]

-- | Contains the output of PurchaseReservedInstancesOffering.
--
-- /See:/ 'newPurchaseReservedInstancesOfferingResponse' smart constructor.
data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse'
  { -- | The IDs of the purchased Reserved Instances. If your purchase crosses
    -- into a discounted pricing tier, the final Reserved Instances IDs might
    -- change. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-reserved-instances-application.html#crossing-pricing-tiers Crossing pricing tiers>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedInstancesOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'purchaseReservedInstancesOfferingResponse_reservedInstancesId' - The IDs of the purchased Reserved Instances. If your purchase crosses
-- into a discounted pricing tier, the final Reserved Instances IDs might
-- change. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-reserved-instances-application.html#crossing-pricing-tiers Crossing pricing tiers>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'httpStatus', 'purchaseReservedInstancesOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedInstancesOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseReservedInstancesOfferingResponse
newPurchaseReservedInstancesOfferingResponse
  pHttpStatus_ =
    PurchaseReservedInstancesOfferingResponse'
      { reservedInstancesId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The IDs of the purchased Reserved Instances. If your purchase crosses
-- into a discounted pricing tier, the final Reserved Instances IDs might
-- change. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-reserved-instances-application.html#crossing-pricing-tiers Crossing pricing tiers>
-- in the /Amazon Elastic Compute Cloud User Guide/.
purchaseReservedInstancesOfferingResponse_reservedInstancesId :: Lens.Lens' PurchaseReservedInstancesOfferingResponse (Prelude.Maybe Prelude.Text)
purchaseReservedInstancesOfferingResponse_reservedInstancesId = Lens.lens (\PurchaseReservedInstancesOfferingResponse' {reservedInstancesId} -> reservedInstancesId) (\s@PurchaseReservedInstancesOfferingResponse' {} a -> s {reservedInstancesId = a} :: PurchaseReservedInstancesOfferingResponse)

-- | The response's http status code.
purchaseReservedInstancesOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedInstancesOfferingResponse Prelude.Int
purchaseReservedInstancesOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedInstancesOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedInstancesOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedInstancesOfferingResponse)

instance
  Prelude.NFData
    PurchaseReservedInstancesOfferingResponse
  where
  rnf PurchaseReservedInstancesOfferingResponse' {..} =
    Prelude.rnf reservedInstancesId `Prelude.seq`
      Prelude.rnf httpStatus
