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
-- Module      : Network.AWS.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.EC2.PurchaseReservedInstancesOffering
  ( -- * Creating a Request
    PurchaseReservedInstancesOffering (..),
    newPurchaseReservedInstancesOffering,

    -- * Request Lenses
    purchaseReservedInstancesOffering_dryRun,
    purchaseReservedInstancesOffering_purchaseTime,
    purchaseReservedInstancesOffering_limitPrice,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for PurchaseReservedInstancesOffering.
--
-- /See:/ 'newPurchaseReservedInstancesOffering' smart constructor.
data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The time at which to purchase the Reserved Instance, in UTC format (for
    -- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    purchaseTime :: Core.Maybe Core.ISO8601,
    -- | Specified for Reserved Instance Marketplace offerings to limit the total
    -- order and ensure that the Reserved Instances are not purchased at
    -- unexpected prices.
    limitPrice :: Core.Maybe ReservedInstanceLimitPrice,
    -- | The number of Reserved Instances to purchase.
    instanceCount :: Core.Int,
    -- | The ID of the Reserved Instance offering to purchase.
    reservedInstancesOfferingId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'purchaseTime', 'purchaseReservedInstancesOffering_purchaseTime' - The time at which to purchase the Reserved Instance, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'limitPrice', 'purchaseReservedInstancesOffering_limitPrice' - Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
--
-- 'instanceCount', 'purchaseReservedInstancesOffering_instanceCount' - The number of Reserved Instances to purchase.
--
-- 'reservedInstancesOfferingId', 'purchaseReservedInstancesOffering_reservedInstancesOfferingId' - The ID of the Reserved Instance offering to purchase.
newPurchaseReservedInstancesOffering ::
  -- | 'instanceCount'
  Core.Int ->
  -- | 'reservedInstancesOfferingId'
  Core.Text ->
  PurchaseReservedInstancesOffering
newPurchaseReservedInstancesOffering
  pInstanceCount_
  pReservedInstancesOfferingId_ =
    PurchaseReservedInstancesOffering'
      { dryRun =
          Core.Nothing,
        purchaseTime = Core.Nothing,
        limitPrice = Core.Nothing,
        instanceCount = pInstanceCount_,
        reservedInstancesOfferingId =
          pReservedInstancesOfferingId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
purchaseReservedInstancesOffering_dryRun :: Lens.Lens' PurchaseReservedInstancesOffering (Core.Maybe Core.Bool)
purchaseReservedInstancesOffering_dryRun = Lens.lens (\PurchaseReservedInstancesOffering' {dryRun} -> dryRun) (\s@PurchaseReservedInstancesOffering' {} a -> s {dryRun = a} :: PurchaseReservedInstancesOffering)

-- | The time at which to purchase the Reserved Instance, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
purchaseReservedInstancesOffering_purchaseTime :: Lens.Lens' PurchaseReservedInstancesOffering (Core.Maybe Core.UTCTime)
purchaseReservedInstancesOffering_purchaseTime = Lens.lens (\PurchaseReservedInstancesOffering' {purchaseTime} -> purchaseTime) (\s@PurchaseReservedInstancesOffering' {} a -> s {purchaseTime = a} :: PurchaseReservedInstancesOffering) Core.. Lens.mapping Core._Time

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
purchaseReservedInstancesOffering_limitPrice :: Lens.Lens' PurchaseReservedInstancesOffering (Core.Maybe ReservedInstanceLimitPrice)
purchaseReservedInstancesOffering_limitPrice = Lens.lens (\PurchaseReservedInstancesOffering' {limitPrice} -> limitPrice) (\s@PurchaseReservedInstancesOffering' {} a -> s {limitPrice = a} :: PurchaseReservedInstancesOffering)

-- | The number of Reserved Instances to purchase.
purchaseReservedInstancesOffering_instanceCount :: Lens.Lens' PurchaseReservedInstancesOffering Core.Int
purchaseReservedInstancesOffering_instanceCount = Lens.lens (\PurchaseReservedInstancesOffering' {instanceCount} -> instanceCount) (\s@PurchaseReservedInstancesOffering' {} a -> s {instanceCount = a} :: PurchaseReservedInstancesOffering)

-- | The ID of the Reserved Instance offering to purchase.
purchaseReservedInstancesOffering_reservedInstancesOfferingId :: Lens.Lens' PurchaseReservedInstancesOffering Core.Text
purchaseReservedInstancesOffering_reservedInstancesOfferingId = Lens.lens (\PurchaseReservedInstancesOffering' {reservedInstancesOfferingId} -> reservedInstancesOfferingId) (\s@PurchaseReservedInstancesOffering' {} a -> s {reservedInstancesOfferingId = a} :: PurchaseReservedInstancesOffering)

instance
  Core.AWSRequest
    PurchaseReservedInstancesOffering
  where
  type
    AWSResponse PurchaseReservedInstancesOffering =
      PurchaseReservedInstancesOfferingResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          PurchaseReservedInstancesOfferingResponse'
            Core.<$> (x Core..@? "reservedInstancesId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    PurchaseReservedInstancesOffering

instance
  Core.NFData
    PurchaseReservedInstancesOffering

instance
  Core.ToHeaders
    PurchaseReservedInstancesOffering
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    PurchaseReservedInstancesOffering
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    PurchaseReservedInstancesOffering
  where
  toQuery PurchaseReservedInstancesOffering' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "PurchaseReservedInstancesOffering" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "PurchaseTime" Core.=: purchaseTime,
        "LimitPrice" Core.=: limitPrice,
        "InstanceCount" Core.=: instanceCount,
        "ReservedInstancesOfferingId"
          Core.=: reservedInstancesOfferingId
      ]

-- | Contains the output of PurchaseReservedInstancesOffering.
--
-- /See:/ 'newPurchaseReservedInstancesOfferingResponse' smart constructor.
data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse'
  { -- | The IDs of the purchased Reserved Instances.
    reservedInstancesId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseReservedInstancesOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'purchaseReservedInstancesOfferingResponse_reservedInstancesId' - The IDs of the purchased Reserved Instances.
--
-- 'httpStatus', 'purchaseReservedInstancesOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedInstancesOfferingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PurchaseReservedInstancesOfferingResponse
newPurchaseReservedInstancesOfferingResponse
  pHttpStatus_ =
    PurchaseReservedInstancesOfferingResponse'
      { reservedInstancesId =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The IDs of the purchased Reserved Instances.
purchaseReservedInstancesOfferingResponse_reservedInstancesId :: Lens.Lens' PurchaseReservedInstancesOfferingResponse (Core.Maybe Core.Text)
purchaseReservedInstancesOfferingResponse_reservedInstancesId = Lens.lens (\PurchaseReservedInstancesOfferingResponse' {reservedInstancesId} -> reservedInstancesId) (\s@PurchaseReservedInstancesOfferingResponse' {} a -> s {reservedInstancesId = a} :: PurchaseReservedInstancesOfferingResponse)

-- | The response's http status code.
purchaseReservedInstancesOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedInstancesOfferingResponse Core.Int
purchaseReservedInstancesOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedInstancesOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedInstancesOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedInstancesOfferingResponse)

instance
  Core.NFData
    PurchaseReservedInstancesOfferingResponse
