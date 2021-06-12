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
-- Module      : Network.AWS.EC2.PurchaseScheduledInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases the Scheduled Instances with the specified schedule.
--
-- Scheduled Instances enable you to purchase Amazon EC2 compute capacity
-- by the hour for a one-year term. Before you can purchase a Scheduled
-- Instance, you must call DescribeScheduledInstanceAvailability to check
-- for available schedules and obtain a purchase token. After you purchase
-- a Scheduled Instance, you must call RunScheduledInstances during each
-- scheduled time period.
--
-- After you purchase a Scheduled Instance, you can\'t cancel, modify, or
-- resell your purchase.
module Network.AWS.EC2.PurchaseScheduledInstances
  ( -- * Creating a Request
    PurchaseScheduledInstances (..),
    newPurchaseScheduledInstances,

    -- * Request Lenses
    purchaseScheduledInstances_dryRun,
    purchaseScheduledInstances_clientToken,
    purchaseScheduledInstances_purchaseRequests,

    -- * Destructuring the Response
    PurchaseScheduledInstancesResponse (..),
    newPurchaseScheduledInstancesResponse,

    -- * Response Lenses
    purchaseScheduledInstancesResponse_scheduledInstanceSet,
    purchaseScheduledInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for PurchaseScheduledInstances.
--
-- /See:/ 'newPurchaseScheduledInstances' smart constructor.
data PurchaseScheduledInstances = PurchaseScheduledInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Unique, case-sensitive identifier that ensures the idempotency of the
    -- request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The purchase requests.
    purchaseRequests :: Core.NonEmpty PurchaseRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseScheduledInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'purchaseScheduledInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'purchaseScheduledInstances_clientToken' - Unique, case-sensitive identifier that ensures the idempotency of the
-- request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'purchaseRequests', 'purchaseScheduledInstances_purchaseRequests' - The purchase requests.
newPurchaseScheduledInstances ::
  -- | 'purchaseRequests'
  Core.NonEmpty PurchaseRequest ->
  PurchaseScheduledInstances
newPurchaseScheduledInstances pPurchaseRequests_ =
  PurchaseScheduledInstances'
    { dryRun = Core.Nothing,
      clientToken = Core.Nothing,
      purchaseRequests =
        Lens._Coerce Lens.# pPurchaseRequests_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
purchaseScheduledInstances_dryRun :: Lens.Lens' PurchaseScheduledInstances (Core.Maybe Core.Bool)
purchaseScheduledInstances_dryRun = Lens.lens (\PurchaseScheduledInstances' {dryRun} -> dryRun) (\s@PurchaseScheduledInstances' {} a -> s {dryRun = a} :: PurchaseScheduledInstances)

-- | Unique, case-sensitive identifier that ensures the idempotency of the
-- request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
purchaseScheduledInstances_clientToken :: Lens.Lens' PurchaseScheduledInstances (Core.Maybe Core.Text)
purchaseScheduledInstances_clientToken = Lens.lens (\PurchaseScheduledInstances' {clientToken} -> clientToken) (\s@PurchaseScheduledInstances' {} a -> s {clientToken = a} :: PurchaseScheduledInstances)

-- | The purchase requests.
purchaseScheduledInstances_purchaseRequests :: Lens.Lens' PurchaseScheduledInstances (Core.NonEmpty PurchaseRequest)
purchaseScheduledInstances_purchaseRequests = Lens.lens (\PurchaseScheduledInstances' {purchaseRequests} -> purchaseRequests) (\s@PurchaseScheduledInstances' {} a -> s {purchaseRequests = a} :: PurchaseScheduledInstances) Core.. Lens._Coerce

instance Core.AWSRequest PurchaseScheduledInstances where
  type
    AWSResponse PurchaseScheduledInstances =
      PurchaseScheduledInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          PurchaseScheduledInstancesResponse'
            Core.<$> ( x Core..@? "scheduledInstanceSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PurchaseScheduledInstances

instance Core.NFData PurchaseScheduledInstances

instance Core.ToHeaders PurchaseScheduledInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath PurchaseScheduledInstances where
  toPath = Core.const "/"

instance Core.ToQuery PurchaseScheduledInstances where
  toQuery PurchaseScheduledInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("PurchaseScheduledInstances" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ClientToken" Core.=: clientToken,
        Core.toQueryList "PurchaseRequest" purchaseRequests
      ]

-- | Contains the output of PurchaseScheduledInstances.
--
-- /See:/ 'newPurchaseScheduledInstancesResponse' smart constructor.
data PurchaseScheduledInstancesResponse = PurchaseScheduledInstancesResponse'
  { -- | Information about the Scheduled Instances.
    scheduledInstanceSet :: Core.Maybe [ScheduledInstance],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseScheduledInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledInstanceSet', 'purchaseScheduledInstancesResponse_scheduledInstanceSet' - Information about the Scheduled Instances.
--
-- 'httpStatus', 'purchaseScheduledInstancesResponse_httpStatus' - The response's http status code.
newPurchaseScheduledInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PurchaseScheduledInstancesResponse
newPurchaseScheduledInstancesResponse pHttpStatus_ =
  PurchaseScheduledInstancesResponse'
    { scheduledInstanceSet =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Scheduled Instances.
purchaseScheduledInstancesResponse_scheduledInstanceSet :: Lens.Lens' PurchaseScheduledInstancesResponse (Core.Maybe [ScheduledInstance])
purchaseScheduledInstancesResponse_scheduledInstanceSet = Lens.lens (\PurchaseScheduledInstancesResponse' {scheduledInstanceSet} -> scheduledInstanceSet) (\s@PurchaseScheduledInstancesResponse' {} a -> s {scheduledInstanceSet = a} :: PurchaseScheduledInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
purchaseScheduledInstancesResponse_httpStatus :: Lens.Lens' PurchaseScheduledInstancesResponse Core.Int
purchaseScheduledInstancesResponse_httpStatus = Lens.lens (\PurchaseScheduledInstancesResponse' {httpStatus} -> httpStatus) (\s@PurchaseScheduledInstancesResponse' {} a -> s {httpStatus = a} :: PurchaseScheduledInstancesResponse)

instance
  Core.NFData
    PurchaseScheduledInstancesResponse
