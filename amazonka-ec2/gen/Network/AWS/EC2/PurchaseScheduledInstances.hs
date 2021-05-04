{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Unique, case-sensitive identifier that ensures the idempotency of the
    -- request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The purchase requests.
    purchaseRequests :: Prelude.NonEmpty PurchaseRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.NonEmpty PurchaseRequest ->
  PurchaseScheduledInstances
newPurchaseScheduledInstances pPurchaseRequests_ =
  PurchaseScheduledInstances'
    { dryRun =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      purchaseRequests =
        Prelude._Coerce Lens.# pPurchaseRequests_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
purchaseScheduledInstances_dryRun :: Lens.Lens' PurchaseScheduledInstances (Prelude.Maybe Prelude.Bool)
purchaseScheduledInstances_dryRun = Lens.lens (\PurchaseScheduledInstances' {dryRun} -> dryRun) (\s@PurchaseScheduledInstances' {} a -> s {dryRun = a} :: PurchaseScheduledInstances)

-- | Unique, case-sensitive identifier that ensures the idempotency of the
-- request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
purchaseScheduledInstances_clientToken :: Lens.Lens' PurchaseScheduledInstances (Prelude.Maybe Prelude.Text)
purchaseScheduledInstances_clientToken = Lens.lens (\PurchaseScheduledInstances' {clientToken} -> clientToken) (\s@PurchaseScheduledInstances' {} a -> s {clientToken = a} :: PurchaseScheduledInstances)

-- | The purchase requests.
purchaseScheduledInstances_purchaseRequests :: Lens.Lens' PurchaseScheduledInstances (Prelude.NonEmpty PurchaseRequest)
purchaseScheduledInstances_purchaseRequests = Lens.lens (\PurchaseScheduledInstances' {purchaseRequests} -> purchaseRequests) (\s@PurchaseScheduledInstances' {} a -> s {purchaseRequests = a} :: PurchaseScheduledInstances) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    PurchaseScheduledInstances
  where
  type
    Rs PurchaseScheduledInstances =
      PurchaseScheduledInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          PurchaseScheduledInstancesResponse'
            Prelude.<$> ( x Prelude..@? "scheduledInstanceSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PurchaseScheduledInstances

instance Prelude.NFData PurchaseScheduledInstances

instance Prelude.ToHeaders PurchaseScheduledInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PurchaseScheduledInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PurchaseScheduledInstances where
  toQuery PurchaseScheduledInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PurchaseScheduledInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "ClientToken" Prelude.=: clientToken,
        Prelude.toQueryList
          "PurchaseRequest"
          purchaseRequests
      ]

-- | Contains the output of PurchaseScheduledInstances.
--
-- /See:/ 'newPurchaseScheduledInstancesResponse' smart constructor.
data PurchaseScheduledInstancesResponse = PurchaseScheduledInstancesResponse'
  { -- | Information about the Scheduled Instances.
    scheduledInstanceSet :: Prelude.Maybe [ScheduledInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PurchaseScheduledInstancesResponse
newPurchaseScheduledInstancesResponse pHttpStatus_ =
  PurchaseScheduledInstancesResponse'
    { scheduledInstanceSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Scheduled Instances.
purchaseScheduledInstancesResponse_scheduledInstanceSet :: Lens.Lens' PurchaseScheduledInstancesResponse (Prelude.Maybe [ScheduledInstance])
purchaseScheduledInstancesResponse_scheduledInstanceSet = Lens.lens (\PurchaseScheduledInstancesResponse' {scheduledInstanceSet} -> scheduledInstanceSet) (\s@PurchaseScheduledInstancesResponse' {} a -> s {scheduledInstanceSet = a} :: PurchaseScheduledInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
purchaseScheduledInstancesResponse_httpStatus :: Lens.Lens' PurchaseScheduledInstancesResponse Prelude.Int
purchaseScheduledInstancesResponse_httpStatus = Lens.lens (\PurchaseScheduledInstancesResponse' {httpStatus} -> httpStatus) (\s@PurchaseScheduledInstancesResponse' {} a -> s {httpStatus = a} :: PurchaseScheduledInstancesResponse)

instance
  Prelude.NFData
    PurchaseScheduledInstancesResponse
