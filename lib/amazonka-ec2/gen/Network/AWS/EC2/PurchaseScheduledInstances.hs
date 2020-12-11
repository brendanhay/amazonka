{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.PurchaseScheduledInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases the Scheduled Instances with the specified schedule.
--
-- Scheduled Instances enable you to purchase Amazon EC2 compute capacity by the hour for a one-year term. Before you can purchase a Scheduled Instance, you must call 'DescribeScheduledInstanceAvailability' to check for available schedules and obtain a purchase token. After you purchase a Scheduled Instance, you must call 'RunScheduledInstances' during each scheduled time period.
-- After you purchase a Scheduled Instance, you can't cancel, modify, or resell your purchase.
module Network.AWS.EC2.PurchaseScheduledInstances
  ( -- * Creating a request
    PurchaseScheduledInstances (..),
    mkPurchaseScheduledInstances,

    -- ** Request lenses
    psiClientToken,
    psiDryRun,
    psiPurchaseRequests,

    -- * Destructuring the response
    PurchaseScheduledInstancesResponse (..),
    mkPurchaseScheduledInstancesResponse,

    -- ** Response lenses
    psirsScheduledInstanceSet,
    psirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for PurchaseScheduledInstances.
--
-- /See:/ 'mkPurchaseScheduledInstances' smart constructor.
data PurchaseScheduledInstances = PurchaseScheduledInstances'
  { clientToken ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    purchaseRequests ::
      Lude.NonEmpty PurchaseRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseScheduledInstances' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'purchaseRequests' - The purchase requests.
mkPurchaseScheduledInstances ::
  -- | 'purchaseRequests'
  Lude.NonEmpty PurchaseRequest ->
  PurchaseScheduledInstances
mkPurchaseScheduledInstances pPurchaseRequests_ =
  PurchaseScheduledInstances'
    { clientToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      purchaseRequests = pPurchaseRequests_
    }

-- | Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiClientToken :: Lens.Lens' PurchaseScheduledInstances (Lude.Maybe Lude.Text)
psiClientToken = Lens.lens (clientToken :: PurchaseScheduledInstances -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: PurchaseScheduledInstances)
{-# DEPRECATED psiClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiDryRun :: Lens.Lens' PurchaseScheduledInstances (Lude.Maybe Lude.Bool)
psiDryRun = Lens.lens (dryRun :: PurchaseScheduledInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: PurchaseScheduledInstances)
{-# DEPRECATED psiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The purchase requests.
--
-- /Note:/ Consider using 'purchaseRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiPurchaseRequests :: Lens.Lens' PurchaseScheduledInstances (Lude.NonEmpty PurchaseRequest)
psiPurchaseRequests = Lens.lens (purchaseRequests :: PurchaseScheduledInstances -> Lude.NonEmpty PurchaseRequest) (\s a -> s {purchaseRequests = a} :: PurchaseScheduledInstances)
{-# DEPRECATED psiPurchaseRequests "Use generic-lens or generic-optics with 'purchaseRequests' instead." #-}

instance Lude.AWSRequest PurchaseScheduledInstances where
  type
    Rs PurchaseScheduledInstances =
      PurchaseScheduledInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          PurchaseScheduledInstancesResponse'
            Lude.<$> ( x Lude..@? "scheduledInstanceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseScheduledInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PurchaseScheduledInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery PurchaseScheduledInstances where
  toQuery PurchaseScheduledInstances' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PurchaseScheduledInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "PurchaseRequest" purchaseRequests
      ]

-- | Contains the output of PurchaseScheduledInstances.
--
-- /See:/ 'mkPurchaseScheduledInstancesResponse' smart constructor.
data PurchaseScheduledInstancesResponse = PurchaseScheduledInstancesResponse'
  { scheduledInstanceSet ::
      Lude.Maybe
        [ScheduledInstance],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseScheduledInstancesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'scheduledInstanceSet' - Information about the Scheduled Instances.
mkPurchaseScheduledInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseScheduledInstancesResponse
mkPurchaseScheduledInstancesResponse pResponseStatus_ =
  PurchaseScheduledInstancesResponse'
    { scheduledInstanceSet =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psirsScheduledInstanceSet :: Lens.Lens' PurchaseScheduledInstancesResponse (Lude.Maybe [ScheduledInstance])
psirsScheduledInstanceSet = Lens.lens (scheduledInstanceSet :: PurchaseScheduledInstancesResponse -> Lude.Maybe [ScheduledInstance]) (\s a -> s {scheduledInstanceSet = a} :: PurchaseScheduledInstancesResponse)
{-# DEPRECATED psirsScheduledInstanceSet "Use generic-lens or generic-optics with 'scheduledInstanceSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psirsResponseStatus :: Lens.Lens' PurchaseScheduledInstancesResponse Lude.Int
psirsResponseStatus = Lens.lens (responseStatus :: PurchaseScheduledInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseScheduledInstancesResponse)
{-# DEPRECATED psirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
