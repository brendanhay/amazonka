{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a Reserved Instance for use with your account. With Reserved Instances, you pay a lower hourly rate compared to On-Demand instance pricing.
--
-- Use 'DescribeReservedInstancesOfferings' to get a list of Reserved Instance offerings that match your specifications. After you've purchased a Reserved Instance, you can check for your new Reserved Instance with 'DescribeReservedInstances' .
-- To queue a purchase for a future date and time, specify a purchase time. If you do not specify a purchase time, the default is the current time.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances> and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.PurchaseReservedInstancesOffering
  ( -- * Creating a request
    PurchaseReservedInstancesOffering (..),
    mkPurchaseReservedInstancesOffering,

    -- ** Request lenses
    prioPurchaseTime,
    prioLimitPrice,
    prioDryRun,
    prioInstanceCount,
    prioReservedInstancesOfferingId,

    -- * Destructuring the response
    PurchaseReservedInstancesOfferingResponse (..),
    mkPurchaseReservedInstancesOfferingResponse,

    -- ** Response lenses
    priorsReservedInstancesId,
    priorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for PurchaseReservedInstancesOffering.
--
-- /See:/ 'mkPurchaseReservedInstancesOffering' smart constructor.
data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering'
  { purchaseTime ::
      Lude.Maybe
        Lude.DateTime,
    limitPrice ::
      Lude.Maybe
        ReservedInstanceLimitPrice,
    dryRun ::
      Lude.Maybe Lude.Bool,
    instanceCount ::
      Lude.Int,
    reservedInstancesOfferingId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedInstancesOffering' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceCount' - The number of Reserved Instances to purchase.
-- * 'limitPrice' - Specified for Reserved Instance Marketplace offerings to limit the total order and ensure that the Reserved Instances are not purchased at unexpected prices.
-- * 'purchaseTime' - The time at which to purchase the Reserved Instance, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
-- * 'reservedInstancesOfferingId' - The ID of the Reserved Instance offering to purchase.
mkPurchaseReservedInstancesOffering ::
  -- | 'instanceCount'
  Lude.Int ->
  -- | 'reservedInstancesOfferingId'
  Lude.Text ->
  PurchaseReservedInstancesOffering
mkPurchaseReservedInstancesOffering
  pInstanceCount_
  pReservedInstancesOfferingId_ =
    PurchaseReservedInstancesOffering'
      { purchaseTime = Lude.Nothing,
        limitPrice = Lude.Nothing,
        dryRun = Lude.Nothing,
        instanceCount = pInstanceCount_,
        reservedInstancesOfferingId = pReservedInstancesOfferingId_
      }

-- | The time at which to purchase the Reserved Instance, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'purchaseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioPurchaseTime :: Lens.Lens' PurchaseReservedInstancesOffering (Lude.Maybe Lude.DateTime)
prioPurchaseTime = Lens.lens (purchaseTime :: PurchaseReservedInstancesOffering -> Lude.Maybe Lude.DateTime) (\s a -> s {purchaseTime = a} :: PurchaseReservedInstancesOffering)
{-# DEPRECATED prioPurchaseTime "Use generic-lens or generic-optics with 'purchaseTime' instead." #-}

-- | Specified for Reserved Instance Marketplace offerings to limit the total order and ensure that the Reserved Instances are not purchased at unexpected prices.
--
-- /Note:/ Consider using 'limitPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioLimitPrice :: Lens.Lens' PurchaseReservedInstancesOffering (Lude.Maybe ReservedInstanceLimitPrice)
prioLimitPrice = Lens.lens (limitPrice :: PurchaseReservedInstancesOffering -> Lude.Maybe ReservedInstanceLimitPrice) (\s a -> s {limitPrice = a} :: PurchaseReservedInstancesOffering)
{-# DEPRECATED prioLimitPrice "Use generic-lens or generic-optics with 'limitPrice' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioDryRun :: Lens.Lens' PurchaseReservedInstancesOffering (Lude.Maybe Lude.Bool)
prioDryRun = Lens.lens (dryRun :: PurchaseReservedInstancesOffering -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: PurchaseReservedInstancesOffering)
{-# DEPRECATED prioDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The number of Reserved Instances to purchase.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioInstanceCount :: Lens.Lens' PurchaseReservedInstancesOffering Lude.Int
prioInstanceCount = Lens.lens (instanceCount :: PurchaseReservedInstancesOffering -> Lude.Int) (\s a -> s {instanceCount = a} :: PurchaseReservedInstancesOffering)
{-# DEPRECATED prioInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The ID of the Reserved Instance offering to purchase.
--
-- /Note:/ Consider using 'reservedInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioReservedInstancesOfferingId :: Lens.Lens' PurchaseReservedInstancesOffering Lude.Text
prioReservedInstancesOfferingId = Lens.lens (reservedInstancesOfferingId :: PurchaseReservedInstancesOffering -> Lude.Text) (\s a -> s {reservedInstancesOfferingId = a} :: PurchaseReservedInstancesOffering)
{-# DEPRECATED prioReservedInstancesOfferingId "Use generic-lens or generic-optics with 'reservedInstancesOfferingId' instead." #-}

instance Lude.AWSRequest PurchaseReservedInstancesOffering where
  type
    Rs PurchaseReservedInstancesOffering =
      PurchaseReservedInstancesOfferingResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          PurchaseReservedInstancesOfferingResponse'
            Lude.<$> (x Lude..@? "reservedInstancesId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseReservedInstancesOffering where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PurchaseReservedInstancesOffering where
  toPath = Lude.const "/"

instance Lude.ToQuery PurchaseReservedInstancesOffering where
  toQuery PurchaseReservedInstancesOffering' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PurchaseReservedInstancesOffering" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "PurchaseTime" Lude.=: purchaseTime,
        "LimitPrice" Lude.=: limitPrice,
        "DryRun" Lude.=: dryRun,
        "InstanceCount" Lude.=: instanceCount,
        "ReservedInstancesOfferingId" Lude.=: reservedInstancesOfferingId
      ]

-- | Contains the output of PurchaseReservedInstancesOffering.
--
-- /See:/ 'mkPurchaseReservedInstancesOfferingResponse' smart constructor.
data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse'
  { reservedInstancesId ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'PurchaseReservedInstancesOfferingResponse' with the minimum fields required to make a request.
--
-- * 'reservedInstancesId' - The IDs of the purchased Reserved Instances.
-- * 'responseStatus' - The response status code.
mkPurchaseReservedInstancesOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseReservedInstancesOfferingResponse
mkPurchaseReservedInstancesOfferingResponse pResponseStatus_ =
  PurchaseReservedInstancesOfferingResponse'
    { reservedInstancesId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IDs of the purchased Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
priorsReservedInstancesId :: Lens.Lens' PurchaseReservedInstancesOfferingResponse (Lude.Maybe Lude.Text)
priorsReservedInstancesId = Lens.lens (reservedInstancesId :: PurchaseReservedInstancesOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: PurchaseReservedInstancesOfferingResponse)
{-# DEPRECATED priorsReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
priorsResponseStatus :: Lens.Lens' PurchaseReservedInstancesOfferingResponse Lude.Int
priorsResponseStatus = Lens.lens (responseStatus :: PurchaseReservedInstancesOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseReservedInstancesOfferingResponse)
{-# DEPRECATED priorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
