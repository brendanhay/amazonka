{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateReservedInstancesListing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listing for Amazon EC2 Standard Reserved Instances to be sold in the Reserved Instance Marketplace. You can submit one Standard Reserved Instance listing at a time. To get a list of your Standard Reserved Instances, you can use the 'DescribeReservedInstances' operation.
--
-- The Reserved Instance Marketplace matches sellers who want to resell Standard Reserved Instance capacity that they no longer need with buyers who want to purchase additional capacity. Reserved Instances bought and sold through the Reserved Instance Marketplace work like any other Reserved Instances.
-- To sell your Standard Reserved Instances, you must first register as a seller in the Reserved Instance Marketplace. After completing the registration process, you can create a Reserved Instance Marketplace listing of some or all of your Standard Reserved Instances, and specify the upfront price to receive for them. Your Standard Reserved Instance listings then become available for purchase. To view the details of your Standard Reserved Instance listing, you can use the 'DescribeReservedInstancesListings' operation.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateReservedInstancesListing
  ( -- * Creating a request
    CreateReservedInstancesListing (..),
    mkCreateReservedInstancesListing,

    -- ** Request lenses
    crilClientToken,
    crilInstanceCount,
    crilPriceSchedules,
    crilReservedInstancesId,

    -- * Destructuring the response
    CreateReservedInstancesListingResponse (..),
    mkCreateReservedInstancesListingResponse,

    -- ** Response lenses
    crilfrsReservedInstancesListings,
    crilfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateReservedInstancesListing.
--
-- /See:/ 'mkCreateReservedInstancesListing' smart constructor.
data CreateReservedInstancesListing = CreateReservedInstancesListing'
  { -- | Unique, case-sensitive identifier you provide to ensure idempotency of your listings. This helps avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Lude.Text,
    -- | The number of instances that are a part of a Reserved Instance account to be listed in the Reserved Instance Marketplace. This number should be less than or equal to the instance count associated with the Reserved Instance ID specified in this call.
    instanceCount :: Lude.Int,
    -- | A list specifying the price of the Standard Reserved Instance for each month remaining in the Reserved Instance term.
    priceSchedules :: [PriceScheduleSpecification],
    -- | The ID of the active Standard Reserved Instance.
    reservedInstancesId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReservedInstancesListing' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of your listings. This helps avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'instanceCount' - The number of instances that are a part of a Reserved Instance account to be listed in the Reserved Instance Marketplace. This number should be less than or equal to the instance count associated with the Reserved Instance ID specified in this call.
-- * 'priceSchedules' - A list specifying the price of the Standard Reserved Instance for each month remaining in the Reserved Instance term.
-- * 'reservedInstancesId' - The ID of the active Standard Reserved Instance.
mkCreateReservedInstancesListing ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'instanceCount'
  Lude.Int ->
  -- | 'reservedInstancesId'
  Lude.Text ->
  CreateReservedInstancesListing
mkCreateReservedInstancesListing
  pClientToken_
  pInstanceCount_
  pReservedInstancesId_ =
    CreateReservedInstancesListing'
      { clientToken = pClientToken_,
        instanceCount = pInstanceCount_,
        priceSchedules = Lude.mempty,
        reservedInstancesId = pReservedInstancesId_
      }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of your listings. This helps avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilClientToken :: Lens.Lens' CreateReservedInstancesListing Lude.Text
crilClientToken = Lens.lens (clientToken :: CreateReservedInstancesListing -> Lude.Text) (\s a -> s {clientToken = a} :: CreateReservedInstancesListing)
{-# DEPRECATED crilClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of instances that are a part of a Reserved Instance account to be listed in the Reserved Instance Marketplace. This number should be less than or equal to the instance count associated with the Reserved Instance ID specified in this call.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilInstanceCount :: Lens.Lens' CreateReservedInstancesListing Lude.Int
crilInstanceCount = Lens.lens (instanceCount :: CreateReservedInstancesListing -> Lude.Int) (\s a -> s {instanceCount = a} :: CreateReservedInstancesListing)
{-# DEPRECATED crilInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | A list specifying the price of the Standard Reserved Instance for each month remaining in the Reserved Instance term.
--
-- /Note:/ Consider using 'priceSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilPriceSchedules :: Lens.Lens' CreateReservedInstancesListing [PriceScheduleSpecification]
crilPriceSchedules = Lens.lens (priceSchedules :: CreateReservedInstancesListing -> [PriceScheduleSpecification]) (\s a -> s {priceSchedules = a} :: CreateReservedInstancesListing)
{-# DEPRECATED crilPriceSchedules "Use generic-lens or generic-optics with 'priceSchedules' instead." #-}

-- | The ID of the active Standard Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilReservedInstancesId :: Lens.Lens' CreateReservedInstancesListing Lude.Text
crilReservedInstancesId = Lens.lens (reservedInstancesId :: CreateReservedInstancesListing -> Lude.Text) (\s a -> s {reservedInstancesId = a} :: CreateReservedInstancesListing)
{-# DEPRECATED crilReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

instance Lude.AWSRequest CreateReservedInstancesListing where
  type
    Rs CreateReservedInstancesListing =
      CreateReservedInstancesListingResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateReservedInstancesListingResponse'
            Lude.<$> ( x Lude..@? "reservedInstancesListingsSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReservedInstancesListing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateReservedInstancesListing where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReservedInstancesListing where
  toQuery CreateReservedInstancesListing' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateReservedInstancesListing" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "InstanceCount" Lude.=: instanceCount,
        Lude.toQueryList "PriceSchedules" priceSchedules,
        "ReservedInstancesId" Lude.=: reservedInstancesId
      ]

-- | Contains the output of CreateReservedInstancesListing.
--
-- /See:/ 'mkCreateReservedInstancesListingResponse' smart constructor.
data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse'
  { -- | Information about the Standard Reserved Instance listing.
    reservedInstancesListings :: Lude.Maybe [ReservedInstancesListing],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReservedInstancesListingResponse' with the minimum fields required to make a request.
--
-- * 'reservedInstancesListings' - Information about the Standard Reserved Instance listing.
-- * 'responseStatus' - The response status code.
mkCreateReservedInstancesListingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReservedInstancesListingResponse
mkCreateReservedInstancesListingResponse pResponseStatus_ =
  CreateReservedInstancesListingResponse'
    { reservedInstancesListings =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Standard Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilfrsReservedInstancesListings :: Lens.Lens' CreateReservedInstancesListingResponse (Lude.Maybe [ReservedInstancesListing])
crilfrsReservedInstancesListings = Lens.lens (reservedInstancesListings :: CreateReservedInstancesListingResponse -> Lude.Maybe [ReservedInstancesListing]) (\s a -> s {reservedInstancesListings = a} :: CreateReservedInstancesListingResponse)
{-# DEPRECATED crilfrsReservedInstancesListings "Use generic-lens or generic-optics with 'reservedInstancesListings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilfrsResponseStatus :: Lens.Lens' CreateReservedInstancesListingResponse Lude.Int
crilfrsResponseStatus = Lens.lens (responseStatus :: CreateReservedInstancesListingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReservedInstancesListingResponse)
{-# DEPRECATED crilfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
