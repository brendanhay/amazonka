{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the Reserved Instances that you purchased.
--
-- For more information about Reserved Instances, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeReservedInstances
  ( -- * Creating a request
    DescribeReservedInstances (..),
    mkDescribeReservedInstances,

    -- ** Request lenses
    driFilters,
    driReservedInstancesIds,
    driOfferingType,
    driOfferingClass,
    driDryRun,

    -- * Destructuring the response
    DescribeReservedInstancesResponse (..),
    mkDescribeReservedInstancesResponse,

    -- ** Response lenses
    drirsReservedInstances,
    drirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeReservedInstances.
--
-- /See:/ 'mkDescribeReservedInstances' smart constructor.
data DescribeReservedInstances = DescribeReservedInstances'
  { filters ::
      Lude.Maybe [Filter],
    reservedInstancesIds ::
      Lude.Maybe [Lude.Text],
    offeringType ::
      Lude.Maybe OfferingTypeValues,
    offeringClass ::
      Lude.Maybe OfferingClassType,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedInstances' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @availability-zone@ - The Availability Zone where the Reserved Instance can be used.
--
--
--     * @duration@ - The duration of the Reserved Instance (one year or three years), in seconds (@31536000@ | @94608000@ ).
--
--
--     * @end@ - The time when the Reserved Instance expires (for example, 2015-08-07T11:54:42.000Z).
--
--
--     * @fixed-price@ - The purchase price of the Reserved Instance (for example, 9800.0).
--
--
--     * @instance-type@ - The instance type that is covered by the reservation.
--
--
--     * @scope@ - The scope of the Reserved Instance (@Region@ or @Availability Zone@ ).
--
--
--     * @product-description@ - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the product platform description will only be displayed to EC2-Classic account holders and are for use with Amazon VPC (@Linux/UNIX@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux@ | @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ | @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ | @Windows with SQL Server Standard (Amazon VPC)@ | @Windows with SQL Server Web@ | @Windows with SQL Server Web (Amazon VPC)@ | @Windows with SQL Server Enterprise@ | @Windows with SQL Server Enterprise (Amazon VPC)@ ).
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instance.
--
--
--     * @start@ - The time at which the Reserved Instance purchase request was placed (for example, 2014-08-07T11:54:42.000Z).
--
--
--     * @state@ - The state of the Reserved Instance (@payment-pending@ | @active@ | @payment-failed@ | @retired@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @usage-price@ - The usage price of the Reserved Instance, per hour (for example, 0.84).
--
--
-- * 'offeringClass' - Describes whether the Reserved Instance is Standard or Convertible.
-- * 'offeringType' - The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
-- * 'reservedInstancesIds' - One or more Reserved Instance IDs.
--
-- Default: Describes all your Reserved Instances, or only those otherwise specified.
mkDescribeReservedInstances ::
  DescribeReservedInstances
mkDescribeReservedInstances =
  DescribeReservedInstances'
    { filters = Lude.Nothing,
      reservedInstancesIds = Lude.Nothing,
      offeringType = Lude.Nothing,
      offeringClass = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @availability-zone@ - The Availability Zone where the Reserved Instance can be used.
--
--
--     * @duration@ - The duration of the Reserved Instance (one year or three years), in seconds (@31536000@ | @94608000@ ).
--
--
--     * @end@ - The time when the Reserved Instance expires (for example, 2015-08-07T11:54:42.000Z).
--
--
--     * @fixed-price@ - The purchase price of the Reserved Instance (for example, 9800.0).
--
--
--     * @instance-type@ - The instance type that is covered by the reservation.
--
--
--     * @scope@ - The scope of the Reserved Instance (@Region@ or @Availability Zone@ ).
--
--
--     * @product-description@ - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the product platform description will only be displayed to EC2-Classic account holders and are for use with Amazon VPC (@Linux/UNIX@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux@ | @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ | @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ | @Windows with SQL Server Standard (Amazon VPC)@ | @Windows with SQL Server Web@ | @Windows with SQL Server Web (Amazon VPC)@ | @Windows with SQL Server Enterprise@ | @Windows with SQL Server Enterprise (Amazon VPC)@ ).
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instance.
--
--
--     * @start@ - The time at which the Reserved Instance purchase request was placed (for example, 2014-08-07T11:54:42.000Z).
--
--
--     * @state@ - The state of the Reserved Instance (@payment-pending@ | @active@ | @payment-failed@ | @retired@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @usage-price@ - The usage price of the Reserved Instance, per hour (for example, 0.84).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driFilters :: Lens.Lens' DescribeReservedInstances (Lude.Maybe [Filter])
driFilters = Lens.lens (filters :: DescribeReservedInstances -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReservedInstances)
{-# DEPRECATED driFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more Reserved Instance IDs.
--
-- Default: Describes all your Reserved Instances, or only those otherwise specified.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driReservedInstancesIds :: Lens.Lens' DescribeReservedInstances (Lude.Maybe [Lude.Text])
driReservedInstancesIds = Lens.lens (reservedInstancesIds :: DescribeReservedInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {reservedInstancesIds = a} :: DescribeReservedInstances)
{-# DEPRECATED driReservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead." #-}

-- | The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driOfferingType :: Lens.Lens' DescribeReservedInstances (Lude.Maybe OfferingTypeValues)
driOfferingType = Lens.lens (offeringType :: DescribeReservedInstances -> Lude.Maybe OfferingTypeValues) (\s a -> s {offeringType = a} :: DescribeReservedInstances)
{-# DEPRECATED driOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Describes whether the Reserved Instance is Standard or Convertible.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driOfferingClass :: Lens.Lens' DescribeReservedInstances (Lude.Maybe OfferingClassType)
driOfferingClass = Lens.lens (offeringClass :: DescribeReservedInstances -> Lude.Maybe OfferingClassType) (\s a -> s {offeringClass = a} :: DescribeReservedInstances)
{-# DEPRECATED driOfferingClass "Use generic-lens or generic-optics with 'offeringClass' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driDryRun :: Lens.Lens' DescribeReservedInstances (Lude.Maybe Lude.Bool)
driDryRun = Lens.lens (dryRun :: DescribeReservedInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeReservedInstances)
{-# DEPRECATED driDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeReservedInstances where
  type
    Rs DescribeReservedInstances =
      DescribeReservedInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeReservedInstancesResponse'
            Lude.<$> ( x Lude..@? "reservedInstancesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedInstances where
  toQuery DescribeReservedInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeReservedInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          ( Lude.toQueryList "ReservedInstancesId"
              Lude.<$> reservedInstancesIds
          ),
        "OfferingType" Lude.=: offeringType,
        "OfferingClass" Lude.=: offeringClass,
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output for DescribeReservedInstances.
--
-- /See:/ 'mkDescribeReservedInstancesResponse' smart constructor.
data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse'
  { reservedInstances ::
      Lude.Maybe
        [ReservedInstances],
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

-- | Creates a value of 'DescribeReservedInstancesResponse' with the minimum fields required to make a request.
--
-- * 'reservedInstances' - A list of Reserved Instances.
-- * 'responseStatus' - The response status code.
mkDescribeReservedInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedInstancesResponse
mkDescribeReservedInstancesResponse pResponseStatus_ =
  DescribeReservedInstancesResponse'
    { reservedInstances =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirsReservedInstances :: Lens.Lens' DescribeReservedInstancesResponse (Lude.Maybe [ReservedInstances])
drirsReservedInstances = Lens.lens (reservedInstances :: DescribeReservedInstancesResponse -> Lude.Maybe [ReservedInstances]) (\s a -> s {reservedInstances = a} :: DescribeReservedInstancesResponse)
{-# DEPRECATED drirsReservedInstances "Use generic-lens or generic-optics with 'reservedInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirsResponseStatus :: Lens.Lens' DescribeReservedInstancesResponse Lude.Int
drirsResponseStatus = Lens.lens (responseStatus :: DescribeReservedInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedInstancesResponse)
{-# DEPRECATED drirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
