{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeReservedInstances (..)
    , mkDescribeReservedInstances
    -- ** Request lenses
    , driDryRun
    , driFilters
    , driOfferingClass
    , driOfferingType
    , driReservedInstancesIds

    -- * Destructuring the response
    , DescribeReservedInstancesResponse (..)
    , mkDescribeReservedInstancesResponse
    -- ** Response lenses
    , drirrsReservedInstances
    , drirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeReservedInstances.
--
-- /See:/ 'mkDescribeReservedInstances' smart constructor.
data DescribeReservedInstances = DescribeReservedInstances'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
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
  , offeringClass :: Core.Maybe Types.OfferingClassType
    -- ^ Describes whether the Reserved Instance is Standard or Convertible.
  , offeringType :: Core.Maybe Types.OfferingTypeValues
    -- ^ The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
  , reservedInstancesIds :: Core.Maybe [Types.ReservationId]
    -- ^ One or more Reserved Instance IDs.
--
-- Default: Describes all your Reserved Instances, or only those otherwise specified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedInstances' value with any optional fields omitted.
mkDescribeReservedInstances
    :: DescribeReservedInstances
mkDescribeReservedInstances
  = DescribeReservedInstances'{dryRun = Core.Nothing,
                               filters = Core.Nothing, offeringClass = Core.Nothing,
                               offeringType = Core.Nothing, reservedInstancesIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driDryRun :: Lens.Lens' DescribeReservedInstances (Core.Maybe Core.Bool)
driDryRun = Lens.field @"dryRun"
{-# INLINEABLE driDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
driFilters :: Lens.Lens' DescribeReservedInstances (Core.Maybe [Types.Filter])
driFilters = Lens.field @"filters"
{-# INLINEABLE driFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | Describes whether the Reserved Instance is Standard or Convertible.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driOfferingClass :: Lens.Lens' DescribeReservedInstances (Core.Maybe Types.OfferingClassType)
driOfferingClass = Lens.field @"offeringClass"
{-# INLINEABLE driOfferingClass #-}
{-# DEPRECATED offeringClass "Use generic-lens or generic-optics with 'offeringClass' instead"  #-}

-- | The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driOfferingType :: Lens.Lens' DescribeReservedInstances (Core.Maybe Types.OfferingTypeValues)
driOfferingType = Lens.field @"offeringType"
{-# INLINEABLE driOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | One or more Reserved Instance IDs.
--
-- Default: Describes all your Reserved Instances, or only those otherwise specified.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driReservedInstancesIds :: Lens.Lens' DescribeReservedInstances (Core.Maybe [Types.ReservationId])
driReservedInstancesIds = Lens.field @"reservedInstancesIds"
{-# INLINEABLE driReservedInstancesIds #-}
{-# DEPRECATED reservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead"  #-}

instance Core.ToQuery DescribeReservedInstances where
        toQuery DescribeReservedInstances{..}
          = Core.toQueryPair "Action"
              ("DescribeReservedInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OfferingClass")
                offeringClass
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OfferingType")
                offeringType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ReservedInstancesId")
                reservedInstancesIds

instance Core.ToHeaders DescribeReservedInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeReservedInstances where
        type Rs DescribeReservedInstances =
             DescribeReservedInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeReservedInstancesResponse' Core.<$>
                   (x Core..@? "reservedInstancesSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output for DescribeReservedInstances.
--
-- /See:/ 'mkDescribeReservedInstancesResponse' smart constructor.
data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse'
  { reservedInstances :: Core.Maybe [Types.ReservedInstances]
    -- ^ A list of Reserved Instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReservedInstancesResponse' value with any optional fields omitted.
mkDescribeReservedInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReservedInstancesResponse
mkDescribeReservedInstancesResponse responseStatus
  = DescribeReservedInstancesResponse'{reservedInstances =
                                         Core.Nothing,
                                       responseStatus}

-- | A list of Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirrsReservedInstances :: Lens.Lens' DescribeReservedInstancesResponse (Core.Maybe [Types.ReservedInstances])
drirrsReservedInstances = Lens.field @"reservedInstances"
{-# INLINEABLE drirrsReservedInstances #-}
{-# DEPRECATED reservedInstances "Use generic-lens or generic-optics with 'reservedInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirrsResponseStatus :: Lens.Lens' DescribeReservedInstancesResponse Core.Int
drirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
