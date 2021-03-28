{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings for the specified detector ID.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListFindings
    (
    -- * Creating a request
      ListFindings (..)
    , mkListFindings
    -- ** Request lenses
    , lfDetectorId
    , lfFindingCriteria
    , lfMaxResults
    , lfNextToken
    , lfSortCriteria

    -- * Destructuring the response
    , ListFindingsResponse (..)
    , mkListFindingsResponse
    -- ** Response lenses
    , lfrrsFindingIds
    , lfrrsNextToken
    , lfrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFindings' smart constructor.
data ListFindings = ListFindings'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector that specifies the GuardDuty service whose findings you want to list.
  , findingCriteria :: Core.Maybe Types.FindingCriteria
    -- ^ Represents the criteria used for querying findings. Valid values include:
--
--
--     * JSON field name
--
--
--     * accountId
--
--
--     * region
--
--
--     * confidence
--
--
--     * id
--
--
--     * resource.accessKeyDetails.accessKeyId
--
--
--     * resource.accessKeyDetails.principalId
--
--
--     * resource.accessKeyDetails.userName
--
--
--     * resource.accessKeyDetails.userType
--
--
--     * resource.instanceDetails.iamInstanceProfile.id
--
--
--     * resource.instanceDetails.imageId
--
--
--     * resource.instanceDetails.instanceId
--
--
--     * resource.instanceDetails.networkInterfaces.ipv6Addresses
--
--
--     * resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
--
--
--     * resource.instanceDetails.networkInterfaces.publicDnsName
--
--
--     * resource.instanceDetails.networkInterfaces.publicIp
--
--
--     * resource.instanceDetails.networkInterfaces.securityGroups.groupId
--
--
--     * resource.instanceDetails.networkInterfaces.securityGroups.groupName
--
--
--     * resource.instanceDetails.networkInterfaces.subnetId
--
--
--     * resource.instanceDetails.networkInterfaces.vpcId
--
--
--     * resource.instanceDetails.tags.key
--
--
--     * resource.instanceDetails.tags.value
--
--
--     * resource.resourceType
--
--
--     * service.action.actionType
--
--
--     * service.action.awsApiCallAction.api
--
--
--     * service.action.awsApiCallAction.callerType
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.city.cityName
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.country.countryName
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.organization.asn
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
--
--
--     * service.action.awsApiCallAction.serviceName
--
--
--     * service.action.dnsRequestAction.domain
--
--
--     * service.action.networkConnectionAction.blocked
--
--
--     * service.action.networkConnectionAction.connectionDirection
--
--
--     * service.action.networkConnectionAction.localPortDetails.port
--
--
--     * service.action.networkConnectionAction.protocol
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.city.cityName
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.country.countryName
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.organization.asn
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
--
--
--     * service.action.networkConnectionAction.remotePortDetails.port
--
--
--     * service.additionalInfo.threatListName
--
--
--     * service.archived
-- When this attribute is set to 'true', only archived findings are listed. When it's set to 'false', only unarchived findings are listed. When this attribute is not set, all existing findings are listed.
--
--
--     * service.resourceRole
--
--
--     * severity
--
--
--     * type
--
--
--     * updatedAt
-- Type: Timestamp in Unix Epoch millisecond format: 1486685375000
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
  , nextToken :: Core.Maybe Core.Text
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
  , sortCriteria :: Core.Maybe Types.SortCriteria
    -- ^ Represents the criteria used for sorting findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFindings' value with any optional fields omitted.
mkListFindings
    :: Types.DetectorId -- ^ 'detectorId'
    -> ListFindings
mkListFindings detectorId
  = ListFindings'{detectorId, findingCriteria = Core.Nothing,
                  maxResults = Core.Nothing, nextToken = Core.Nothing,
                  sortCriteria = Core.Nothing}

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to list.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfDetectorId :: Lens.Lens' ListFindings Types.DetectorId
lfDetectorId = Lens.field @"detectorId"
{-# INLINEABLE lfDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | Represents the criteria used for querying findings. Valid values include:
--
--
--     * JSON field name
--
--
--     * accountId
--
--
--     * region
--
--
--     * confidence
--
--
--     * id
--
--
--     * resource.accessKeyDetails.accessKeyId
--
--
--     * resource.accessKeyDetails.principalId
--
--
--     * resource.accessKeyDetails.userName
--
--
--     * resource.accessKeyDetails.userType
--
--
--     * resource.instanceDetails.iamInstanceProfile.id
--
--
--     * resource.instanceDetails.imageId
--
--
--     * resource.instanceDetails.instanceId
--
--
--     * resource.instanceDetails.networkInterfaces.ipv6Addresses
--
--
--     * resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
--
--
--     * resource.instanceDetails.networkInterfaces.publicDnsName
--
--
--     * resource.instanceDetails.networkInterfaces.publicIp
--
--
--     * resource.instanceDetails.networkInterfaces.securityGroups.groupId
--
--
--     * resource.instanceDetails.networkInterfaces.securityGroups.groupName
--
--
--     * resource.instanceDetails.networkInterfaces.subnetId
--
--
--     * resource.instanceDetails.networkInterfaces.vpcId
--
--
--     * resource.instanceDetails.tags.key
--
--
--     * resource.instanceDetails.tags.value
--
--
--     * resource.resourceType
--
--
--     * service.action.actionType
--
--
--     * service.action.awsApiCallAction.api
--
--
--     * service.action.awsApiCallAction.callerType
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.city.cityName
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.country.countryName
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.organization.asn
--
--
--     * service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
--
--
--     * service.action.awsApiCallAction.serviceName
--
--
--     * service.action.dnsRequestAction.domain
--
--
--     * service.action.networkConnectionAction.blocked
--
--
--     * service.action.networkConnectionAction.connectionDirection
--
--
--     * service.action.networkConnectionAction.localPortDetails.port
--
--
--     * service.action.networkConnectionAction.protocol
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.city.cityName
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.country.countryName
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.organization.asn
--
--
--     * service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
--
--
--     * service.action.networkConnectionAction.remotePortDetails.port
--
--
--     * service.additionalInfo.threatListName
--
--
--     * service.archived
-- When this attribute is set to 'true', only archived findings are listed. When it's set to 'false', only unarchived findings are listed. When this attribute is not set, all existing findings are listed.
--
--
--     * service.resourceRole
--
--
--     * severity
--
--
--     * type
--
--
--     * updatedAt
-- Type: Timestamp in Unix Epoch millisecond format: 1486685375000
--
--
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFindingCriteria :: Lens.Lens' ListFindings (Core.Maybe Types.FindingCriteria)
lfFindingCriteria = Lens.field @"findingCriteria"
{-# INLINEABLE lfFindingCriteria #-}
{-# DEPRECATED findingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFindings (Core.Maybe Core.Natural)
lfMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lfMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFindings (Core.Maybe Core.Text)
lfNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Represents the criteria used for sorting findings.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfSortCriteria :: Lens.Lens' ListFindings (Core.Maybe Types.SortCriteria)
lfSortCriteria = Lens.field @"sortCriteria"
{-# INLINEABLE lfSortCriteria #-}
{-# DEPRECATED sortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead"  #-}

instance Core.ToQuery ListFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListFindings where
        toHeaders ListFindings{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListFindings where
        toJSON ListFindings{..}
          = Core.object
              (Core.catMaybes
                 [("findingCriteria" Core..=) Core.<$> findingCriteria,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("sortCriteria" Core..=) Core.<$> sortCriteria])

instance Core.AWSRequest ListFindings where
        type Rs ListFindings = ListFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/findings",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFindingsResponse' Core.<$>
                   (x Core..:? "findingIds" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFindings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"findingIds") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
  { findingIds :: [Types.FindingId]
    -- ^ The IDs of the findings that you're listing.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination parameter to be used on the next list operation to retrieve more items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFindingsResponse' value with any optional fields omitted.
mkListFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFindingsResponse
mkListFindingsResponse responseStatus
  = ListFindingsResponse'{findingIds = Core.mempty,
                          nextToken = Core.Nothing, responseStatus}

-- | The IDs of the findings that you're listing.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFindingIds :: Lens.Lens' ListFindingsResponse [Types.FindingId]
lfrrsFindingIds = Lens.field @"findingIds"
{-# INLINEABLE lfrrsFindingIds #-}
{-# DEPRECATED findingIds "Use generic-lens or generic-optics with 'findingIds' instead"  #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsNextToken :: Lens.Lens' ListFindingsResponse (Core.Maybe Core.Text)
lfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsResponseStatus :: Lens.Lens' ListFindingsResponse Core.Int
lfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
