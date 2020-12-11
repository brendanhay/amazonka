{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListFindings (..),
    mkListFindings,

    -- ** Request lenses
    lfFindingCriteria,
    lfSortCriteria,
    lfNextToken,
    lfMaxResults,
    lfDetectorId,

    -- * Destructuring the response
    ListFindingsResponse (..),
    mkListFindingsResponse,

    -- ** Response lenses
    lfrsNextToken,
    lfrsResponseStatus,
    lfrsFindingIds,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFindings' smart constructor.
data ListFindings = ListFindings'
  { findingCriteria ::
      Lude.Maybe FindingCriteria,
    sortCriteria :: Lude.Maybe SortCriteria,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    detectorId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFindings' with the minimum fields required to make a request.
--
-- * 'detectorId' - The ID of the detector that specifies the GuardDuty service whose findings you want to list.
-- * 'findingCriteria' - Represents the criteria used for querying findings. Valid values include:
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
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
-- * 'sortCriteria' - Represents the criteria used for sorting findings.
mkListFindings ::
  -- | 'detectorId'
  Lude.Text ->
  ListFindings
mkListFindings pDetectorId_ =
  ListFindings'
    { findingCriteria = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      detectorId = pDetectorId_
    }

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
lfFindingCriteria :: Lens.Lens' ListFindings (Lude.Maybe FindingCriteria)
lfFindingCriteria = Lens.lens (findingCriteria :: ListFindings -> Lude.Maybe FindingCriteria) (\s a -> s {findingCriteria = a} :: ListFindings)
{-# DEPRECATED lfFindingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead." #-}

-- | Represents the criteria used for sorting findings.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfSortCriteria :: Lens.Lens' ListFindings (Lude.Maybe SortCriteria)
lfSortCriteria = Lens.lens (sortCriteria :: ListFindings -> Lude.Maybe SortCriteria) (\s a -> s {sortCriteria = a} :: ListFindings)
{-# DEPRECATED lfSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFindings (Lude.Maybe Lude.Text)
lfNextToken = Lens.lens (nextToken :: ListFindings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFindings)
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFindings (Lude.Maybe Lude.Natural)
lfMaxResults = Lens.lens (maxResults :: ListFindings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFindings)
{-# DEPRECATED lfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to list.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfDetectorId :: Lens.Lens' ListFindings Lude.Text
lfDetectorId = Lens.lens (detectorId :: ListFindings -> Lude.Text) (\s a -> s {detectorId = a} :: ListFindings)
{-# DEPRECATED lfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Page.AWSPager ListFindings where
  page rq rs
    | Page.stop (rs Lens.^. lfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfrsFindingIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfNextToken Lens..~ rs Lens.^. lfrsNextToken

instance Lude.AWSRequest ListFindings where
  type Rs ListFindings = ListFindingsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFindingsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "findingIds" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListFindings where
  toJSON ListFindings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("findingCriteria" Lude..=) Lude.<$> findingCriteria,
            ("sortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListFindings where
  toPath ListFindings' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/findings"]

instance Lude.ToQuery ListFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    findingIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFindingsResponse' with the minimum fields required to make a request.
--
-- * 'findingIds' - The IDs of the findings that you're listing.
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
mkListFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFindingsResponse
mkListFindingsResponse pResponseStatus_ =
  ListFindingsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      findingIds = Lude.mempty
    }

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsNextToken :: Lens.Lens' ListFindingsResponse (Lude.Maybe Lude.Text)
lfrsNextToken = Lens.lens (nextToken :: ListFindingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFindingsResponse)
{-# DEPRECATED lfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsResponseStatus :: Lens.Lens' ListFindingsResponse Lude.Int
lfrsResponseStatus = Lens.lens (responseStatus :: ListFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFindingsResponse)
{-# DEPRECATED lfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The IDs of the findings that you're listing.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsFindingIds :: Lens.Lens' ListFindingsResponse [Lude.Text]
lfrsFindingIds = Lens.lens (findingIds :: ListFindingsResponse -> [Lude.Text]) (\s a -> s {findingIds = a} :: ListFindingsResponse)
{-# DEPRECATED lfrsFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}
