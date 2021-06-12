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
-- Module      : Network.AWS.GuardDuty.ListFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings for the specified detector ID.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListFindings
  ( -- * Creating a Request
    ListFindings (..),
    newListFindings,

    -- * Request Lenses
    listFindings_nextToken,
    listFindings_sortCriteria,
    listFindings_findingCriteria,
    listFindings_maxResults,
    listFindings_detectorId,

    -- * Destructuring the Response
    ListFindingsResponse (..),
    newListFindingsResponse,

    -- * Response Lenses
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findingIds,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFindings' smart constructor.
data ListFindings = ListFindings'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | Represents the criteria used for sorting findings.
    sortCriteria :: Core.Maybe SortCriteria,
    -- | Represents the criteria used for querying findings. Valid values
    -- include:
    --
    -- -   JSON field name
    --
    -- -   accountId
    --
    -- -   region
    --
    -- -   confidence
    --
    -- -   id
    --
    -- -   resource.accessKeyDetails.accessKeyId
    --
    -- -   resource.accessKeyDetails.principalId
    --
    -- -   resource.accessKeyDetails.userName
    --
    -- -   resource.accessKeyDetails.userType
    --
    -- -   resource.instanceDetails.iamInstanceProfile.id
    --
    -- -   resource.instanceDetails.imageId
    --
    -- -   resource.instanceDetails.instanceId
    --
    -- -   resource.instanceDetails.networkInterfaces.ipv6Addresses
    --
    -- -   resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
    --
    -- -   resource.instanceDetails.networkInterfaces.publicDnsName
    --
    -- -   resource.instanceDetails.networkInterfaces.publicIp
    --
    -- -   resource.instanceDetails.networkInterfaces.securityGroups.groupId
    --
    -- -   resource.instanceDetails.networkInterfaces.securityGroups.groupName
    --
    -- -   resource.instanceDetails.networkInterfaces.subnetId
    --
    -- -   resource.instanceDetails.networkInterfaces.vpcId
    --
    -- -   resource.instanceDetails.tags.key
    --
    -- -   resource.instanceDetails.tags.value
    --
    -- -   resource.resourceType
    --
    -- -   service.action.actionType
    --
    -- -   service.action.awsApiCallAction.api
    --
    -- -   service.action.awsApiCallAction.callerType
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.city.cityName
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.country.countryName
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.organization.asn
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
    --
    -- -   service.action.awsApiCallAction.serviceName
    --
    -- -   service.action.dnsRequestAction.domain
    --
    -- -   service.action.networkConnectionAction.blocked
    --
    -- -   service.action.networkConnectionAction.connectionDirection
    --
    -- -   service.action.networkConnectionAction.localPortDetails.port
    --
    -- -   service.action.networkConnectionAction.protocol
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.city.cityName
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.country.countryName
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.organization.asn
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
    --
    -- -   service.action.networkConnectionAction.remotePortDetails.port
    --
    -- -   service.additionalInfo.threatListName
    --
    -- -   service.archived
    --
    --     When this attribute is set to \'true\', only archived findings are
    --     listed. When it\'s set to \'false\', only unarchived findings are
    --     listed. When this attribute is not set, all existing findings are
    --     listed.
    --
    -- -   service.resourceRole
    --
    -- -   severity
    --
    -- -   type
    --
    -- -   updatedAt
    --
    --     Type: Timestamp in Unix Epoch millisecond format: 1486685375000
    findingCriteria :: Core.Maybe FindingCriteria,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the detector that specifies the GuardDuty service whose
    -- findings you want to list.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFindings_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'sortCriteria', 'listFindings_sortCriteria' - Represents the criteria used for sorting findings.
--
-- 'findingCriteria', 'listFindings_findingCriteria' - Represents the criteria used for querying findings. Valid values
-- include:
--
-- -   JSON field name
--
-- -   accountId
--
-- -   region
--
-- -   confidence
--
-- -   id
--
-- -   resource.accessKeyDetails.accessKeyId
--
-- -   resource.accessKeyDetails.principalId
--
-- -   resource.accessKeyDetails.userName
--
-- -   resource.accessKeyDetails.userType
--
-- -   resource.instanceDetails.iamInstanceProfile.id
--
-- -   resource.instanceDetails.imageId
--
-- -   resource.instanceDetails.instanceId
--
-- -   resource.instanceDetails.networkInterfaces.ipv6Addresses
--
-- -   resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
--
-- -   resource.instanceDetails.networkInterfaces.publicDnsName
--
-- -   resource.instanceDetails.networkInterfaces.publicIp
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupId
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupName
--
-- -   resource.instanceDetails.networkInterfaces.subnetId
--
-- -   resource.instanceDetails.networkInterfaces.vpcId
--
-- -   resource.instanceDetails.tags.key
--
-- -   resource.instanceDetails.tags.value
--
-- -   resource.resourceType
--
-- -   service.action.actionType
--
-- -   service.action.awsApiCallAction.api
--
-- -   service.action.awsApiCallAction.callerType
--
-- -   service.action.awsApiCallAction.remoteIpDetails.city.cityName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.country.countryName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asn
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.awsApiCallAction.serviceName
--
-- -   service.action.dnsRequestAction.domain
--
-- -   service.action.networkConnectionAction.blocked
--
-- -   service.action.networkConnectionAction.connectionDirection
--
-- -   service.action.networkConnectionAction.localPortDetails.port
--
-- -   service.action.networkConnectionAction.protocol
--
-- -   service.action.networkConnectionAction.remoteIpDetails.city.cityName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.country.countryName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asn
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.networkConnectionAction.remotePortDetails.port
--
-- -   service.additionalInfo.threatListName
--
-- -   service.archived
--
--     When this attribute is set to \'true\', only archived findings are
--     listed. When it\'s set to \'false\', only unarchived findings are
--     listed. When this attribute is not set, all existing findings are
--     listed.
--
-- -   service.resourceRole
--
-- -   severity
--
-- -   type
--
-- -   updatedAt
--
--     Type: Timestamp in Unix Epoch millisecond format: 1486685375000
--
-- 'maxResults', 'listFindings_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
--
-- 'detectorId', 'listFindings_detectorId' - The ID of the detector that specifies the GuardDuty service whose
-- findings you want to list.
newListFindings ::
  -- | 'detectorId'
  Core.Text ->
  ListFindings
newListFindings pDetectorId_ =
  ListFindings'
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      findingCriteria = Core.Nothing,
      maxResults = Core.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listFindings_nextToken :: Lens.Lens' ListFindings (Core.Maybe Core.Text)
listFindings_nextToken = Lens.lens (\ListFindings' {nextToken} -> nextToken) (\s@ListFindings' {} a -> s {nextToken = a} :: ListFindings)

-- | Represents the criteria used for sorting findings.
listFindings_sortCriteria :: Lens.Lens' ListFindings (Core.Maybe SortCriteria)
listFindings_sortCriteria = Lens.lens (\ListFindings' {sortCriteria} -> sortCriteria) (\s@ListFindings' {} a -> s {sortCriteria = a} :: ListFindings)

-- | Represents the criteria used for querying findings. Valid values
-- include:
--
-- -   JSON field name
--
-- -   accountId
--
-- -   region
--
-- -   confidence
--
-- -   id
--
-- -   resource.accessKeyDetails.accessKeyId
--
-- -   resource.accessKeyDetails.principalId
--
-- -   resource.accessKeyDetails.userName
--
-- -   resource.accessKeyDetails.userType
--
-- -   resource.instanceDetails.iamInstanceProfile.id
--
-- -   resource.instanceDetails.imageId
--
-- -   resource.instanceDetails.instanceId
--
-- -   resource.instanceDetails.networkInterfaces.ipv6Addresses
--
-- -   resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
--
-- -   resource.instanceDetails.networkInterfaces.publicDnsName
--
-- -   resource.instanceDetails.networkInterfaces.publicIp
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupId
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupName
--
-- -   resource.instanceDetails.networkInterfaces.subnetId
--
-- -   resource.instanceDetails.networkInterfaces.vpcId
--
-- -   resource.instanceDetails.tags.key
--
-- -   resource.instanceDetails.tags.value
--
-- -   resource.resourceType
--
-- -   service.action.actionType
--
-- -   service.action.awsApiCallAction.api
--
-- -   service.action.awsApiCallAction.callerType
--
-- -   service.action.awsApiCallAction.remoteIpDetails.city.cityName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.country.countryName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asn
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.awsApiCallAction.serviceName
--
-- -   service.action.dnsRequestAction.domain
--
-- -   service.action.networkConnectionAction.blocked
--
-- -   service.action.networkConnectionAction.connectionDirection
--
-- -   service.action.networkConnectionAction.localPortDetails.port
--
-- -   service.action.networkConnectionAction.protocol
--
-- -   service.action.networkConnectionAction.remoteIpDetails.city.cityName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.country.countryName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asn
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.networkConnectionAction.remotePortDetails.port
--
-- -   service.additionalInfo.threatListName
--
-- -   service.archived
--
--     When this attribute is set to \'true\', only archived findings are
--     listed. When it\'s set to \'false\', only unarchived findings are
--     listed. When this attribute is not set, all existing findings are
--     listed.
--
-- -   service.resourceRole
--
-- -   severity
--
-- -   type
--
-- -   updatedAt
--
--     Type: Timestamp in Unix Epoch millisecond format: 1486685375000
listFindings_findingCriteria :: Lens.Lens' ListFindings (Core.Maybe FindingCriteria)
listFindings_findingCriteria = Lens.lens (\ListFindings' {findingCriteria} -> findingCriteria) (\s@ListFindings' {} a -> s {findingCriteria = a} :: ListFindings)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
listFindings_maxResults :: Lens.Lens' ListFindings (Core.Maybe Core.Natural)
listFindings_maxResults = Lens.lens (\ListFindings' {maxResults} -> maxResults) (\s@ListFindings' {} a -> s {maxResults = a} :: ListFindings)

-- | The ID of the detector that specifies the GuardDuty service whose
-- findings you want to list.
listFindings_detectorId :: Lens.Lens' ListFindings Core.Text
listFindings_detectorId = Lens.lens (\ListFindings' {detectorId} -> detectorId) (\s@ListFindings' {} a -> s {detectorId = a} :: ListFindings)

instance Core.AWSPager ListFindings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFindingsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listFindingsResponse_findingIds) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFindings_nextToken
          Lens..~ rs
          Lens.^? listFindingsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListFindings where
  type AWSResponse ListFindings = ListFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFindingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "findingIds" Core..!@ Core.mempty)
      )

instance Core.Hashable ListFindings

instance Core.NFData ListFindings

instance Core.ToHeaders ListFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListFindings where
  toJSON ListFindings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("sortCriteria" Core..=) Core.<$> sortCriteria,
            ("findingCriteria" Core..=) Core.<$> findingCriteria,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListFindings where
  toPath ListFindings' {..} =
    Core.mconcat
      ["/detector/", Core.toBS detectorId, "/findings"]

instance Core.ToQuery ListFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The IDs of the findings that you\'re listing.
    findingIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFindingsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'listFindingsResponse_httpStatus' - The response's http status code.
--
-- 'findingIds', 'listFindingsResponse_findingIds' - The IDs of the findings that you\'re listing.
newListFindingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFindingsResponse
newListFindingsResponse pHttpStatus_ =
  ListFindingsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      findingIds = Core.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listFindingsResponse_nextToken :: Lens.Lens' ListFindingsResponse (Core.Maybe Core.Text)
listFindingsResponse_nextToken = Lens.lens (\ListFindingsResponse' {nextToken} -> nextToken) (\s@ListFindingsResponse' {} a -> s {nextToken = a} :: ListFindingsResponse)

-- | The response's http status code.
listFindingsResponse_httpStatus :: Lens.Lens' ListFindingsResponse Core.Int
listFindingsResponse_httpStatus = Lens.lens (\ListFindingsResponse' {httpStatus} -> httpStatus) (\s@ListFindingsResponse' {} a -> s {httpStatus = a} :: ListFindingsResponse)

-- | The IDs of the findings that you\'re listing.
listFindingsResponse_findingIds :: Lens.Lens' ListFindingsResponse [Core.Text]
listFindingsResponse_findingIds = Lens.lens (\ListFindingsResponse' {findingIds} -> findingIds) (\s@ListFindingsResponse' {} a -> s {findingIds = a} :: ListFindingsResponse) Core.. Lens._Coerce

instance Core.NFData ListFindingsResponse
