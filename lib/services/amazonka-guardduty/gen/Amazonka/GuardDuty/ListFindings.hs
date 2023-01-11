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
-- Module      : Amazonka.GuardDuty.ListFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings for the specified detector ID.
--
-- This operation returns paginated results.
module Amazonka.GuardDuty.ListFindings
  ( -- * Creating a Request
    ListFindings (..),
    newListFindings,

    -- * Request Lenses
    listFindings_findingCriteria,
    listFindings_maxResults,
    listFindings_nextToken,
    listFindings_sortCriteria,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFindings' smart constructor.
data ListFindings = ListFindings'
  { -- | Represents the criteria used for querying findings. Valid values
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
    findingCriteria :: Prelude.Maybe FindingCriteria,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the criteria used for sorting findings.
    sortCriteria :: Prelude.Maybe SortCriteria,
    -- | The ID of the detector that specifies the GuardDuty service whose
    -- findings you want to list.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'nextToken', 'listFindings_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'sortCriteria', 'listFindings_sortCriteria' - Represents the criteria used for sorting findings.
--
-- 'detectorId', 'listFindings_detectorId' - The ID of the detector that specifies the GuardDuty service whose
-- findings you want to list.
newListFindings ::
  -- | 'detectorId'
  Prelude.Text ->
  ListFindings
newListFindings pDetectorId_ =
  ListFindings'
    { findingCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing,
      detectorId = pDetectorId_
    }

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
listFindings_findingCriteria :: Lens.Lens' ListFindings (Prelude.Maybe FindingCriteria)
listFindings_findingCriteria = Lens.lens (\ListFindings' {findingCriteria} -> findingCriteria) (\s@ListFindings' {} a -> s {findingCriteria = a} :: ListFindings)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
listFindings_maxResults :: Lens.Lens' ListFindings (Prelude.Maybe Prelude.Natural)
listFindings_maxResults = Lens.lens (\ListFindings' {maxResults} -> maxResults) (\s@ListFindings' {} a -> s {maxResults = a} :: ListFindings)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listFindings_nextToken :: Lens.Lens' ListFindings (Prelude.Maybe Prelude.Text)
listFindings_nextToken = Lens.lens (\ListFindings' {nextToken} -> nextToken) (\s@ListFindings' {} a -> s {nextToken = a} :: ListFindings)

-- | Represents the criteria used for sorting findings.
listFindings_sortCriteria :: Lens.Lens' ListFindings (Prelude.Maybe SortCriteria)
listFindings_sortCriteria = Lens.lens (\ListFindings' {sortCriteria} -> sortCriteria) (\s@ListFindings' {} a -> s {sortCriteria = a} :: ListFindings)

-- | The ID of the detector that specifies the GuardDuty service whose
-- findings you want to list.
listFindings_detectorId :: Lens.Lens' ListFindings Prelude.Text
listFindings_detectorId = Lens.lens (\ListFindings' {detectorId} -> detectorId) (\s@ListFindings' {} a -> s {detectorId = a} :: ListFindings)

instance Core.AWSPager ListFindings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFindingsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listFindingsResponse_findingIds) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFindings_nextToken
          Lens..~ rs
          Lens.^? listFindingsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFindings where
  type AWSResponse ListFindings = ListFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFindingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "findingIds" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListFindings where
  hashWithSalt _salt ListFindings' {..} =
    _salt `Prelude.hashWithSalt` findingCriteria
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData ListFindings where
  rnf ListFindings' {..} =
    Prelude.rnf findingCriteria
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf detectorId

instance Data.ToHeaders ListFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFindings where
  toJSON ListFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("findingCriteria" Data..=)
              Prelude.<$> findingCriteria,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath ListFindings where
  toPath ListFindings' {..} =
    Prelude.mconcat
      ["/detector/", Data.toBS detectorId, "/findings"]

instance Data.ToQuery ListFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The IDs of the findings that you\'re listing.
    findingIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListFindingsResponse
newListFindingsResponse pHttpStatus_ =
  ListFindingsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      findingIds = Prelude.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listFindingsResponse_nextToken :: Lens.Lens' ListFindingsResponse (Prelude.Maybe Prelude.Text)
listFindingsResponse_nextToken = Lens.lens (\ListFindingsResponse' {nextToken} -> nextToken) (\s@ListFindingsResponse' {} a -> s {nextToken = a} :: ListFindingsResponse)

-- | The response's http status code.
listFindingsResponse_httpStatus :: Lens.Lens' ListFindingsResponse Prelude.Int
listFindingsResponse_httpStatus = Lens.lens (\ListFindingsResponse' {httpStatus} -> httpStatus) (\s@ListFindingsResponse' {} a -> s {httpStatus = a} :: ListFindingsResponse)

-- | The IDs of the findings that you\'re listing.
listFindingsResponse_findingIds :: Lens.Lens' ListFindingsResponse [Prelude.Text]
listFindingsResponse_findingIds = Lens.lens (\ListFindingsResponse' {findingIds} -> findingIds) (\s@ListFindingsResponse' {} a -> s {findingIds = a} :: ListFindingsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFindingsResponse where
  rnf ListFindingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf findingIds
