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
-- Module      : Network.AWS.ResourceGroupsTagging.GetComplianceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a table that shows counts of resources that are noncompliant
-- with their tag policies.
--
-- For more information on tag policies, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html Tag Policies>
-- in the /AWS Organizations User Guide./
--
-- You can call this operation only from the organization\'s management
-- account and from the us-east-1 Region.
--
-- This operation supports pagination, where the response can be sent in
-- multiple pages. You should check the @PaginationToken@ response
-- parameter to determine if there are additional results available to
-- return. Repeat the query, passing the @PaginationToken@ response
-- parameter value as an input to the next request until you recieve a
-- @null@ value. A null value for @PaginationToken@ indicates that there
-- are no more results waiting to be returned.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetComplianceSummary
  ( -- * Creating a Request
    GetComplianceSummary (..),
    newGetComplianceSummary,

    -- * Request Lenses
    getComplianceSummary_maxResults,
    getComplianceSummary_regionFilters,
    getComplianceSummary_targetIdFilters,
    getComplianceSummary_paginationToken,
    getComplianceSummary_groupBy,
    getComplianceSummary_resourceTypeFilters,
    getComplianceSummary_tagKeyFilters,

    -- * Destructuring the Response
    GetComplianceSummaryResponse (..),
    newGetComplianceSummaryResponse,

    -- * Response Lenses
    getComplianceSummaryResponse_paginationToken,
    getComplianceSummaryResponse_summaryList,
    getComplianceSummaryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetComplianceSummary' smart constructor.
data GetComplianceSummary = GetComplianceSummary'
  { -- | Specifies the maximum number of results to be returned in each page. A
    -- query can return fewer than this maximum, even if there are more results
    -- still to return. You should always check the @PaginationToken@ response
    -- value to see if there are more results. You can specify a minimum of 1
    -- and a maximum value of 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies a list of AWS Regions to limit the output by. If you use this
    -- parameter, the count of returned noncompliant resources includes only
    -- resources in the specified Regions.
    regionFilters :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Specifies target identifiers (usually, specific account IDs) to limit
    -- the output by. If you use this parameter, the count of returned
    -- noncompliant resources includes only resources with the specified target
    -- IDs.
    targetIdFilters :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Specifies a @PaginationToken@ response value from a previous request to
    -- indicate that you want the next page of results. Leave this parameter
    -- empty in your initial request.
    paginationToken :: Core.Maybe Core.Text,
    -- | Specifies a list of attributes to group the counts of noncompliant
    -- resources by. If supplied, the counts are sorted by those attributes.
    groupBy :: Core.Maybe [GroupByAttribute],
    -- | Specifies that you want the response to include information for only
    -- resources of the specified types. The format of each resource type is
    -- @service[:resourceType]@. For example, specifying a resource type of
    -- @ec2@ returns all Amazon EC2 resources (which includes EC2 instances).
    -- Specifying a resource type of @ec2:instance@ returns only EC2 instances.
    --
    -- The string for each service name and resource type is the same as that
    -- embedded in a resource\'s Amazon Resource Name (ARN). Consult the /AWS
    -- General Reference/ for the following:
    --
    -- -   For a list of service name strings, see
    --     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>.
    --
    -- -   For resource type strings, see
    --     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs>.
    --
    -- -   For more information about ARNs, see
    --     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    --
    -- You can specify multiple resource types by using a comma separated
    -- array. The array can include up to 100 items. Note that the length
    -- constraint requirement applies to each resource type filter.
    resourceTypeFilters :: Core.Maybe [Core.Text],
    -- | Specifies that you want the response to include information for only
    -- resources that have tags with the specified tag keys. If you use this
    -- parameter, the count of returned noncompliant resources includes only
    -- resources that have the specified tag keys.
    tagKeyFilters :: Core.Maybe (Core.NonEmpty Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getComplianceSummary_maxResults' - Specifies the maximum number of results to be returned in each page. A
-- query can return fewer than this maximum, even if there are more results
-- still to return. You should always check the @PaginationToken@ response
-- value to see if there are more results. You can specify a minimum of 1
-- and a maximum value of 100.
--
-- 'regionFilters', 'getComplianceSummary_regionFilters' - Specifies a list of AWS Regions to limit the output by. If you use this
-- parameter, the count of returned noncompliant resources includes only
-- resources in the specified Regions.
--
-- 'targetIdFilters', 'getComplianceSummary_targetIdFilters' - Specifies target identifiers (usually, specific account IDs) to limit
-- the output by. If you use this parameter, the count of returned
-- noncompliant resources includes only resources with the specified target
-- IDs.
--
-- 'paginationToken', 'getComplianceSummary_paginationToken' - Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
--
-- 'groupBy', 'getComplianceSummary_groupBy' - Specifies a list of attributes to group the counts of noncompliant
-- resources by. If supplied, the counts are sorted by those attributes.
--
-- 'resourceTypeFilters', 'getComplianceSummary_resourceTypeFilters' - Specifies that you want the response to include information for only
-- resources of the specified types. The format of each resource type is
-- @service[:resourceType]@. For example, specifying a resource type of
-- @ec2@ returns all Amazon EC2 resources (which includes EC2 instances).
-- Specifying a resource type of @ec2:instance@ returns only EC2 instances.
--
-- The string for each service name and resource type is the same as that
-- embedded in a resource\'s Amazon Resource Name (ARN). Consult the /AWS
-- General Reference/ for the following:
--
-- -   For a list of service name strings, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>.
--
-- -   For resource type strings, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs>.
--
-- -   For more information about ARNs, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- You can specify multiple resource types by using a comma separated
-- array. The array can include up to 100 items. Note that the length
-- constraint requirement applies to each resource type filter.
--
-- 'tagKeyFilters', 'getComplianceSummary_tagKeyFilters' - Specifies that you want the response to include information for only
-- resources that have tags with the specified tag keys. If you use this
-- parameter, the count of returned noncompliant resources includes only
-- resources that have the specified tag keys.
newGetComplianceSummary ::
  GetComplianceSummary
newGetComplianceSummary =
  GetComplianceSummary'
    { maxResults = Core.Nothing,
      regionFilters = Core.Nothing,
      targetIdFilters = Core.Nothing,
      paginationToken = Core.Nothing,
      groupBy = Core.Nothing,
      resourceTypeFilters = Core.Nothing,
      tagKeyFilters = Core.Nothing
    }

-- | Specifies the maximum number of results to be returned in each page. A
-- query can return fewer than this maximum, even if there are more results
-- still to return. You should always check the @PaginationToken@ response
-- value to see if there are more results. You can specify a minimum of 1
-- and a maximum value of 100.
getComplianceSummary_maxResults :: Lens.Lens' GetComplianceSummary (Core.Maybe Core.Natural)
getComplianceSummary_maxResults = Lens.lens (\GetComplianceSummary' {maxResults} -> maxResults) (\s@GetComplianceSummary' {} a -> s {maxResults = a} :: GetComplianceSummary)

-- | Specifies a list of AWS Regions to limit the output by. If you use this
-- parameter, the count of returned noncompliant resources includes only
-- resources in the specified Regions.
getComplianceSummary_regionFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe (Core.NonEmpty Core.Text))
getComplianceSummary_regionFilters = Lens.lens (\GetComplianceSummary' {regionFilters} -> regionFilters) (\s@GetComplianceSummary' {} a -> s {regionFilters = a} :: GetComplianceSummary) Core.. Lens.mapping Lens._Coerce

-- | Specifies target identifiers (usually, specific account IDs) to limit
-- the output by. If you use this parameter, the count of returned
-- noncompliant resources includes only resources with the specified target
-- IDs.
getComplianceSummary_targetIdFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe (Core.NonEmpty Core.Text))
getComplianceSummary_targetIdFilters = Lens.lens (\GetComplianceSummary' {targetIdFilters} -> targetIdFilters) (\s@GetComplianceSummary' {} a -> s {targetIdFilters = a} :: GetComplianceSummary) Core.. Lens.mapping Lens._Coerce

-- | Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
getComplianceSummary_paginationToken :: Lens.Lens' GetComplianceSummary (Core.Maybe Core.Text)
getComplianceSummary_paginationToken = Lens.lens (\GetComplianceSummary' {paginationToken} -> paginationToken) (\s@GetComplianceSummary' {} a -> s {paginationToken = a} :: GetComplianceSummary)

-- | Specifies a list of attributes to group the counts of noncompliant
-- resources by. If supplied, the counts are sorted by those attributes.
getComplianceSummary_groupBy :: Lens.Lens' GetComplianceSummary (Core.Maybe [GroupByAttribute])
getComplianceSummary_groupBy = Lens.lens (\GetComplianceSummary' {groupBy} -> groupBy) (\s@GetComplianceSummary' {} a -> s {groupBy = a} :: GetComplianceSummary) Core.. Lens.mapping Lens._Coerce

-- | Specifies that you want the response to include information for only
-- resources of the specified types. The format of each resource type is
-- @service[:resourceType]@. For example, specifying a resource type of
-- @ec2@ returns all Amazon EC2 resources (which includes EC2 instances).
-- Specifying a resource type of @ec2:instance@ returns only EC2 instances.
--
-- The string for each service name and resource type is the same as that
-- embedded in a resource\'s Amazon Resource Name (ARN). Consult the /AWS
-- General Reference/ for the following:
--
-- -   For a list of service name strings, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>.
--
-- -   For resource type strings, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs>.
--
-- -   For more information about ARNs, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- You can specify multiple resource types by using a comma separated
-- array. The array can include up to 100 items. Note that the length
-- constraint requirement applies to each resource type filter.
getComplianceSummary_resourceTypeFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe [Core.Text])
getComplianceSummary_resourceTypeFilters = Lens.lens (\GetComplianceSummary' {resourceTypeFilters} -> resourceTypeFilters) (\s@GetComplianceSummary' {} a -> s {resourceTypeFilters = a} :: GetComplianceSummary) Core.. Lens.mapping Lens._Coerce

-- | Specifies that you want the response to include information for only
-- resources that have tags with the specified tag keys. If you use this
-- parameter, the count of returned noncompliant resources includes only
-- resources that have the specified tag keys.
getComplianceSummary_tagKeyFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe (Core.NonEmpty Core.Text))
getComplianceSummary_tagKeyFilters = Lens.lens (\GetComplianceSummary' {tagKeyFilters} -> tagKeyFilters) (\s@GetComplianceSummary' {} a -> s {tagKeyFilters = a} :: GetComplianceSummary) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager GetComplianceSummary where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getComplianceSummaryResponse_paginationToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getComplianceSummaryResponse_summaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getComplianceSummary_paginationToken
          Lens..~ rs
          Lens.^? getComplianceSummaryResponse_paginationToken
            Core.. Lens._Just

instance Core.AWSRequest GetComplianceSummary where
  type
    AWSResponse GetComplianceSummary =
      GetComplianceSummaryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceSummaryResponse'
            Core.<$> (x Core..?> "PaginationToken")
            Core.<*> (x Core..?> "SummaryList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetComplianceSummary

instance Core.NFData GetComplianceSummary

instance Core.ToHeaders GetComplianceSummary where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ResourceGroupsTaggingAPI_20170126.GetComplianceSummary" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetComplianceSummary where
  toJSON GetComplianceSummary' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("RegionFilters" Core..=) Core.<$> regionFilters,
            ("TargetIdFilters" Core..=) Core.<$> targetIdFilters,
            ("PaginationToken" Core..=) Core.<$> paginationToken,
            ("GroupBy" Core..=) Core.<$> groupBy,
            ("ResourceTypeFilters" Core..=)
              Core.<$> resourceTypeFilters,
            ("TagKeyFilters" Core..=) Core.<$> tagKeyFilters
          ]
      )

instance Core.ToPath GetComplianceSummary where
  toPath = Core.const "/"

instance Core.ToQuery GetComplianceSummary where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetComplianceSummaryResponse' smart constructor.
data GetComplianceSummaryResponse = GetComplianceSummaryResponse'
  { -- | A string that indicates that there is more data available than this
    -- response contains. To receive the next part of the response, specify
    -- this response value as the @PaginationToken@ value in the request for
    -- the next page.
    paginationToken :: Core.Maybe Core.Text,
    -- | A table that shows counts of noncompliant resources.
    summaryList :: Core.Maybe [Summary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'getComplianceSummaryResponse_paginationToken' - A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
--
-- 'summaryList', 'getComplianceSummaryResponse_summaryList' - A table that shows counts of noncompliant resources.
--
-- 'httpStatus', 'getComplianceSummaryResponse_httpStatus' - The response's http status code.
newGetComplianceSummaryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetComplianceSummaryResponse
newGetComplianceSummaryResponse pHttpStatus_ =
  GetComplianceSummaryResponse'
    { paginationToken =
        Core.Nothing,
      summaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
getComplianceSummaryResponse_paginationToken :: Lens.Lens' GetComplianceSummaryResponse (Core.Maybe Core.Text)
getComplianceSummaryResponse_paginationToken = Lens.lens (\GetComplianceSummaryResponse' {paginationToken} -> paginationToken) (\s@GetComplianceSummaryResponse' {} a -> s {paginationToken = a} :: GetComplianceSummaryResponse)

-- | A table that shows counts of noncompliant resources.
getComplianceSummaryResponse_summaryList :: Lens.Lens' GetComplianceSummaryResponse (Core.Maybe [Summary])
getComplianceSummaryResponse_summaryList = Lens.lens (\GetComplianceSummaryResponse' {summaryList} -> summaryList) (\s@GetComplianceSummaryResponse' {} a -> s {summaryList = a} :: GetComplianceSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getComplianceSummaryResponse_httpStatus :: Lens.Lens' GetComplianceSummaryResponse Core.Int
getComplianceSummaryResponse_httpStatus = Lens.lens (\GetComplianceSummaryResponse' {httpStatus} -> httpStatus) (\s@GetComplianceSummaryResponse' {} a -> s {httpStatus = a} :: GetComplianceSummaryResponse)

instance Core.NFData GetComplianceSummaryResponse
