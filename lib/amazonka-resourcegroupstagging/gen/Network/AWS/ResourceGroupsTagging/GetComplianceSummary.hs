{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.GetComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a table that shows counts of resources that are noncompliant with their tag policies.
--
-- For more information on tag policies, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html Tag Policies> in the /AWS Organizations User Guide./
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetComplianceSummary
  ( -- * Creating a request
    GetComplianceSummary (..),
    mkGetComplianceSummary,

    -- ** Request lenses
    gcsGroupBy,
    gcsPaginationToken,
    gcsTargetIdFilters,
    gcsResourceTypeFilters,
    gcsRegionFilters,
    gcsTagKeyFilters,
    gcsMaxResults,

    -- * Destructuring the response
    GetComplianceSummaryResponse (..),
    mkGetComplianceSummaryResponse,

    -- ** Response lenses
    gcsrsPaginationToken,
    gcsrsSummaryList,
    gcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetComplianceSummary' smart constructor.
data GetComplianceSummary = GetComplianceSummary'
  { groupBy ::
      Lude.Maybe [GroupByAttribute],
    paginationToken :: Lude.Maybe Lude.Text,
    targetIdFilters ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    resourceTypeFilters :: Lude.Maybe [Lude.Text],
    regionFilters ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    tagKeyFilters ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceSummary' with the minimum fields required to make a request.
--
-- * 'groupBy' - A list of attributes to group the counts of noncompliant resources by. If supplied, the counts are sorted by those attributes.
-- * 'maxResults' - A limit that restricts the number of results that are returned per page.
-- * 'paginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
-- * 'regionFilters' - A list of Regions to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources in the specified Regions.
-- * 'resourceTypeFilters' - The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances.
--
-- The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:
--
--     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .
--
--
--     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .
--
--
--     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
--
-- You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter.
-- * 'tagKeyFilters' - A list of tag keys to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources that have the specified tag keys.
-- * 'targetIdFilters' - The target identifiers (usually, specific account IDs) to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources with the specified target IDs.
mkGetComplianceSummary ::
  GetComplianceSummary
mkGetComplianceSummary =
  GetComplianceSummary'
    { groupBy = Lude.Nothing,
      paginationToken = Lude.Nothing,
      targetIdFilters = Lude.Nothing,
      resourceTypeFilters = Lude.Nothing,
      regionFilters = Lude.Nothing,
      tagKeyFilters = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A list of attributes to group the counts of noncompliant resources by. If supplied, the counts are sorted by those attributes.
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsGroupBy :: Lens.Lens' GetComplianceSummary (Lude.Maybe [GroupByAttribute])
gcsGroupBy = Lens.lens (groupBy :: GetComplianceSummary -> Lude.Maybe [GroupByAttribute]) (\s a -> s {groupBy = a} :: GetComplianceSummary)
{-# DEPRECATED gcsGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsPaginationToken :: Lens.Lens' GetComplianceSummary (Lude.Maybe Lude.Text)
gcsPaginationToken = Lens.lens (paginationToken :: GetComplianceSummary -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetComplianceSummary)
{-# DEPRECATED gcsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The target identifiers (usually, specific account IDs) to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources with the specified target IDs.
--
-- /Note:/ Consider using 'targetIdFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsTargetIdFilters :: Lens.Lens' GetComplianceSummary (Lude.Maybe (Lude.NonEmpty Lude.Text))
gcsTargetIdFilters = Lens.lens (targetIdFilters :: GetComplianceSummary -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {targetIdFilters = a} :: GetComplianceSummary)
{-# DEPRECATED gcsTargetIdFilters "Use generic-lens or generic-optics with 'targetIdFilters' instead." #-}

-- | The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances.
--
-- The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:
--
--     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .
--
--
--     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .
--
--
--     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
--
-- You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter.
--
-- /Note:/ Consider using 'resourceTypeFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsResourceTypeFilters :: Lens.Lens' GetComplianceSummary (Lude.Maybe [Lude.Text])
gcsResourceTypeFilters = Lens.lens (resourceTypeFilters :: GetComplianceSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypeFilters = a} :: GetComplianceSummary)
{-# DEPRECATED gcsResourceTypeFilters "Use generic-lens or generic-optics with 'resourceTypeFilters' instead." #-}

-- | A list of Regions to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources in the specified Regions.
--
-- /Note:/ Consider using 'regionFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsRegionFilters :: Lens.Lens' GetComplianceSummary (Lude.Maybe (Lude.NonEmpty Lude.Text))
gcsRegionFilters = Lens.lens (regionFilters :: GetComplianceSummary -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {regionFilters = a} :: GetComplianceSummary)
{-# DEPRECATED gcsRegionFilters "Use generic-lens or generic-optics with 'regionFilters' instead." #-}

-- | A list of tag keys to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources that have the specified tag keys.
--
-- /Note:/ Consider using 'tagKeyFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsTagKeyFilters :: Lens.Lens' GetComplianceSummary (Lude.Maybe (Lude.NonEmpty Lude.Text))
gcsTagKeyFilters = Lens.lens (tagKeyFilters :: GetComplianceSummary -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {tagKeyFilters = a} :: GetComplianceSummary)
{-# DEPRECATED gcsTagKeyFilters "Use generic-lens or generic-optics with 'tagKeyFilters' instead." #-}

-- | A limit that restricts the number of results that are returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsMaxResults :: Lens.Lens' GetComplianceSummary (Lude.Maybe Lude.Natural)
gcsMaxResults = Lens.lens (maxResults :: GetComplianceSummary -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetComplianceSummary)
{-# DEPRECATED gcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetComplianceSummary where
  page rq rs
    | Page.stop (rs Lens.^. gcsrsPaginationToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcsrsSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcsPaginationToken Lens..~ rs Lens.^. gcsrsPaginationToken

instance Lude.AWSRequest GetComplianceSummary where
  type Rs GetComplianceSummary = GetComplianceSummaryResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetComplianceSummaryResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "SummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetComplianceSummary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.GetComplianceSummary" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetComplianceSummary where
  toJSON GetComplianceSummary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupBy" Lude..=) Lude.<$> groupBy,
            ("PaginationToken" Lude..=) Lude.<$> paginationToken,
            ("TargetIdFilters" Lude..=) Lude.<$> targetIdFilters,
            ("ResourceTypeFilters" Lude..=) Lude.<$> resourceTypeFilters,
            ("RegionFilters" Lude..=) Lude.<$> regionFilters,
            ("TagKeyFilters" Lude..=) Lude.<$> tagKeyFilters,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetComplianceSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetComplianceSummary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetComplianceSummaryResponse' smart constructor.
data GetComplianceSummaryResponse = GetComplianceSummaryResponse'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    summaryList ::
      Lude.Maybe [Summary],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceSummaryResponse' with the minimum fields required to make a request.
--
-- * 'paginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
-- * 'responseStatus' - The response status code.
-- * 'summaryList' - A table that shows counts of noncompliant resources.
mkGetComplianceSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetComplianceSummaryResponse
mkGetComplianceSummaryResponse pResponseStatus_ =
  GetComplianceSummaryResponse'
    { paginationToken = Lude.Nothing,
      summaryList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsPaginationToken :: Lens.Lens' GetComplianceSummaryResponse (Lude.Maybe Lude.Text)
gcsrsPaginationToken = Lens.lens (paginationToken :: GetComplianceSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetComplianceSummaryResponse)
{-# DEPRECATED gcsrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | A table that shows counts of noncompliant resources.
--
-- /Note:/ Consider using 'summaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsSummaryList :: Lens.Lens' GetComplianceSummaryResponse (Lude.Maybe [Summary])
gcsrsSummaryList = Lens.lens (summaryList :: GetComplianceSummaryResponse -> Lude.Maybe [Summary]) (\s a -> s {summaryList = a} :: GetComplianceSummaryResponse)
{-# DEPRECATED gcsrsSummaryList "Use generic-lens or generic-optics with 'summaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsResponseStatus :: Lens.Lens' GetComplianceSummaryResponse Lude.Int
gcsrsResponseStatus = Lens.lens (responseStatus :: GetComplianceSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetComplianceSummaryResponse)
{-# DEPRECATED gcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
