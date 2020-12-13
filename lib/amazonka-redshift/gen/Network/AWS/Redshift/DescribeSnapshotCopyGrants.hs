{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeSnapshotCopyGrants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot copy grants owned by the AWS account in the destination region.
--
-- For more information about managing snapshot copy grants, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeSnapshotCopyGrants
  ( -- * Creating a request
    DescribeSnapshotCopyGrants (..),
    mkDescribeSnapshotCopyGrants,

    -- ** Request lenses
    dscgTagValues,
    dscgTagKeys,
    dscgMarker,
    dscgMaxRecords,
    dscgSnapshotCopyGrantName,

    -- * Destructuring the response
    DescribeSnapshotCopyGrantsResponse (..),
    mkDescribeSnapshotCopyGrantsResponse,

    -- ** Response lenses
    dscgrsSnapshotCopyGrants,
    dscgrsMarker,
    dscgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The result of the @DescribeSnapshotCopyGrants@ action.
--
-- /See:/ 'mkDescribeSnapshotCopyGrants' smart constructor.
data DescribeSnapshotCopyGrants = DescribeSnapshotCopyGrants'
  { -- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
    tagValues :: Lude.Maybe [Lude.Text],
    -- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotCopyGrants' with the minimum fields required to make a request.
--
-- * 'tagValues' - A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'snapshotCopyGrantName' - The name of the snapshot copy grant.
mkDescribeSnapshotCopyGrants ::
  DescribeSnapshotCopyGrants
mkDescribeSnapshotCopyGrants =
  DescribeSnapshotCopyGrants'
    { tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      snapshotCopyGrantName = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching resources that are associated with the specified value or values. For example, suppose that you have resources tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with all resources that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgTagValues :: Lens.Lens' DescribeSnapshotCopyGrants (Lude.Maybe [Lude.Text])
dscgTagValues = Lens.lens (tagValues :: DescribeSnapshotCopyGrants -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeSnapshotCopyGrants)
{-# DEPRECATED dscgTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A tag key or keys for which you want to return all matching resources that are associated with the specified key or keys. For example, suppose that you have resources tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with all resources that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgTagKeys :: Lens.Lens' DescribeSnapshotCopyGrants (Lude.Maybe [Lude.Text])
dscgTagKeys = Lens.lens (tagKeys :: DescribeSnapshotCopyGrants -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeSnapshotCopyGrants)
{-# DEPRECATED dscgTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgMarker :: Lens.Lens' DescribeSnapshotCopyGrants (Lude.Maybe Lude.Text)
dscgMarker = Lens.lens (marker :: DescribeSnapshotCopyGrants -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSnapshotCopyGrants)
{-# DEPRECATED dscgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgMaxRecords :: Lens.Lens' DescribeSnapshotCopyGrants (Lude.Maybe Lude.Int)
dscgMaxRecords = Lens.lens (maxRecords :: DescribeSnapshotCopyGrants -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeSnapshotCopyGrants)
{-# DEPRECATED dscgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgSnapshotCopyGrantName :: Lens.Lens' DescribeSnapshotCopyGrants (Lude.Maybe Lude.Text)
dscgSnapshotCopyGrantName = Lens.lens (snapshotCopyGrantName :: DescribeSnapshotCopyGrants -> Lude.Maybe Lude.Text) (\s a -> s {snapshotCopyGrantName = a} :: DescribeSnapshotCopyGrants)
{-# DEPRECATED dscgSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Page.AWSPager DescribeSnapshotCopyGrants where
  page rq rs
    | Page.stop (rs Lens.^. dscgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dscgrsSnapshotCopyGrants) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dscgMarker Lens..~ rs Lens.^. dscgrsMarker

instance Lude.AWSRequest DescribeSnapshotCopyGrants where
  type
    Rs DescribeSnapshotCopyGrants =
      DescribeSnapshotCopyGrantsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeSnapshotCopyGrantsResult"
      ( \s h x ->
          DescribeSnapshotCopyGrantsResponse'
            Lude.<$> ( x Lude..@? "SnapshotCopyGrants" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "SnapshotCopyGrant")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSnapshotCopyGrants where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSnapshotCopyGrants where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSnapshotCopyGrants where
  toQuery DescribeSnapshotCopyGrants' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeSnapshotCopyGrants" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "SnapshotCopyGrantName" Lude.=: snapshotCopyGrantName
      ]

-- |
--
-- /See:/ 'mkDescribeSnapshotCopyGrantsResponse' smart constructor.
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
  { -- | The list of @SnapshotCopyGrant@ objects.
    snapshotCopyGrants :: Lude.Maybe [SnapshotCopyGrant],
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotCopyGrantsResponse' with the minimum fields required to make a request.
--
-- * 'snapshotCopyGrants' - The list of @SnapshotCopyGrant@ objects.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
-- * 'responseStatus' - The response status code.
mkDescribeSnapshotCopyGrantsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSnapshotCopyGrantsResponse
mkDescribeSnapshotCopyGrantsResponse pResponseStatus_ =
  DescribeSnapshotCopyGrantsResponse'
    { snapshotCopyGrants =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of @SnapshotCopyGrant@ objects.
--
-- /Note:/ Consider using 'snapshotCopyGrants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrsSnapshotCopyGrants :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Lude.Maybe [SnapshotCopyGrant])
dscgrsSnapshotCopyGrants = Lens.lens (snapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> Lude.Maybe [SnapshotCopyGrant]) (\s a -> s {snapshotCopyGrants = a} :: DescribeSnapshotCopyGrantsResponse)
{-# DEPRECATED dscgrsSnapshotCopyGrants "Use generic-lens or generic-optics with 'snapshotCopyGrants' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeSnapshotCopyGrant@ request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__ parameter or the __Marker__ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrsMarker :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Lude.Maybe Lude.Text)
dscgrsMarker = Lens.lens (marker :: DescribeSnapshotCopyGrantsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSnapshotCopyGrantsResponse)
{-# DEPRECATED dscgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscgrsResponseStatus :: Lens.Lens' DescribeSnapshotCopyGrantsResponse Lude.Int
dscgrsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotCopyGrantsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotCopyGrantsResponse)
{-# DEPRECATED dscgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
