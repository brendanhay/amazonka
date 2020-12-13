{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterDBRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterDbRevision@ objects.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterDBRevisions
  ( -- * Creating a request
    DescribeClusterDBRevisions (..),
    mkDescribeClusterDBRevisions,

    -- ** Request lenses
    dcdrClusterIdentifier,
    dcdrMarker,
    dcdrMaxRecords,

    -- * Destructuring the response
    DescribeClusterDBRevisionsResponse (..),
    mkDescribeClusterDBRevisionsResponse,

    -- ** Response lenses
    dcdrrsClusterDBRevisions,
    dcdrrsMarker,
    dcdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClusterDBRevisions' smart constructor.
data DescribeClusterDBRevisions = DescribeClusterDBRevisions'
  { -- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are requesting. This parameter is case sensitive. All clusters defined for an account are returned by default.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | An optional parameter that specifies the starting point for returning a set of response records. When the results of a @DescribeClusterDbRevisions@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the @ClusterIdentifier@ parameter, or the @marker@ parameter, but not both.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified MaxRecords value, a value is returned in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.
    --
    -- Default: 100
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterDBRevisions' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - A unique identifier for a cluster whose @ClusterDbRevisions@ you are requesting. This parameter is case sensitive. All clusters defined for an account are returned by default.
-- * 'marker' - An optional parameter that specifies the starting point for returning a set of response records. When the results of a @DescribeClusterDbRevisions@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the @ClusterIdentifier@ parameter, or the @marker@ parameter, but not both.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified MaxRecords value, a value is returned in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.
--
-- Default: 100
-- Constraints: minimum 20, maximum 100.
mkDescribeClusterDBRevisions ::
  DescribeClusterDBRevisions
mkDescribeClusterDBRevisions =
  DescribeClusterDBRevisions'
    { clusterIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are requesting. This parameter is case sensitive. All clusters defined for an account are returned by default.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrClusterIdentifier :: Lens.Lens' DescribeClusterDBRevisions (Lude.Maybe Lude.Text)
dcdrClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeClusterDBRevisions -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeClusterDBRevisions)
{-# DEPRECATED dcdrClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | An optional parameter that specifies the starting point for returning a set of response records. When the results of a @DescribeClusterDbRevisions@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the @ClusterIdentifier@ parameter, or the @marker@ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrMarker :: Lens.Lens' DescribeClusterDBRevisions (Lude.Maybe Lude.Text)
dcdrMarker = Lens.lens (marker :: DescribeClusterDBRevisions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterDBRevisions)
{-# DEPRECATED dcdrMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified MaxRecords value, a value is returned in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.
--
-- Default: 100
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrMaxRecords :: Lens.Lens' DescribeClusterDBRevisions (Lude.Maybe Lude.Int)
dcdrMaxRecords = Lens.lens (maxRecords :: DescribeClusterDBRevisions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterDBRevisions)
{-# DEPRECATED dcdrMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeClusterDBRevisions where
  page rq rs
    | Page.stop (rs Lens.^. dcdrrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcdrrsClusterDBRevisions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcdrMarker Lens..~ rs Lens.^. dcdrrsMarker

instance Lude.AWSRequest DescribeClusterDBRevisions where
  type
    Rs DescribeClusterDBRevisions =
      DescribeClusterDBRevisionsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterDbRevisionsResult"
      ( \s h x ->
          DescribeClusterDBRevisionsResponse'
            Lude.<$> ( x Lude..@? "ClusterDbRevisions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ClusterDbRevision")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterDBRevisions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterDBRevisions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterDBRevisions where
  toQuery DescribeClusterDBRevisions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClusterDbRevisions" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeClusterDBRevisionsResponse' smart constructor.
data DescribeClusterDBRevisionsResponse = DescribeClusterDBRevisionsResponse'
  { -- | A list of revisions.
    clusterDBRevisions :: Lude.Maybe [ClusterDBRevision],
    -- | A string representing the starting point for the next set of revisions. If a value is returned in a response, you can retrieve the next set of revisions by providing the value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all revisions have already been returned.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterDBRevisionsResponse' with the minimum fields required to make a request.
--
-- * 'clusterDBRevisions' - A list of revisions.
-- * 'marker' - A string representing the starting point for the next set of revisions. If a value is returned in a response, you can retrieve the next set of revisions by providing the value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all revisions have already been returned.
-- * 'responseStatus' - The response status code.
mkDescribeClusterDBRevisionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterDBRevisionsResponse
mkDescribeClusterDBRevisionsResponse pResponseStatus_ =
  DescribeClusterDBRevisionsResponse'
    { clusterDBRevisions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of revisions.
--
-- /Note:/ Consider using 'clusterDBRevisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrrsClusterDBRevisions :: Lens.Lens' DescribeClusterDBRevisionsResponse (Lude.Maybe [ClusterDBRevision])
dcdrrsClusterDBRevisions = Lens.lens (clusterDBRevisions :: DescribeClusterDBRevisionsResponse -> Lude.Maybe [ClusterDBRevision]) (\s a -> s {clusterDBRevisions = a} :: DescribeClusterDBRevisionsResponse)
{-# DEPRECATED dcdrrsClusterDBRevisions "Use generic-lens or generic-optics with 'clusterDBRevisions' instead." #-}

-- | A string representing the starting point for the next set of revisions. If a value is returned in a response, you can retrieve the next set of revisions by providing the value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all revisions have already been returned.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrrsMarker :: Lens.Lens' DescribeClusterDBRevisionsResponse (Lude.Maybe Lude.Text)
dcdrrsMarker = Lens.lens (marker :: DescribeClusterDBRevisionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterDBRevisionsResponse)
{-# DEPRECATED dcdrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrrsResponseStatus :: Lens.Lens' DescribeClusterDBRevisionsResponse Lude.Int
dcdrrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterDBRevisionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterDBRevisionsResponse)
{-# DEPRECATED dcdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
