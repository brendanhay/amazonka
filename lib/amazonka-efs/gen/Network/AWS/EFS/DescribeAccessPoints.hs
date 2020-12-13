{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeAccessPoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS access point if the @AccessPointId@ is provided. If you provide an EFS @FileSystemId@ , it returns descriptions of all access points for that file system. You can provide either an @AccessPointId@ or a @FileSystemId@ in the request, but not both.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeAccessPoints@ action.
module Network.AWS.EFS.DescribeAccessPoints
  ( -- * Creating a request
    DescribeAccessPoints (..),
    mkDescribeAccessPoints,

    -- ** Request lenses
    dapAccessPointId,
    dapFileSystemId,
    dapNextToken,
    dapMaxResults,

    -- * Destructuring the response
    DescribeAccessPointsResponse (..),
    mkDescribeAccessPointsResponse,

    -- ** Response lenses
    daprsAccessPoints,
    daprsNextToken,
    daprsResponseStatus,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccessPoints' smart constructor.
data DescribeAccessPoints = DescribeAccessPoints'
  { -- | (Optional) Specifies an EFS access point to describe in the response; mutually exclusive with @FileSystemId@ .
    accessPointId :: Lude.Maybe Lude.Text,
    -- | (Optional) If you provide a @FileSystemId@ , EFS returns all access points for that file system; mutually exclusive with @AccessPointId@ .
    fileSystemId :: Lude.Maybe Lude.Text,
    -- | @NextToken@ is present if the response is paginated. You can use @NextMarker@ in the subsequent request to fetch the next page of access point descriptions.
    nextToken :: Lude.Maybe Lude.Text,
    -- | (Optional) When retrieving all access points for a file system, you can optionally specify the @MaxItems@ parameter to limit the number of objects returned in a response. The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccessPoints' with the minimum fields required to make a request.
--
-- * 'accessPointId' - (Optional) Specifies an EFS access point to describe in the response; mutually exclusive with @FileSystemId@ .
-- * 'fileSystemId' - (Optional) If you provide a @FileSystemId@ , EFS returns all access points for that file system; mutually exclusive with @AccessPointId@ .
-- * 'nextToken' - @NextToken@ is present if the response is paginated. You can use @NextMarker@ in the subsequent request to fetch the next page of access point descriptions.
-- * 'maxResults' - (Optional) When retrieving all access points for a file system, you can optionally specify the @MaxItems@ parameter to limit the number of objects returned in a response. The default value is 100.
mkDescribeAccessPoints ::
  DescribeAccessPoints
mkDescribeAccessPoints =
  DescribeAccessPoints'
    { accessPointId = Lude.Nothing,
      fileSystemId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | (Optional) Specifies an EFS access point to describe in the response; mutually exclusive with @FileSystemId@ .
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapAccessPointId :: Lens.Lens' DescribeAccessPoints (Lude.Maybe Lude.Text)
dapAccessPointId = Lens.lens (accessPointId :: DescribeAccessPoints -> Lude.Maybe Lude.Text) (\s a -> s {accessPointId = a} :: DescribeAccessPoints)
{-# DEPRECATED dapAccessPointId "Use generic-lens or generic-optics with 'accessPointId' instead." #-}

-- | (Optional) If you provide a @FileSystemId@ , EFS returns all access points for that file system; mutually exclusive with @AccessPointId@ .
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapFileSystemId :: Lens.Lens' DescribeAccessPoints (Lude.Maybe Lude.Text)
dapFileSystemId = Lens.lens (fileSystemId :: DescribeAccessPoints -> Lude.Maybe Lude.Text) (\s a -> s {fileSystemId = a} :: DescribeAccessPoints)
{-# DEPRECATED dapFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | @NextToken@ is present if the response is paginated. You can use @NextMarker@ in the subsequent request to fetch the next page of access point descriptions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapNextToken :: Lens.Lens' DescribeAccessPoints (Lude.Maybe Lude.Text)
dapNextToken = Lens.lens (nextToken :: DescribeAccessPoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAccessPoints)
{-# DEPRECATED dapNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) When retrieving all access points for a file system, you can optionally specify the @MaxItems@ parameter to limit the number of objects returned in a response. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapMaxResults :: Lens.Lens' DescribeAccessPoints (Lude.Maybe Lude.Natural)
dapMaxResults = Lens.lens (maxResults :: DescribeAccessPoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAccessPoints)
{-# DEPRECATED dapMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeAccessPoints where
  type Rs DescribeAccessPoints = DescribeAccessPointsResponse
  request = Req.get efsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAccessPointsResponse'
            Lude.<$> (x Lude..?> "AccessPoints" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccessPoints where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccessPoints where
  toPath = Lude.const "/2015-02-01/access-points"

instance Lude.ToQuery DescribeAccessPoints where
  toQuery DescribeAccessPoints' {..} =
    Lude.mconcat
      [ "AccessPointId" Lude.=: accessPointId,
        "FileSystemId" Lude.=: fileSystemId,
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeAccessPointsResponse' smart constructor.
data DescribeAccessPointsResponse = DescribeAccessPointsResponse'
  { -- | An array of access point descriptions.
    accessPoints :: Lude.Maybe [AccessPointDescription],
    -- | Present if there are more access points than returned in the response. You can use the NextMarker in the subsequent request to fetch the additional descriptions.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccessPointsResponse' with the minimum fields required to make a request.
--
-- * 'accessPoints' - An array of access point descriptions.
-- * 'nextToken' - Present if there are more access points than returned in the response. You can use the NextMarker in the subsequent request to fetch the additional descriptions.
-- * 'responseStatus' - The response status code.
mkDescribeAccessPointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccessPointsResponse
mkDescribeAccessPointsResponse pResponseStatus_ =
  DescribeAccessPointsResponse'
    { accessPoints = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of access point descriptions.
--
-- /Note:/ Consider using 'accessPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprsAccessPoints :: Lens.Lens' DescribeAccessPointsResponse (Lude.Maybe [AccessPointDescription])
daprsAccessPoints = Lens.lens (accessPoints :: DescribeAccessPointsResponse -> Lude.Maybe [AccessPointDescription]) (\s a -> s {accessPoints = a} :: DescribeAccessPointsResponse)
{-# DEPRECATED daprsAccessPoints "Use generic-lens or generic-optics with 'accessPoints' instead." #-}

-- | Present if there are more access points than returned in the response. You can use the NextMarker in the subsequent request to fetch the additional descriptions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprsNextToken :: Lens.Lens' DescribeAccessPointsResponse (Lude.Maybe Lude.Text)
daprsNextToken = Lens.lens (nextToken :: DescribeAccessPointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAccessPointsResponse)
{-# DEPRECATED daprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprsResponseStatus :: Lens.Lens' DescribeAccessPointsResponse Lude.Int
daprsResponseStatus = Lens.lens (responseStatus :: DescribeAccessPointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccessPointsResponse)
{-# DEPRECATED daprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
