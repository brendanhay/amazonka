{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of application versions.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
  ( -- * Creating a request
    DescribeApplicationVersions (..),
    mkDescribeApplicationVersions,

    -- ** Request lenses
    dVersionLabels,
    dNextToken,
    dMaxRecords,
    dApplicationName,

    -- * Destructuring the response
    DescribeApplicationVersionsResponse (..),
    mkDescribeApplicationVersionsResponse,

    -- ** Response lenses
    davrsApplicationVersions,
    davrsNextToken,
    davrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to describe application versions.
--
-- /See:/ 'mkDescribeApplicationVersions' smart constructor.
data DescribeApplicationVersions = DescribeApplicationVersions'
  { -- | Specify a version label to show a specific application version.
    versionLabels :: Lude.Maybe [Lude.Text],
    -- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Lude.Maybe Lude.Text,
    -- | For a paginated request. Specify a maximum number of application versions to include in each response.
    --
    -- If no @MaxRecords@ is specified, all available application versions are retrieved in a single response.
    maxRecords :: Lude.Maybe Lude.Natural,
    -- | Specify an application name to show only application versions for that application.
    applicationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplicationVersions' with the minimum fields required to make a request.
--
-- * 'versionLabels' - Specify a version label to show a specific application version.
-- * 'nextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
-- * 'maxRecords' - For a paginated request. Specify a maximum number of application versions to include in each response.
--
-- If no @MaxRecords@ is specified, all available application versions are retrieved in a single response.
-- * 'applicationName' - Specify an application name to show only application versions for that application.
mkDescribeApplicationVersions ::
  DescribeApplicationVersions
mkDescribeApplicationVersions =
  DescribeApplicationVersions'
    { versionLabels = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing,
      applicationName = Lude.Nothing
    }

-- | Specify a version label to show a specific application version.
--
-- /Note:/ Consider using 'versionLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionLabels :: Lens.Lens' DescribeApplicationVersions (Lude.Maybe [Lude.Text])
dVersionLabels = Lens.lens (versionLabels :: DescribeApplicationVersions -> Lude.Maybe [Lude.Text]) (\s a -> s {versionLabels = a} :: DescribeApplicationVersions)
{-# DEPRECATED dVersionLabels "Use generic-lens or generic-optics with 'versionLabels' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeApplicationVersions (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeApplicationVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeApplicationVersions)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | For a paginated request. Specify a maximum number of application versions to include in each response.
--
-- If no @MaxRecords@ is specified, all available application versions are retrieved in a single response.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeApplicationVersions (Lude.Maybe Lude.Natural)
dMaxRecords = Lens.lens (maxRecords :: DescribeApplicationVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeApplicationVersions)
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | Specify an application name to show only application versions for that application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationName :: Lens.Lens' DescribeApplicationVersions (Lude.Maybe Lude.Text)
dApplicationName = Lens.lens (applicationName :: DescribeApplicationVersions -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DescribeApplicationVersions)
{-# DEPRECATED dApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Page.AWSPager DescribeApplicationVersions where
  page rq rs
    | Page.stop (rs Lens.^. davrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. davrsApplicationVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. davrsNextToken

instance Lude.AWSRequest DescribeApplicationVersions where
  type
    Rs DescribeApplicationVersions =
      DescribeApplicationVersionsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeApplicationVersionsResult"
      ( \s h x ->
          DescribeApplicationVersionsResponse'
            Lude.<$> ( x Lude..@? "ApplicationVersions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeApplicationVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeApplicationVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeApplicationVersions where
  toQuery DescribeApplicationVersions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeApplicationVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "VersionLabels"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> versionLabels),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords,
        "ApplicationName" Lude.=: applicationName
      ]

-- | Result message wrapping a list of application version descriptions.
--
-- /See:/ 'mkDescribeApplicationVersionsResponse' smart constructor.
data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse'
  { -- | List of @ApplicationVersionDescription@ objects sorted in order of creation.
    applicationVersions :: Lude.Maybe [ApplicationVersionDescription],
    -- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplicationVersionsResponse' with the minimum fields required to make a request.
--
-- * 'applicationVersions' - List of @ApplicationVersionDescription@ objects sorted in order of creation.
-- * 'nextToken' - In a paginated request, the token that you can pass in a subsequent request to get the next response page.
-- * 'responseStatus' - The response status code.
mkDescribeApplicationVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeApplicationVersionsResponse
mkDescribeApplicationVersionsResponse pResponseStatus_ =
  DescribeApplicationVersionsResponse'
    { applicationVersions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of @ApplicationVersionDescription@ objects sorted in order of creation.
--
-- /Note:/ Consider using 'applicationVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrsApplicationVersions :: Lens.Lens' DescribeApplicationVersionsResponse (Lude.Maybe [ApplicationVersionDescription])
davrsApplicationVersions = Lens.lens (applicationVersions :: DescribeApplicationVersionsResponse -> Lude.Maybe [ApplicationVersionDescription]) (\s a -> s {applicationVersions = a} :: DescribeApplicationVersionsResponse)
{-# DEPRECATED davrsApplicationVersions "Use generic-lens or generic-optics with 'applicationVersions' instead." #-}

-- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrsNextToken :: Lens.Lens' DescribeApplicationVersionsResponse (Lude.Maybe Lude.Text)
davrsNextToken = Lens.lens (nextToken :: DescribeApplicationVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeApplicationVersionsResponse)
{-# DEPRECATED davrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrsResponseStatus :: Lens.Lens' DescribeApplicationVersionsResponse Lude.Int
davrsResponseStatus = Lens.lens (responseStatus :: DescribeApplicationVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeApplicationVersionsResponse)
{-# DEPRECATED davrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
