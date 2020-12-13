{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves build resources for all builds associated with the AWS account in use. You can limit results to builds that are in a specific status by using the @Status@ parameter. Use the pagination parameters to retrieve results in a set of sequential pages.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
-- __Related operations__
--
--     * 'CreateBuild'
--
--
--     * 'ListBuilds'
--
--
--     * 'DescribeBuild'
--
--
--     * 'UpdateBuild'
--
--
--     * 'DeleteBuild'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListBuilds
  ( -- * Creating a request
    ListBuilds (..),
    mkListBuilds,

    -- ** Request lenses
    lbStatus,
    lbNextToken,
    lbLimit,

    -- * Destructuring the response
    ListBuildsResponse (..),
    mkListBuildsResponse,

    -- ** Response lenses
    lbrsBuilds,
    lbrsNextToken,
    lbrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { -- | Build status to filter results by. To retrieve all builds, leave this parameter empty.
    --
    -- Possible build statuses include the following:
    --
    --     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.
    --
    --
    --     * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.
    --
    --
    --     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
    status :: Lude.Maybe BuildStatus,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBuilds' with the minimum fields required to make a request.
--
-- * 'status' - Build status to filter results by. To retrieve all builds, leave this parameter empty.
--
-- Possible build statuses include the following:
--
--     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.
--
--
--     * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.
--
--
--     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
--
--
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
mkListBuilds ::
  ListBuilds
mkListBuilds =
  ListBuilds'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Build status to filter results by. To retrieve all builds, leave this parameter empty.
--
-- Possible build statuses include the following:
--
--     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.
--
--
--     * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.
--
--
--     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbStatus :: Lens.Lens' ListBuilds (Lude.Maybe BuildStatus)
lbStatus = Lens.lens (status :: ListBuilds -> Lude.Maybe BuildStatus) (\s a -> s {status = a} :: ListBuilds)
{-# DEPRECATED lbStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBuilds (Lude.Maybe Lude.Text)
lbNextToken = Lens.lens (nextToken :: ListBuilds -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuilds)
{-# DEPRECATED lbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLimit :: Lens.Lens' ListBuilds (Lude.Maybe Lude.Natural)
lbLimit = Lens.lens (limit :: ListBuilds -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListBuilds)
{-# DEPRECATED lbLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListBuilds where
  page rq rs
    | Page.stop (rs Lens.^. lbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbrsBuilds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbNextToken Lens..~ rs Lens.^. lbrsNextToken

instance Lude.AWSRequest ListBuilds where
  type Rs ListBuilds = ListBuildsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBuildsResponse'
            Lude.<$> (x Lude..?> "Builds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBuilds where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("GameLift.ListBuilds" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBuilds where
  toJSON ListBuilds' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListBuilds where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBuilds where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { -- | A collection of build resources that match the request.
    builds :: Lude.Maybe [Build],
    -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBuildsResponse' with the minimum fields required to make a request.
--
-- * 'builds' - A collection of build resources that match the request.
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkListBuildsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBuildsResponse
mkListBuildsResponse pResponseStatus_ =
  ListBuildsResponse'
    { builds = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of build resources that match the request.
--
-- /Note:/ Consider using 'builds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsBuilds :: Lens.Lens' ListBuildsResponse (Lude.Maybe [Build])
lbrsBuilds = Lens.lens (builds :: ListBuildsResponse -> Lude.Maybe [Build]) (\s a -> s {builds = a} :: ListBuildsResponse)
{-# DEPRECATED lbrsBuilds "Use generic-lens or generic-optics with 'builds' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBuildsResponse (Lude.Maybe Lude.Text)
lbrsNextToken = Lens.lens (nextToken :: ListBuildsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildsResponse)
{-# DEPRECATED lbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsResponseStatus :: Lens.Lens' ListBuildsResponse Lude.Int
lbrsResponseStatus = Lens.lens (responseStatus :: ListBuildsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBuildsResponse)
{-# DEPRECATED lbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
