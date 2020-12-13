{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListCreatedArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the created artifacts attached to a given migration task in an update stream. This API has the following traits:
--
--
--     * Gets the list of the created artifacts while migration is taking place.
--
--
--     * Shows the artifacts created by the migration tool that was associated by the @AssociateCreatedArtifact@ API.
--
--
--     * Lists created artifacts in a paginated interface.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListCreatedArtifacts
  ( -- * Creating a request
    ListCreatedArtifacts (..),
    mkListCreatedArtifacts,

    -- ** Request lenses
    lcaNextToken,
    lcaProgressUpdateStream,
    lcaMigrationTaskName,
    lcaMaxResults,

    -- * Destructuring the response
    ListCreatedArtifactsResponse (..),
    mkListCreatedArtifactsResponse,

    -- ** Response lenses
    lcarsNextToken,
    lcarsCreatedArtifactList,
    lcarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCreatedArtifacts' smart constructor.
data ListCreatedArtifacts = ListCreatedArtifacts'
  { -- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Lude.Text,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Lude.Text,
    -- | Maximum number of results to be returned per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCreatedArtifacts' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
-- * 'migrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
-- * 'maxResults' - Maximum number of results to be returned per page.
mkListCreatedArtifacts ::
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  ListCreatedArtifacts
mkListCreatedArtifacts pProgressUpdateStream_ pMigrationTaskName_ =
  ListCreatedArtifacts'
    { nextToken = Lude.Nothing,
      progressUpdateStream = pProgressUpdateStream_,
      migrationTaskName = pMigrationTaskName_,
      maxResults = Lude.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaNextToken :: Lens.Lens' ListCreatedArtifacts (Lude.Maybe Lude.Text)
lcaNextToken = Lens.lens (nextToken :: ListCreatedArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCreatedArtifacts)
{-# DEPRECATED lcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaProgressUpdateStream :: Lens.Lens' ListCreatedArtifacts Lude.Text
lcaProgressUpdateStream = Lens.lens (progressUpdateStream :: ListCreatedArtifacts -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: ListCreatedArtifacts)
{-# DEPRECATED lcaProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaMigrationTaskName :: Lens.Lens' ListCreatedArtifacts Lude.Text
lcaMigrationTaskName = Lens.lens (migrationTaskName :: ListCreatedArtifacts -> Lude.Text) (\s a -> s {migrationTaskName = a} :: ListCreatedArtifacts)
{-# DEPRECATED lcaMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Maximum number of results to be returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaMaxResults :: Lens.Lens' ListCreatedArtifacts (Lude.Maybe Lude.Natural)
lcaMaxResults = Lens.lens (maxResults :: ListCreatedArtifacts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCreatedArtifacts)
{-# DEPRECATED lcaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCreatedArtifacts where
  page rq rs
    | Page.stop (rs Lens.^. lcarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcarsCreatedArtifactList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcaNextToken Lens..~ rs Lens.^. lcarsNextToken

instance Lude.AWSRequest ListCreatedArtifacts where
  type Rs ListCreatedArtifacts = ListCreatedArtifactsResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCreatedArtifactsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CreatedArtifactList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCreatedArtifacts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.ListCreatedArtifacts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCreatedArtifacts where
  toJSON ListCreatedArtifacts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCreatedArtifacts where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCreatedArtifacts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCreatedArtifactsResponse' smart constructor.
data ListCreatedArtifactsResponse = ListCreatedArtifactsResponse'
  { -- | If there are more created artifacts than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of created artifacts up to the maximum number of results specified in the request.
    createdArtifactList :: Lude.Maybe [CreatedArtifact],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCreatedArtifactsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are more created artifacts than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
-- * 'createdArtifactList' - List of created artifacts up to the maximum number of results specified in the request.
-- * 'responseStatus' - The response status code.
mkListCreatedArtifactsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCreatedArtifactsResponse
mkListCreatedArtifactsResponse pResponseStatus_ =
  ListCreatedArtifactsResponse'
    { nextToken = Lude.Nothing,
      createdArtifactList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are more created artifacts than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarsNextToken :: Lens.Lens' ListCreatedArtifactsResponse (Lude.Maybe Lude.Text)
lcarsNextToken = Lens.lens (nextToken :: ListCreatedArtifactsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCreatedArtifactsResponse)
{-# DEPRECATED lcarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of created artifacts up to the maximum number of results specified in the request.
--
-- /Note:/ Consider using 'createdArtifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarsCreatedArtifactList :: Lens.Lens' ListCreatedArtifactsResponse (Lude.Maybe [CreatedArtifact])
lcarsCreatedArtifactList = Lens.lens (createdArtifactList :: ListCreatedArtifactsResponse -> Lude.Maybe [CreatedArtifact]) (\s a -> s {createdArtifactList = a} :: ListCreatedArtifactsResponse)
{-# DEPRECATED lcarsCreatedArtifactList "Use generic-lens or generic-optics with 'createdArtifactList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarsResponseStatus :: Lens.Lens' ListCreatedArtifactsResponse Lude.Int
lcarsResponseStatus = Lens.lens (responseStatus :: ListCreatedArtifactsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCreatedArtifactsResponse)
{-# DEPRECATED lcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
