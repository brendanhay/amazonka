{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of applications nested in the containing application.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
  ( -- * Creating a request
    ListApplicationDependencies (..),
    mkListApplicationDependencies,

    -- ** Request lenses
    ladSemanticVersion,
    ladNextToken,
    ladApplicationId,
    ladMaxItems,

    -- * Destructuring the response
    ListApplicationDependenciesResponse (..),
    mkListApplicationDependenciesResponse,

    -- ** Response lenses
    ladrsDependencies,
    ladrsNextToken,
    ladrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkListApplicationDependencies' smart constructor.
data ListApplicationDependencies = ListApplicationDependencies'
  { -- | The semantic version of the application to get.
    semanticVersion :: Lude.Maybe Lude.Text,
    -- | A token to specify where to start paginating.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text,
    -- | The total number of items to return.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplicationDependencies' with the minimum fields required to make a request.
--
-- * 'semanticVersion' - The semantic version of the application to get.
-- * 'nextToken' - A token to specify where to start paginating.
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'maxItems' - The total number of items to return.
mkListApplicationDependencies ::
  -- | 'applicationId'
  Lude.Text ->
  ListApplicationDependencies
mkListApplicationDependencies pApplicationId_ =
  ListApplicationDependencies'
    { semanticVersion = Lude.Nothing,
      nextToken = Lude.Nothing,
      applicationId = pApplicationId_,
      maxItems = Lude.Nothing
    }

-- | The semantic version of the application to get.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladSemanticVersion :: Lens.Lens' ListApplicationDependencies (Lude.Maybe Lude.Text)
ladSemanticVersion = Lens.lens (semanticVersion :: ListApplicationDependencies -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: ListApplicationDependencies)
{-# DEPRECATED ladSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | A token to specify where to start paginating.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladNextToken :: Lens.Lens' ListApplicationDependencies (Lude.Maybe Lude.Text)
ladNextToken = Lens.lens (nextToken :: ListApplicationDependencies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationDependencies)
{-# DEPRECATED ladNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladApplicationId :: Lens.Lens' ListApplicationDependencies Lude.Text
ladApplicationId = Lens.lens (applicationId :: ListApplicationDependencies -> Lude.Text) (\s a -> s {applicationId = a} :: ListApplicationDependencies)
{-# DEPRECATED ladApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The total number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladMaxItems :: Lens.Lens' ListApplicationDependencies (Lude.Maybe Lude.Natural)
ladMaxItems = Lens.lens (maxItems :: ListApplicationDependencies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListApplicationDependencies)
{-# DEPRECATED ladMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListApplicationDependencies where
  page rq rs
    | Page.stop (rs Lens.^. ladrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ladrsDependencies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ladNextToken Lens..~ rs Lens.^. ladrsNextToken

instance Lude.AWSRequest ListApplicationDependencies where
  type
    Rs ListApplicationDependencies =
      ListApplicationDependenciesResponse
  request = Req.get serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApplicationDependenciesResponse'
            Lude.<$> (x Lude..?> "dependencies" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApplicationDependencies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListApplicationDependencies where
  toPath ListApplicationDependencies' {..} =
    Lude.mconcat
      ["/applications/", Lude.toBS applicationId, "/dependencies"]

instance Lude.ToQuery ListApplicationDependencies where
  toQuery ListApplicationDependencies' {..} =
    Lude.mconcat
      [ "semanticVersion" Lude.=: semanticVersion,
        "nextToken" Lude.=: nextToken,
        "maxItems" Lude.=: maxItems
      ]

-- | /See:/ 'mkListApplicationDependenciesResponse' smart constructor.
data ListApplicationDependenciesResponse = ListApplicationDependenciesResponse'
  { -- | An array of application summaries nested in the application.
    dependencies :: Lude.Maybe [ApplicationDependencySummary],
    -- | The token to request the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApplicationDependenciesResponse' with the minimum fields required to make a request.
--
-- * 'dependencies' - An array of application summaries nested in the application.
-- * 'nextToken' - The token to request the next page of results.
-- * 'responseStatus' - The response status code.
mkListApplicationDependenciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApplicationDependenciesResponse
mkListApplicationDependenciesResponse pResponseStatus_ =
  ListApplicationDependenciesResponse'
    { dependencies = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of application summaries nested in the application.
--
-- /Note:/ Consider using 'dependencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrsDependencies :: Lens.Lens' ListApplicationDependenciesResponse (Lude.Maybe [ApplicationDependencySummary])
ladrsDependencies = Lens.lens (dependencies :: ListApplicationDependenciesResponse -> Lude.Maybe [ApplicationDependencySummary]) (\s a -> s {dependencies = a} :: ListApplicationDependenciesResponse)
{-# DEPRECATED ladrsDependencies "Use generic-lens or generic-optics with 'dependencies' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrsNextToken :: Lens.Lens' ListApplicationDependenciesResponse (Lude.Maybe Lude.Text)
ladrsNextToken = Lens.lens (nextToken :: ListApplicationDependenciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationDependenciesResponse)
{-# DEPRECATED ladrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrsResponseStatus :: Lens.Lens' ListApplicationDependenciesResponse Lude.Int
ladrsResponseStatus = Lens.lens (responseStatus :: ListApplicationDependenciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApplicationDependenciesResponse)
{-# DEPRECATED ladrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
