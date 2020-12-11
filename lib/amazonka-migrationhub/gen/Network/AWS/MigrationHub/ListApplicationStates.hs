{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListApplicationStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the migration statuses for your applications. If you use the optional @ApplicationIds@ parameter, only the migration statuses for those applications will be returned.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListApplicationStates
  ( -- * Creating a request
    ListApplicationStates (..),
    mkListApplicationStates,

    -- ** Request lenses
    lasNextToken,
    lasApplicationIds,
    lasMaxResults,

    -- * Destructuring the response
    ListApplicationStatesResponse (..),
    mkListApplicationStatesResponse,

    -- ** Response lenses
    lasrsApplicationStateList,
    lasrsNextToken,
    lasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListApplicationStates' smart constructor.
data ListApplicationStates = ListApplicationStates'
  { nextToken ::
      Lude.Maybe Lude.Text,
    applicationIds ::
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

-- | Creates a value of 'ListApplicationStates' with the minimum fields required to make a request.
--
-- * 'applicationIds' - The configurationIds from the Application Discovery Service that uniquely identifies your applications.
-- * 'maxResults' - Maximum number of results to be returned per page.
-- * 'nextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
mkListApplicationStates ::
  ListApplicationStates
mkListApplicationStates =
  ListApplicationStates'
    { nextToken = Lude.Nothing,
      applicationIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasNextToken :: Lens.Lens' ListApplicationStates (Lude.Maybe Lude.Text)
lasNextToken = Lens.lens (nextToken :: ListApplicationStates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationStates)
{-# DEPRECATED lasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The configurationIds from the Application Discovery Service that uniquely identifies your applications.
--
-- /Note:/ Consider using 'applicationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasApplicationIds :: Lens.Lens' ListApplicationStates (Lude.Maybe (Lude.NonEmpty Lude.Text))
lasApplicationIds = Lens.lens (applicationIds :: ListApplicationStates -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {applicationIds = a} :: ListApplicationStates)
{-# DEPRECATED lasApplicationIds "Use generic-lens or generic-optics with 'applicationIds' instead." #-}

-- | Maximum number of results to be returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasMaxResults :: Lens.Lens' ListApplicationStates (Lude.Maybe Lude.Natural)
lasMaxResults = Lens.lens (maxResults :: ListApplicationStates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListApplicationStates)
{-# DEPRECATED lasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListApplicationStates where
  page rq rs
    | Page.stop (rs Lens.^. lasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasrsApplicationStateList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lasNextToken Lens..~ rs Lens.^. lasrsNextToken

instance Lude.AWSRequest ListApplicationStates where
  type Rs ListApplicationStates = ListApplicationStatesResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApplicationStatesResponse'
            Lude.<$> (x Lude..?> "ApplicationStateList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApplicationStates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.ListApplicationStates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListApplicationStates where
  toJSON ListApplicationStates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ApplicationIds" Lude..=) Lude.<$> applicationIds,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListApplicationStates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListApplicationStates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListApplicationStatesResponse' smart constructor.
data ListApplicationStatesResponse = ListApplicationStatesResponse'
  { applicationStateList ::
      Lude.Maybe [ApplicationState],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListApplicationStatesResponse' with the minimum fields required to make a request.
--
-- * 'applicationStateList' - A list of Applications that exist in Application Discovery Service.
-- * 'nextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
-- * 'responseStatus' - The response status code.
mkListApplicationStatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApplicationStatesResponse
mkListApplicationStatesResponse pResponseStatus_ =
  ListApplicationStatesResponse'
    { applicationStateList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of Applications that exist in Application Discovery Service.
--
-- /Note:/ Consider using 'applicationStateList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsApplicationStateList :: Lens.Lens' ListApplicationStatesResponse (Lude.Maybe [ApplicationState])
lasrsApplicationStateList = Lens.lens (applicationStateList :: ListApplicationStatesResponse -> Lude.Maybe [ApplicationState]) (\s a -> s {applicationStateList = a} :: ListApplicationStatesResponse)
{-# DEPRECATED lasrsApplicationStateList "Use generic-lens or generic-optics with 'applicationStateList' instead." #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsNextToken :: Lens.Lens' ListApplicationStatesResponse (Lude.Maybe Lude.Text)
lasrsNextToken = Lens.lens (nextToken :: ListApplicationStatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApplicationStatesResponse)
{-# DEPRECATED lasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsResponseStatus :: Lens.Lens' ListApplicationStatesResponse Lude.Int
lasrsResponseStatus = Lens.lens (responseStatus :: ListApplicationStatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApplicationStatesResponse)
{-# DEPRECATED lasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
