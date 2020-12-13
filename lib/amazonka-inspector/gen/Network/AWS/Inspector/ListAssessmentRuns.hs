{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListAssessmentRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment runs that correspond to the assessment templates that are specified by the ARNs of the assessment templates.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentRuns
  ( -- * Creating a request
    ListAssessmentRuns (..),
    mkListAssessmentRuns,

    -- ** Request lenses
    larNextToken,
    larFilter,
    larAssessmentTemplateARNs,
    larMaxResults,

    -- * Destructuring the response
    ListAssessmentRunsResponse (..),
    mkListAssessmentRunsResponse,

    -- ** Response lenses
    larrsAssessmentRunARNs,
    larrsNextToken,
    larrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssessmentRuns' smart constructor.
data ListAssessmentRuns = ListAssessmentRuns'
  { -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRuns__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | You can use this parameter to specify a subset of data to be included in the action's response.
    --
    -- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
    filter :: Lude.Maybe AssessmentRunFilter,
    -- | The ARNs that specify the assessment templates whose assessment runs you want to list.
    assessmentTemplateARNs :: Lude.Maybe [Lude.Text],
    -- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentRuns' with the minimum fields required to make a request.
--
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRuns__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
-- * 'filter' - You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
-- * 'assessmentTemplateARNs' - The ARNs that specify the assessment templates whose assessment runs you want to list.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
mkListAssessmentRuns ::
  ListAssessmentRuns
mkListAssessmentRuns =
  ListAssessmentRuns'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      assessmentTemplateARNs = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRuns__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larNextToken :: Lens.Lens' ListAssessmentRuns (Lude.Maybe Lude.Text)
larNextToken = Lens.lens (nextToken :: ListAssessmentRuns -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentRuns)
{-# DEPRECATED larNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larFilter :: Lens.Lens' ListAssessmentRuns (Lude.Maybe AssessmentRunFilter)
larFilter = Lens.lens (filter :: ListAssessmentRuns -> Lude.Maybe AssessmentRunFilter) (\s a -> s {filter = a} :: ListAssessmentRuns)
{-# DEPRECATED larFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The ARNs that specify the assessment templates whose assessment runs you want to list.
--
-- /Note:/ Consider using 'assessmentTemplateARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larAssessmentTemplateARNs :: Lens.Lens' ListAssessmentRuns (Lude.Maybe [Lude.Text])
larAssessmentTemplateARNs = Lens.lens (assessmentTemplateARNs :: ListAssessmentRuns -> Lude.Maybe [Lude.Text]) (\s a -> s {assessmentTemplateARNs = a} :: ListAssessmentRuns)
{-# DEPRECATED larAssessmentTemplateARNs "Use generic-lens or generic-optics with 'assessmentTemplateARNs' instead." #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larMaxResults :: Lens.Lens' ListAssessmentRuns (Lude.Maybe Lude.Int)
larMaxResults = Lens.lens (maxResults :: ListAssessmentRuns -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListAssessmentRuns)
{-# DEPRECATED larMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAssessmentRuns where
  page rq rs
    | Page.stop (rs Lens.^. larrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larrsAssessmentRunARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& larNextToken Lens..~ rs Lens.^. larrsNextToken

instance Lude.AWSRequest ListAssessmentRuns where
  type Rs ListAssessmentRuns = ListAssessmentRunsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssessmentRunsResponse'
            Lude.<$> (x Lude..?> "assessmentRunArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssessmentRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.ListAssessmentRuns" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssessmentRuns where
  toJSON ListAssessmentRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("assessmentTemplateArns" Lude..=) Lude.<$> assessmentTemplateARNs,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAssessmentRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssessmentRuns where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssessmentRunsResponse' smart constructor.
data ListAssessmentRunsResponse = ListAssessmentRunsResponse'
  { -- | A list of ARNs that specifies the assessment runs that are returned by the action.
    assessmentRunARNs :: [Lude.Text],
    -- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentRunsResponse' with the minimum fields required to make a request.
--
-- * 'assessmentRunARNs' - A list of ARNs that specifies the assessment runs that are returned by the action.
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'responseStatus' - The response status code.
mkListAssessmentRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssessmentRunsResponse
mkListAssessmentRunsResponse pResponseStatus_ =
  ListAssessmentRunsResponse'
    { assessmentRunARNs = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of ARNs that specifies the assessment runs that are returned by the action.
--
-- /Note:/ Consider using 'assessmentRunARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAssessmentRunARNs :: Lens.Lens' ListAssessmentRunsResponse [Lude.Text]
larrsAssessmentRunARNs = Lens.lens (assessmentRunARNs :: ListAssessmentRunsResponse -> [Lude.Text]) (\s a -> s {assessmentRunARNs = a} :: ListAssessmentRunsResponse)
{-# DEPRECATED larrsAssessmentRunARNs "Use generic-lens or generic-optics with 'assessmentRunARNs' instead." #-}

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListAssessmentRunsResponse (Lude.Maybe Lude.Text)
larrsNextToken = Lens.lens (nextToken :: ListAssessmentRunsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentRunsResponse)
{-# DEPRECATED larrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAssessmentRunsResponse Lude.Int
larrsResponseStatus = Lens.lens (responseStatus :: ListAssessmentRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssessmentRunsResponse)
{-# DEPRECATED larrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
