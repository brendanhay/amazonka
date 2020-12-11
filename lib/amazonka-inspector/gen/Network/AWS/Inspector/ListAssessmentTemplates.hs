{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListAssessmentTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment templates that correspond to the assessment targets that are specified by the ARNs of the assessment targets.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTemplates
  ( -- * Creating a request
    ListAssessmentTemplates (..),
    mkListAssessmentTemplates,

    -- ** Request lenses
    latNextToken,
    latFilter,
    latMaxResults,
    latAssessmentTargetARNs,

    -- * Destructuring the response
    ListAssessmentTemplatesResponse (..),
    mkListAssessmentTemplatesResponse,

    -- ** Response lenses
    latrsNextToken,
    latrsResponseStatus,
    latrsAssessmentTemplateARNs,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssessmentTemplates' smart constructor.
data ListAssessmentTemplates = ListAssessmentTemplates'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe AssessmentTemplateFilter,
    maxResults :: Lude.Maybe Lude.Int,
    assessmentTargetARNs ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentTemplates' with the minimum fields required to make a request.
--
-- * 'assessmentTargetARNs' - A list of ARNs that specifies the assessment targets whose assessment templates you want to list.
-- * 'filter' - You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTemplates__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
mkListAssessmentTemplates ::
  ListAssessmentTemplates
mkListAssessmentTemplates =
  ListAssessmentTemplates'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing,
      assessmentTargetARNs = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTemplates__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextToken :: Lens.Lens' ListAssessmentTemplates (Lude.Maybe Lude.Text)
latNextToken = Lens.lens (nextToken :: ListAssessmentTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentTemplates)
{-# DEPRECATED latNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latFilter :: Lens.Lens' ListAssessmentTemplates (Lude.Maybe AssessmentTemplateFilter)
latFilter = Lens.lens (filter :: ListAssessmentTemplates -> Lude.Maybe AssessmentTemplateFilter) (\s a -> s {filter = a} :: ListAssessmentTemplates)
{-# DEPRECATED latFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latMaxResults :: Lens.Lens' ListAssessmentTemplates (Lude.Maybe Lude.Int)
latMaxResults = Lens.lens (maxResults :: ListAssessmentTemplates -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListAssessmentTemplates)
{-# DEPRECATED latMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A list of ARNs that specifies the assessment targets whose assessment templates you want to list.
--
-- /Note:/ Consider using 'assessmentTargetARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latAssessmentTargetARNs :: Lens.Lens' ListAssessmentTemplates (Lude.Maybe [Lude.Text])
latAssessmentTargetARNs = Lens.lens (assessmentTargetARNs :: ListAssessmentTemplates -> Lude.Maybe [Lude.Text]) (\s a -> s {assessmentTargetARNs = a} :: ListAssessmentTemplates)
{-# DEPRECATED latAssessmentTargetARNs "Use generic-lens or generic-optics with 'assessmentTargetARNs' instead." #-}

instance Page.AWSPager ListAssessmentTemplates where
  page rq rs
    | Page.stop (rs Lens.^. latrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. latrsAssessmentTemplateARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& latNextToken Lens..~ rs Lens.^. latrsNextToken

instance Lude.AWSRequest ListAssessmentTemplates where
  type Rs ListAssessmentTemplates = ListAssessmentTemplatesResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssessmentTemplatesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "assessmentTemplateArns" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListAssessmentTemplates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.ListAssessmentTemplates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssessmentTemplates where
  toJSON ListAssessmentTemplates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("assessmentTargetArns" Lude..=) Lude.<$> assessmentTargetARNs
          ]
      )

instance Lude.ToPath ListAssessmentTemplates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssessmentTemplates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssessmentTemplatesResponse' smart constructor.
data ListAssessmentTemplatesResponse = ListAssessmentTemplatesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    assessmentTemplateARNs ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssessmentTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'assessmentTemplateARNs' - A list of ARNs that specifies the assessment templates returned by the action.
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'responseStatus' - The response status code.
mkListAssessmentTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssessmentTemplatesResponse
mkListAssessmentTemplatesResponse pResponseStatus_ =
  ListAssessmentTemplatesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      assessmentTemplateARNs = Lude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsNextToken :: Lens.Lens' ListAssessmentTemplatesResponse (Lude.Maybe Lude.Text)
latrsNextToken = Lens.lens (nextToken :: ListAssessmentTemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssessmentTemplatesResponse)
{-# DEPRECATED latrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsResponseStatus :: Lens.Lens' ListAssessmentTemplatesResponse Lude.Int
latrsResponseStatus = Lens.lens (responseStatus :: ListAssessmentTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssessmentTemplatesResponse)
{-# DEPRECATED latrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of ARNs that specifies the assessment templates returned by the action.
--
-- /Note:/ Consider using 'assessmentTemplateARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrsAssessmentTemplateARNs :: Lens.Lens' ListAssessmentTemplatesResponse [Lude.Text]
latrsAssessmentTemplateARNs = Lens.lens (assessmentTemplateARNs :: ListAssessmentTemplatesResponse -> [Lude.Text]) (\s a -> s {assessmentTemplateARNs = a} :: ListAssessmentTemplatesResponse)
{-# DEPRECATED latrsAssessmentTemplateARNs "Use generic-lens or generic-optics with 'assessmentTemplateARNs' instead." #-}
