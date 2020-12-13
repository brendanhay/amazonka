{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeAssessmentTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment templates that are specified by the ARNs of the assessment templates.
module Network.AWS.Inspector.DescribeAssessmentTemplates
  ( -- * Creating a request
    DescribeAssessmentTemplates (..),
    mkDescribeAssessmentTemplates,

    -- ** Request lenses
    datAssessmentTemplateARNs,

    -- * Destructuring the response
    DescribeAssessmentTemplatesResponse (..),
    mkDescribeAssessmentTemplatesResponse,

    -- ** Response lenses
    datrsAssessmentTemplates,
    datrsFailedItems,
    datrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAssessmentTemplates' smart constructor.
newtype DescribeAssessmentTemplates = DescribeAssessmentTemplates'
  { assessmentTemplateARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssessmentTemplates' with the minimum fields required to make a request.
--
-- * 'assessmentTemplateARNs' -
mkDescribeAssessmentTemplates ::
  -- | 'assessmentTemplateARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeAssessmentTemplates
mkDescribeAssessmentTemplates pAssessmentTemplateARNs_ =
  DescribeAssessmentTemplates'
    { assessmentTemplateARNs =
        pAssessmentTemplateARNs_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'assessmentTemplateARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTemplateARNs :: Lens.Lens' DescribeAssessmentTemplates (Lude.NonEmpty Lude.Text)
datAssessmentTemplateARNs = Lens.lens (assessmentTemplateARNs :: DescribeAssessmentTemplates -> Lude.NonEmpty Lude.Text) (\s a -> s {assessmentTemplateARNs = a} :: DescribeAssessmentTemplates)
{-# DEPRECATED datAssessmentTemplateARNs "Use generic-lens or generic-optics with 'assessmentTemplateARNs' instead." #-}

instance Lude.AWSRequest DescribeAssessmentTemplates where
  type
    Rs DescribeAssessmentTemplates =
      DescribeAssessmentTemplatesResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAssessmentTemplatesResponse'
            Lude.<$> (x Lude..?> "assessmentTemplates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAssessmentTemplates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "InspectorService.DescribeAssessmentTemplates" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAssessmentTemplates where
  toJSON DescribeAssessmentTemplates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("assessmentTemplateArns" Lude..= assessmentTemplateARNs)
          ]
      )

instance Lude.ToPath DescribeAssessmentTemplates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAssessmentTemplates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAssessmentTemplatesResponse' smart constructor.
data DescribeAssessmentTemplatesResponse = DescribeAssessmentTemplatesResponse'
  { -- | Information about the assessment templates.
    assessmentTemplates :: [AssessmentTemplate],
    -- | Assessment template details that cannot be described. An error code is provided for each failed item.
    failedItems :: Lude.HashMap Lude.Text (FailedItemDetails),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssessmentTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'assessmentTemplates' - Information about the assessment templates.
-- * 'failedItems' - Assessment template details that cannot be described. An error code is provided for each failed item.
-- * 'responseStatus' - The response status code.
mkDescribeAssessmentTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAssessmentTemplatesResponse
mkDescribeAssessmentTemplatesResponse pResponseStatus_ =
  DescribeAssessmentTemplatesResponse'
    { assessmentTemplates =
        Lude.mempty,
      failedItems = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | Information about the assessment templates.
--
-- /Note:/ Consider using 'assessmentTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsAssessmentTemplates :: Lens.Lens' DescribeAssessmentTemplatesResponse [AssessmentTemplate]
datrsAssessmentTemplates = Lens.lens (assessmentTemplates :: DescribeAssessmentTemplatesResponse -> [AssessmentTemplate]) (\s a -> s {assessmentTemplates = a} :: DescribeAssessmentTemplatesResponse)
{-# DEPRECATED datrsAssessmentTemplates "Use generic-lens or generic-optics with 'assessmentTemplates' instead." #-}

-- | Assessment template details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsFailedItems :: Lens.Lens' DescribeAssessmentTemplatesResponse (Lude.HashMap Lude.Text (FailedItemDetails))
datrsFailedItems = Lens.lens (failedItems :: DescribeAssessmentTemplatesResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: DescribeAssessmentTemplatesResponse)
{-# DEPRECATED datrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsResponseStatus :: Lens.Lens' DescribeAssessmentTemplatesResponse Lude.Int
datrsResponseStatus = Lens.lens (responseStatus :: DescribeAssessmentTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAssessmentTemplatesResponse)
{-# DEPRECATED datrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
