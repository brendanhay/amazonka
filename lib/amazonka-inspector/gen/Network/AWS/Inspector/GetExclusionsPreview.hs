{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.GetExclusionsPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the exclusions preview (a list of ExclusionPreview objects) specified by the preview token. You can obtain the preview token by running the CreateExclusionsPreview API.
module Network.AWS.Inspector.GetExclusionsPreview
  ( -- * Creating a request
    GetExclusionsPreview (..),
    mkGetExclusionsPreview,

    -- ** Request lenses
    gepLocale,
    gepNextToken,
    gepAssessmentTemplateARN,
    gepMaxResults,
    gepPreviewToken,

    -- * Destructuring the response
    GetExclusionsPreviewResponse (..),
    mkGetExclusionsPreviewResponse,

    -- ** Response lenses
    geprsExclusionPreviews,
    geprsPreviewStatus,
    geprsNextToken,
    geprsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetExclusionsPreview' smart constructor.
data GetExclusionsPreview = GetExclusionsPreview'
  { -- | The locale into which you want to translate the exclusion's title, description, and recommendation.
    locale :: Lude.Maybe Locale,
    -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the GetExclusionsPreviewRequest action. Subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ARN that specifies the assessment template for which the exclusions preview was requested.
    assessmentTemplateARN :: Lude.Text,
    -- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 100. The maximum value is 500.
    maxResults :: Lude.Maybe Lude.Int,
    -- | The unique identifier associated of the exclusions preview.
    previewToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExclusionsPreview' with the minimum fields required to make a request.
--
-- * 'locale' - The locale into which you want to translate the exclusion's title, description, and recommendation.
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the GetExclusionsPreviewRequest action. Subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
-- * 'assessmentTemplateARN' - The ARN that specifies the assessment template for which the exclusions preview was requested.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 100. The maximum value is 500.
-- * 'previewToken' - The unique identifier associated of the exclusions preview.
mkGetExclusionsPreview ::
  -- | 'assessmentTemplateARN'
  Lude.Text ->
  -- | 'previewToken'
  Lude.Text ->
  GetExclusionsPreview
mkGetExclusionsPreview pAssessmentTemplateARN_ pPreviewToken_ =
  GetExclusionsPreview'
    { locale = Lude.Nothing,
      nextToken = Lude.Nothing,
      assessmentTemplateARN = pAssessmentTemplateARN_,
      maxResults = Lude.Nothing,
      previewToken = pPreviewToken_
    }

-- | The locale into which you want to translate the exclusion's title, description, and recommendation.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepLocale :: Lens.Lens' GetExclusionsPreview (Lude.Maybe Locale)
gepLocale = Lens.lens (locale :: GetExclusionsPreview -> Lude.Maybe Locale) (\s a -> s {locale = a} :: GetExclusionsPreview)
{-# DEPRECATED gepLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the GetExclusionsPreviewRequest action. Subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepNextToken :: Lens.Lens' GetExclusionsPreview (Lude.Maybe Lude.Text)
gepNextToken = Lens.lens (nextToken :: GetExclusionsPreview -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetExclusionsPreview)
{-# DEPRECATED gepNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARN that specifies the assessment template for which the exclusions preview was requested.
--
-- /Note:/ Consider using 'assessmentTemplateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepAssessmentTemplateARN :: Lens.Lens' GetExclusionsPreview Lude.Text
gepAssessmentTemplateARN = Lens.lens (assessmentTemplateARN :: GetExclusionsPreview -> Lude.Text) (\s a -> s {assessmentTemplateARN = a} :: GetExclusionsPreview)
{-# DEPRECATED gepAssessmentTemplateARN "Use generic-lens or generic-optics with 'assessmentTemplateARN' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 100. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepMaxResults :: Lens.Lens' GetExclusionsPreview (Lude.Maybe Lude.Int)
gepMaxResults = Lens.lens (maxResults :: GetExclusionsPreview -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetExclusionsPreview)
{-# DEPRECATED gepMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier associated of the exclusions preview.
--
-- /Note:/ Consider using 'previewToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepPreviewToken :: Lens.Lens' GetExclusionsPreview Lude.Text
gepPreviewToken = Lens.lens (previewToken :: GetExclusionsPreview -> Lude.Text) (\s a -> s {previewToken = a} :: GetExclusionsPreview)
{-# DEPRECATED gepPreviewToken "Use generic-lens or generic-optics with 'previewToken' instead." #-}

instance Lude.AWSRequest GetExclusionsPreview where
  type Rs GetExclusionsPreview = GetExclusionsPreviewResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetExclusionsPreviewResponse'
            Lude.<$> (x Lude..?> "exclusionPreviews" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "previewStatus")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetExclusionsPreview where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.GetExclusionsPreview" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetExclusionsPreview where
  toJSON GetExclusionsPreview' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("assessmentTemplateArn" Lude..= assessmentTemplateARN),
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("previewToken" Lude..= previewToken)
          ]
      )

instance Lude.ToPath GetExclusionsPreview where
  toPath = Lude.const "/"

instance Lude.ToQuery GetExclusionsPreview where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetExclusionsPreviewResponse' smart constructor.
data GetExclusionsPreviewResponse = GetExclusionsPreviewResponse'
  { -- | Information about the exclusions included in the preview.
    exclusionPreviews :: Lude.Maybe [ExclusionPreview],
    -- | Specifies the status of the request to generate an exclusions preview.
    previewStatus :: PreviewStatus,
    -- | When a response is generated, if there is more data to be listed, this parameters is present in the response and contains the value to use for the nextToken parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExclusionsPreviewResponse' with the minimum fields required to make a request.
--
-- * 'exclusionPreviews' - Information about the exclusions included in the preview.
-- * 'previewStatus' - Specifies the status of the request to generate an exclusions preview.
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameters is present in the response and contains the value to use for the nextToken parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'responseStatus' - The response status code.
mkGetExclusionsPreviewResponse ::
  -- | 'previewStatus'
  PreviewStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  GetExclusionsPreviewResponse
mkGetExclusionsPreviewResponse pPreviewStatus_ pResponseStatus_ =
  GetExclusionsPreviewResponse'
    { exclusionPreviews = Lude.Nothing,
      previewStatus = pPreviewStatus_,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the exclusions included in the preview.
--
-- /Note:/ Consider using 'exclusionPreviews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprsExclusionPreviews :: Lens.Lens' GetExclusionsPreviewResponse (Lude.Maybe [ExclusionPreview])
geprsExclusionPreviews = Lens.lens (exclusionPreviews :: GetExclusionsPreviewResponse -> Lude.Maybe [ExclusionPreview]) (\s a -> s {exclusionPreviews = a} :: GetExclusionsPreviewResponse)
{-# DEPRECATED geprsExclusionPreviews "Use generic-lens or generic-optics with 'exclusionPreviews' instead." #-}

-- | Specifies the status of the request to generate an exclusions preview.
--
-- /Note:/ Consider using 'previewStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprsPreviewStatus :: Lens.Lens' GetExclusionsPreviewResponse PreviewStatus
geprsPreviewStatus = Lens.lens (previewStatus :: GetExclusionsPreviewResponse -> PreviewStatus) (\s a -> s {previewStatus = a} :: GetExclusionsPreviewResponse)
{-# DEPRECATED geprsPreviewStatus "Use generic-lens or generic-optics with 'previewStatus' instead." #-}

-- | When a response is generated, if there is more data to be listed, this parameters is present in the response and contains the value to use for the nextToken parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprsNextToken :: Lens.Lens' GetExclusionsPreviewResponse (Lude.Maybe Lude.Text)
geprsNextToken = Lens.lens (nextToken :: GetExclusionsPreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetExclusionsPreviewResponse)
{-# DEPRECATED geprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprsResponseStatus :: Lens.Lens' GetExclusionsPreviewResponse Lude.Int
geprsResponseStatus = Lens.lens (responseStatus :: GetExclusionsPreviewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExclusionsPreviewResponse)
{-# DEPRECATED geprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
