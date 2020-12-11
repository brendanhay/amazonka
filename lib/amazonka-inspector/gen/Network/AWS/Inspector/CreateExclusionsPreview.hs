{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.CreateExclusionsPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the generation of an exclusions preview for the specified assessment template. The exclusions preview lists the potential exclusions (ExclusionPreview) that Inspector can detect before it runs the assessment.
module Network.AWS.Inspector.CreateExclusionsPreview
  ( -- * Creating a request
    CreateExclusionsPreview (..),
    mkCreateExclusionsPreview,

    -- ** Request lenses
    cepAssessmentTemplateARN,

    -- * Destructuring the response
    CreateExclusionsPreviewResponse (..),
    mkCreateExclusionsPreviewResponse,

    -- ** Response lenses
    ceprsResponseStatus,
    ceprsPreviewToken,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateExclusionsPreview' smart constructor.
newtype CreateExclusionsPreview = CreateExclusionsPreview'
  { assessmentTemplateARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateExclusionsPreview' with the minimum fields required to make a request.
--
-- * 'assessmentTemplateARN' - The ARN that specifies the assessment template for which you want to create an exclusions preview.
mkCreateExclusionsPreview ::
  -- | 'assessmentTemplateARN'
  Lude.Text ->
  CreateExclusionsPreview
mkCreateExclusionsPreview pAssessmentTemplateARN_ =
  CreateExclusionsPreview'
    { assessmentTemplateARN =
        pAssessmentTemplateARN_
    }

-- | The ARN that specifies the assessment template for which you want to create an exclusions preview.
--
-- /Note:/ Consider using 'assessmentTemplateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cepAssessmentTemplateARN :: Lens.Lens' CreateExclusionsPreview Lude.Text
cepAssessmentTemplateARN = Lens.lens (assessmentTemplateARN :: CreateExclusionsPreview -> Lude.Text) (\s a -> s {assessmentTemplateARN = a} :: CreateExclusionsPreview)
{-# DEPRECATED cepAssessmentTemplateARN "Use generic-lens or generic-optics with 'assessmentTemplateARN' instead." #-}

instance Lude.AWSRequest CreateExclusionsPreview where
  type Rs CreateExclusionsPreview = CreateExclusionsPreviewResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateExclusionsPreviewResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "previewToken")
      )

instance Lude.ToHeaders CreateExclusionsPreview where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.CreateExclusionsPreview" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateExclusionsPreview where
  toJSON CreateExclusionsPreview' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("assessmentTemplateArn" Lude..= assessmentTemplateARN)
          ]
      )

instance Lude.ToPath CreateExclusionsPreview where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateExclusionsPreview where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateExclusionsPreviewResponse' smart constructor.
data CreateExclusionsPreviewResponse = CreateExclusionsPreviewResponse'
  { responseStatus ::
      Lude.Int,
    previewToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateExclusionsPreviewResponse' with the minimum fields required to make a request.
--
-- * 'previewToken' - Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
-- * 'responseStatus' - The response status code.
mkCreateExclusionsPreviewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'previewToken'
  Lude.Text ->
  CreateExclusionsPreviewResponse
mkCreateExclusionsPreviewResponse pResponseStatus_ pPreviewToken_ =
  CreateExclusionsPreviewResponse'
    { responseStatus =
        pResponseStatus_,
      previewToken = pPreviewToken_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceprsResponseStatus :: Lens.Lens' CreateExclusionsPreviewResponse Lude.Int
ceprsResponseStatus = Lens.lens (responseStatus :: CreateExclusionsPreviewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateExclusionsPreviewResponse)
{-# DEPRECATED ceprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
--
-- /Note:/ Consider using 'previewToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceprsPreviewToken :: Lens.Lens' CreateExclusionsPreviewResponse Lude.Text
ceprsPreviewToken = Lens.lens (previewToken :: CreateExclusionsPreviewResponse -> Lude.Text) (\s a -> s {previewToken = a} :: CreateExclusionsPreviewResponse)
{-# DEPRECATED ceprsPreviewToken "Use generic-lens or generic-optics with 'previewToken' instead." #-}
