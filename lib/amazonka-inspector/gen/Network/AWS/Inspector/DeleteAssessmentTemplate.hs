{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DeleteAssessmentTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment template that is specified by the ARN of the assessment template.
module Network.AWS.Inspector.DeleteAssessmentTemplate
  ( -- * Creating a request
    DeleteAssessmentTemplate (..),
    mkDeleteAssessmentTemplate,

    -- ** Request lenses
    datAssessmentTemplateARN,

    -- * Destructuring the response
    DeleteAssessmentTemplateResponse (..),
    mkDeleteAssessmentTemplateResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAssessmentTemplate' smart constructor.
newtype DeleteAssessmentTemplate = DeleteAssessmentTemplate'
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

-- | Creates a value of 'DeleteAssessmentTemplate' with the minimum fields required to make a request.
--
-- * 'assessmentTemplateARN' - The ARN that specifies the assessment template that you want to delete.
mkDeleteAssessmentTemplate ::
  -- | 'assessmentTemplateARN'
  Lude.Text ->
  DeleteAssessmentTemplate
mkDeleteAssessmentTemplate pAssessmentTemplateARN_ =
  DeleteAssessmentTemplate'
    { assessmentTemplateARN =
        pAssessmentTemplateARN_
    }

-- | The ARN that specifies the assessment template that you want to delete.
--
-- /Note:/ Consider using 'assessmentTemplateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTemplateARN :: Lens.Lens' DeleteAssessmentTemplate Lude.Text
datAssessmentTemplateARN = Lens.lens (assessmentTemplateARN :: DeleteAssessmentTemplate -> Lude.Text) (\s a -> s {assessmentTemplateARN = a} :: DeleteAssessmentTemplate)
{-# DEPRECATED datAssessmentTemplateARN "Use generic-lens or generic-optics with 'assessmentTemplateARN' instead." #-}

instance Lude.AWSRequest DeleteAssessmentTemplate where
  type Rs DeleteAssessmentTemplate = DeleteAssessmentTemplateResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull DeleteAssessmentTemplateResponse'

instance Lude.ToHeaders DeleteAssessmentTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DeleteAssessmentTemplate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAssessmentTemplate where
  toJSON DeleteAssessmentTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("assessmentTemplateArn" Lude..= assessmentTemplateARN)
          ]
      )

instance Lude.ToPath DeleteAssessmentTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAssessmentTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAssessmentTemplateResponse' smart constructor.
data DeleteAssessmentTemplateResponse = DeleteAssessmentTemplateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAssessmentTemplateResponse' with the minimum fields required to make a request.
mkDeleteAssessmentTemplateResponse ::
  DeleteAssessmentTemplateResponse
mkDeleteAssessmentTemplateResponse =
  DeleteAssessmentTemplateResponse'
