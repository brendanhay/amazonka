{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.StartAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the assessment run specified by the ARN of the assessment template. For this API to function properly, you must not exceed the limit of running up to 500 concurrent agents per AWS account.
module Network.AWS.Inspector.StartAssessmentRun
  ( -- * Creating a request
    StartAssessmentRun (..),
    mkStartAssessmentRun,

    -- ** Request lenses
    sarAssessmentRunName,
    sarAssessmentTemplateARN,

    -- * Destructuring the response
    StartAssessmentRunResponse (..),
    mkStartAssessmentRunResponse,

    -- ** Response lenses
    sarrsAssessmentRunARN,
    sarrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartAssessmentRun' smart constructor.
data StartAssessmentRun = StartAssessmentRun'
  { -- | You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
    assessmentRunName :: Lude.Maybe Lude.Text,
    -- | The ARN of the assessment template of the assessment run that you want to start.
    assessmentTemplateARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAssessmentRun' with the minimum fields required to make a request.
--
-- * 'assessmentRunName' - You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
-- * 'assessmentTemplateARN' - The ARN of the assessment template of the assessment run that you want to start.
mkStartAssessmentRun ::
  -- | 'assessmentTemplateARN'
  Lude.Text ->
  StartAssessmentRun
mkStartAssessmentRun pAssessmentTemplateARN_ =
  StartAssessmentRun'
    { assessmentRunName = Lude.Nothing,
      assessmentTemplateARN = pAssessmentTemplateARN_
    }

-- | You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
--
-- /Note:/ Consider using 'assessmentRunName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentRunName :: Lens.Lens' StartAssessmentRun (Lude.Maybe Lude.Text)
sarAssessmentRunName = Lens.lens (assessmentRunName :: StartAssessmentRun -> Lude.Maybe Lude.Text) (\s a -> s {assessmentRunName = a} :: StartAssessmentRun)
{-# DEPRECATED sarAssessmentRunName "Use generic-lens or generic-optics with 'assessmentRunName' instead." #-}

-- | The ARN of the assessment template of the assessment run that you want to start.
--
-- /Note:/ Consider using 'assessmentTemplateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentTemplateARN :: Lens.Lens' StartAssessmentRun Lude.Text
sarAssessmentTemplateARN = Lens.lens (assessmentTemplateARN :: StartAssessmentRun -> Lude.Text) (\s a -> s {assessmentTemplateARN = a} :: StartAssessmentRun)
{-# DEPRECATED sarAssessmentTemplateARN "Use generic-lens or generic-optics with 'assessmentTemplateARN' instead." #-}

instance Lude.AWSRequest StartAssessmentRun where
  type Rs StartAssessmentRun = StartAssessmentRunResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartAssessmentRunResponse'
            Lude.<$> (x Lude..:> "assessmentRunArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartAssessmentRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.StartAssessmentRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartAssessmentRun where
  toJSON StartAssessmentRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("assessmentRunName" Lude..=) Lude.<$> assessmentRunName,
            Lude.Just ("assessmentTemplateArn" Lude..= assessmentTemplateARN)
          ]
      )

instance Lude.ToPath StartAssessmentRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartAssessmentRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartAssessmentRunResponse' smart constructor.
data StartAssessmentRunResponse = StartAssessmentRunResponse'
  { -- | The ARN of the assessment run that has been started.
    assessmentRunARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAssessmentRunResponse' with the minimum fields required to make a request.
--
-- * 'assessmentRunARN' - The ARN of the assessment run that has been started.
-- * 'responseStatus' - The response status code.
mkStartAssessmentRunResponse ::
  -- | 'assessmentRunARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  StartAssessmentRunResponse
mkStartAssessmentRunResponse pAssessmentRunARN_ pResponseStatus_ =
  StartAssessmentRunResponse'
    { assessmentRunARN =
        pAssessmentRunARN_,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the assessment run that has been started.
--
-- /Note:/ Consider using 'assessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsAssessmentRunARN :: Lens.Lens' StartAssessmentRunResponse Lude.Text
sarrsAssessmentRunARN = Lens.lens (assessmentRunARN :: StartAssessmentRunResponse -> Lude.Text) (\s a -> s {assessmentRunARN = a} :: StartAssessmentRunResponse)
{-# DEPRECATED sarrsAssessmentRunARN "Use generic-lens or generic-optics with 'assessmentRunARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsResponseStatus :: Lens.Lens' StartAssessmentRunResponse Lude.Int
sarrsResponseStatus = Lens.lens (responseStatus :: StartAssessmentRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartAssessmentRunResponse)
{-# DEPRECATED sarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
