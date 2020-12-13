{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DeleteAssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment target that is specified by the ARN of the assessment target.
module Network.AWS.Inspector.DeleteAssessmentTarget
  ( -- * Creating a request
    DeleteAssessmentTarget (..),
    mkDeleteAssessmentTarget,

    -- ** Request lenses
    datAssessmentTargetARN,

    -- * Destructuring the response
    DeleteAssessmentTargetResponse (..),
    mkDeleteAssessmentTargetResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAssessmentTarget' smart constructor.
newtype DeleteAssessmentTarget = DeleteAssessmentTarget'
  { -- | The ARN that specifies the assessment target that you want to delete.
    assessmentTargetARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAssessmentTarget' with the minimum fields required to make a request.
--
-- * 'assessmentTargetARN' - The ARN that specifies the assessment target that you want to delete.
mkDeleteAssessmentTarget ::
  -- | 'assessmentTargetARN'
  Lude.Text ->
  DeleteAssessmentTarget
mkDeleteAssessmentTarget pAssessmentTargetARN_ =
  DeleteAssessmentTarget'
    { assessmentTargetARN =
        pAssessmentTargetARN_
    }

-- | The ARN that specifies the assessment target that you want to delete.
--
-- /Note:/ Consider using 'assessmentTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTargetARN :: Lens.Lens' DeleteAssessmentTarget Lude.Text
datAssessmentTargetARN = Lens.lens (assessmentTargetARN :: DeleteAssessmentTarget -> Lude.Text) (\s a -> s {assessmentTargetARN = a} :: DeleteAssessmentTarget)
{-# DEPRECATED datAssessmentTargetARN "Use generic-lens or generic-optics with 'assessmentTargetARN' instead." #-}

instance Lude.AWSRequest DeleteAssessmentTarget where
  type Rs DeleteAssessmentTarget = DeleteAssessmentTargetResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull DeleteAssessmentTargetResponse'

instance Lude.ToHeaders DeleteAssessmentTarget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DeleteAssessmentTarget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAssessmentTarget where
  toJSON DeleteAssessmentTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("assessmentTargetArn" Lude..= assessmentTargetARN)]
      )

instance Lude.ToPath DeleteAssessmentTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAssessmentTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAssessmentTargetResponse' smart constructor.
data DeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAssessmentTargetResponse' with the minimum fields required to make a request.
mkDeleteAssessmentTargetResponse ::
  DeleteAssessmentTargetResponse
mkDeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
