{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DeleteAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment run that is specified by the ARN of the assessment run.
module Network.AWS.Inspector.DeleteAssessmentRun
  ( -- * Creating a request
    DeleteAssessmentRun (..),
    mkDeleteAssessmentRun,

    -- ** Request lenses
    darAssessmentRunARN,

    -- * Destructuring the response
    DeleteAssessmentRunResponse (..),
    mkDeleteAssessmentRunResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAssessmentRun' smart constructor.
newtype DeleteAssessmentRun = DeleteAssessmentRun'
  { assessmentRunARN ::
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

-- | Creates a value of 'DeleteAssessmentRun' with the minimum fields required to make a request.
--
-- * 'assessmentRunARN' - The ARN that specifies the assessment run that you want to delete.
mkDeleteAssessmentRun ::
  -- | 'assessmentRunARN'
  Lude.Text ->
  DeleteAssessmentRun
mkDeleteAssessmentRun pAssessmentRunARN_ =
  DeleteAssessmentRun' {assessmentRunARN = pAssessmentRunARN_}

-- | The ARN that specifies the assessment run that you want to delete.
--
-- /Note:/ Consider using 'assessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darAssessmentRunARN :: Lens.Lens' DeleteAssessmentRun Lude.Text
darAssessmentRunARN = Lens.lens (assessmentRunARN :: DeleteAssessmentRun -> Lude.Text) (\s a -> s {assessmentRunARN = a} :: DeleteAssessmentRun)
{-# DEPRECATED darAssessmentRunARN "Use generic-lens or generic-optics with 'assessmentRunARN' instead." #-}

instance Lude.AWSRequest DeleteAssessmentRun where
  type Rs DeleteAssessmentRun = DeleteAssessmentRunResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull DeleteAssessmentRunResponse'

instance Lude.ToHeaders DeleteAssessmentRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DeleteAssessmentRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAssessmentRun where
  toJSON DeleteAssessmentRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("assessmentRunArn" Lude..= assessmentRunARN)]
      )

instance Lude.ToPath DeleteAssessmentRun where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAssessmentRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAssessmentRunResponse' smart constructor.
data DeleteAssessmentRunResponse = DeleteAssessmentRunResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAssessmentRunResponse' with the minimum fields required to make a request.
mkDeleteAssessmentRunResponse ::
  DeleteAssessmentRunResponse
mkDeleteAssessmentRunResponse = DeleteAssessmentRunResponse'
