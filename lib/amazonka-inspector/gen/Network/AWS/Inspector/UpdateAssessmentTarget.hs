{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.UpdateAssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the assessment target that is specified by the ARN of the assessment target.
--
-- If resourceGroupArn is not specified, all EC2 instances in the current AWS account and region are included in the assessment target.
module Network.AWS.Inspector.UpdateAssessmentTarget
  ( -- * Creating a request
    UpdateAssessmentTarget (..),
    mkUpdateAssessmentTarget,

    -- ** Request lenses
    uatResourceGroupARN,
    uatAssessmentTargetARN,
    uatAssessmentTargetName,

    -- * Destructuring the response
    UpdateAssessmentTargetResponse (..),
    mkUpdateAssessmentTargetResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAssessmentTarget' smart constructor.
data UpdateAssessmentTarget = UpdateAssessmentTarget'
  { resourceGroupARN ::
      Lude.Maybe Lude.Text,
    assessmentTargetARN :: Lude.Text,
    assessmentTargetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAssessmentTarget' with the minimum fields required to make a request.
--
-- * 'assessmentTargetARN' - The ARN of the assessment target that you want to update.
-- * 'assessmentTargetName' - The name of the assessment target that you want to update.
-- * 'resourceGroupARN' - The ARN of the resource group that is used to specify the new resource group to associate with the assessment target.
mkUpdateAssessmentTarget ::
  -- | 'assessmentTargetARN'
  Lude.Text ->
  -- | 'assessmentTargetName'
  Lude.Text ->
  UpdateAssessmentTarget
mkUpdateAssessmentTarget
  pAssessmentTargetARN_
  pAssessmentTargetName_ =
    UpdateAssessmentTarget'
      { resourceGroupARN = Lude.Nothing,
        assessmentTargetARN = pAssessmentTargetARN_,
        assessmentTargetName = pAssessmentTargetName_
      }

-- | The ARN of the resource group that is used to specify the new resource group to associate with the assessment target.
--
-- /Note:/ Consider using 'resourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatResourceGroupARN :: Lens.Lens' UpdateAssessmentTarget (Lude.Maybe Lude.Text)
uatResourceGroupARN = Lens.lens (resourceGroupARN :: UpdateAssessmentTarget -> Lude.Maybe Lude.Text) (\s a -> s {resourceGroupARN = a} :: UpdateAssessmentTarget)
{-# DEPRECATED uatResourceGroupARN "Use generic-lens or generic-optics with 'resourceGroupARN' instead." #-}

-- | The ARN of the assessment target that you want to update.
--
-- /Note:/ Consider using 'assessmentTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatAssessmentTargetARN :: Lens.Lens' UpdateAssessmentTarget Lude.Text
uatAssessmentTargetARN = Lens.lens (assessmentTargetARN :: UpdateAssessmentTarget -> Lude.Text) (\s a -> s {assessmentTargetARN = a} :: UpdateAssessmentTarget)
{-# DEPRECATED uatAssessmentTargetARN "Use generic-lens or generic-optics with 'assessmentTargetARN' instead." #-}

-- | The name of the assessment target that you want to update.
--
-- /Note:/ Consider using 'assessmentTargetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatAssessmentTargetName :: Lens.Lens' UpdateAssessmentTarget Lude.Text
uatAssessmentTargetName = Lens.lens (assessmentTargetName :: UpdateAssessmentTarget -> Lude.Text) (\s a -> s {assessmentTargetName = a} :: UpdateAssessmentTarget)
{-# DEPRECATED uatAssessmentTargetName "Use generic-lens or generic-optics with 'assessmentTargetName' instead." #-}

instance Lude.AWSRequest UpdateAssessmentTarget where
  type Rs UpdateAssessmentTarget = UpdateAssessmentTargetResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull UpdateAssessmentTargetResponse'

instance Lude.ToHeaders UpdateAssessmentTarget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.UpdateAssessmentTarget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAssessmentTarget where
  toJSON UpdateAssessmentTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("resourceGroupArn" Lude..=) Lude.<$> resourceGroupARN,
            Lude.Just ("assessmentTargetArn" Lude..= assessmentTargetARN),
            Lude.Just ("assessmentTargetName" Lude..= assessmentTargetName)
          ]
      )

instance Lude.ToPath UpdateAssessmentTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAssessmentTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAssessmentTargetResponse' smart constructor.
data UpdateAssessmentTargetResponse = UpdateAssessmentTargetResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAssessmentTargetResponse' with the minimum fields required to make a request.
mkUpdateAssessmentTargetResponse ::
  UpdateAssessmentTargetResponse
mkUpdateAssessmentTargetResponse = UpdateAssessmentTargetResponse'
