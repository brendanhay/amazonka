{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.UpdateScalingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified scaling plan.
--
-- You cannot update a scaling plan if it is in the process of being created, updated, or deleted.
module Network.AWS.AutoScalingPlans.UpdateScalingPlan
  ( -- * Creating a request
    UpdateScalingPlan (..),
    mkUpdateScalingPlan,

    -- ** Request lenses
    uspScalingInstructions,
    uspApplicationSource,
    uspScalingPlanName,
    uspScalingPlanVersion,

    -- * Destructuring the response
    UpdateScalingPlanResponse (..),
    mkUpdateScalingPlanResponse,

    -- ** Response lenses
    usprsResponseStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateScalingPlan' smart constructor.
data UpdateScalingPlan = UpdateScalingPlan'
  { scalingInstructions ::
      Lude.Maybe [ScalingInstruction],
    applicationSource :: Lude.Maybe ApplicationSource,
    scalingPlanName :: Lude.Text,
    scalingPlanVersion :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScalingPlan' with the minimum fields required to make a request.
--
-- * 'applicationSource' - A CloudFormation stack or set of tags.
-- * 'scalingInstructions' - The scaling instructions.
-- * 'scalingPlanName' - The name of the scaling plan.
-- * 'scalingPlanVersion' - The version number of the scaling plan.
mkUpdateScalingPlan ::
  -- | 'scalingPlanName'
  Lude.Text ->
  -- | 'scalingPlanVersion'
  Lude.Integer ->
  UpdateScalingPlan
mkUpdateScalingPlan pScalingPlanName_ pScalingPlanVersion_ =
  UpdateScalingPlan'
    { scalingInstructions = Lude.Nothing,
      applicationSource = Lude.Nothing,
      scalingPlanName = pScalingPlanName_,
      scalingPlanVersion = pScalingPlanVersion_
    }

-- | The scaling instructions.
--
-- /Note:/ Consider using 'scalingInstructions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspScalingInstructions :: Lens.Lens' UpdateScalingPlan (Lude.Maybe [ScalingInstruction])
uspScalingInstructions = Lens.lens (scalingInstructions :: UpdateScalingPlan -> Lude.Maybe [ScalingInstruction]) (\s a -> s {scalingInstructions = a} :: UpdateScalingPlan)
{-# DEPRECATED uspScalingInstructions "Use generic-lens or generic-optics with 'scalingInstructions' instead." #-}

-- | A CloudFormation stack or set of tags.
--
-- /Note:/ Consider using 'applicationSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspApplicationSource :: Lens.Lens' UpdateScalingPlan (Lude.Maybe ApplicationSource)
uspApplicationSource = Lens.lens (applicationSource :: UpdateScalingPlan -> Lude.Maybe ApplicationSource) (\s a -> s {applicationSource = a} :: UpdateScalingPlan)
{-# DEPRECATED uspApplicationSource "Use generic-lens or generic-optics with 'applicationSource' instead." #-}

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspScalingPlanName :: Lens.Lens' UpdateScalingPlan Lude.Text
uspScalingPlanName = Lens.lens (scalingPlanName :: UpdateScalingPlan -> Lude.Text) (\s a -> s {scalingPlanName = a} :: UpdateScalingPlan)
{-# DEPRECATED uspScalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead." #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspScalingPlanVersion :: Lens.Lens' UpdateScalingPlan Lude.Integer
uspScalingPlanVersion = Lens.lens (scalingPlanVersion :: UpdateScalingPlan -> Lude.Integer) (\s a -> s {scalingPlanVersion = a} :: UpdateScalingPlan)
{-# DEPRECATED uspScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

instance Lude.AWSRequest UpdateScalingPlan where
  type Rs UpdateScalingPlan = UpdateScalingPlanResponse
  request = Req.postJSON autoScalingPlansService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateScalingPlanResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateScalingPlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AnyScaleScalingPlannerFrontendService.UpdateScalingPlan" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateScalingPlan where
  toJSON UpdateScalingPlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ScalingInstructions" Lude..=) Lude.<$> scalingInstructions,
            ("ApplicationSource" Lude..=) Lude.<$> applicationSource,
            Lude.Just ("ScalingPlanName" Lude..= scalingPlanName),
            Lude.Just ("ScalingPlanVersion" Lude..= scalingPlanVersion)
          ]
      )

instance Lude.ToPath UpdateScalingPlan where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateScalingPlan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateScalingPlanResponse' smart constructor.
newtype UpdateScalingPlanResponse = UpdateScalingPlanResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScalingPlanResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateScalingPlanResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateScalingPlanResponse
mkUpdateScalingPlanResponse pResponseStatus_ =
  UpdateScalingPlanResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsResponseStatus :: Lens.Lens' UpdateScalingPlanResponse Lude.Int
usprsResponseStatus = Lens.lens (responseStatus :: UpdateScalingPlanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateScalingPlanResponse)
{-# DEPRECATED usprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
