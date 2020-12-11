{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.DeleteScalingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling plan.
--
-- Deleting a scaling plan deletes the underlying 'ScalingInstruction' for all of the scalable resources that are covered by the plan.
-- If the plan has launched resources or has scaling activities in progress, you must delete those resources separately.
module Network.AWS.AutoScalingPlans.DeleteScalingPlan
  ( -- * Creating a request
    DeleteScalingPlan (..),
    mkDeleteScalingPlan,

    -- ** Request lenses
    dspScalingPlanName,
    dspScalingPlanVersion,

    -- * Destructuring the response
    DeleteScalingPlanResponse (..),
    mkDeleteScalingPlanResponse,

    -- ** Response lenses
    dsprsResponseStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteScalingPlan' smart constructor.
data DeleteScalingPlan = DeleteScalingPlan'
  { scalingPlanName ::
      Lude.Text,
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

-- | Creates a value of 'DeleteScalingPlan' with the minimum fields required to make a request.
--
-- * 'scalingPlanName' - The name of the scaling plan.
-- * 'scalingPlanVersion' - The version number of the scaling plan.
mkDeleteScalingPlan ::
  -- | 'scalingPlanName'
  Lude.Text ->
  -- | 'scalingPlanVersion'
  Lude.Integer ->
  DeleteScalingPlan
mkDeleteScalingPlan pScalingPlanName_ pScalingPlanVersion_ =
  DeleteScalingPlan'
    { scalingPlanName = pScalingPlanName_,
      scalingPlanVersion = pScalingPlanVersion_
    }

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspScalingPlanName :: Lens.Lens' DeleteScalingPlan Lude.Text
dspScalingPlanName = Lens.lens (scalingPlanName :: DeleteScalingPlan -> Lude.Text) (\s a -> s {scalingPlanName = a} :: DeleteScalingPlan)
{-# DEPRECATED dspScalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead." #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspScalingPlanVersion :: Lens.Lens' DeleteScalingPlan Lude.Integer
dspScalingPlanVersion = Lens.lens (scalingPlanVersion :: DeleteScalingPlan -> Lude.Integer) (\s a -> s {scalingPlanVersion = a} :: DeleteScalingPlan)
{-# DEPRECATED dspScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

instance Lude.AWSRequest DeleteScalingPlan where
  type Rs DeleteScalingPlan = DeleteScalingPlanResponse
  request = Req.postJSON autoScalingPlansService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteScalingPlanResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteScalingPlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AnyScaleScalingPlannerFrontendService.DeleteScalingPlan" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteScalingPlan where
  toJSON DeleteScalingPlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ScalingPlanName" Lude..= scalingPlanName),
            Lude.Just ("ScalingPlanVersion" Lude..= scalingPlanVersion)
          ]
      )

instance Lude.ToPath DeleteScalingPlan where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteScalingPlan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteScalingPlanResponse' smart constructor.
newtype DeleteScalingPlanResponse = DeleteScalingPlanResponse'
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

-- | Creates a value of 'DeleteScalingPlanResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteScalingPlanResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteScalingPlanResponse
mkDeleteScalingPlanResponse pResponseStatus_ =
  DeleteScalingPlanResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsResponseStatus :: Lens.Lens' DeleteScalingPlanResponse Lude.Int
dsprsResponseStatus = Lens.lens (responseStatus :: DeleteScalingPlanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteScalingPlanResponse)
{-# DEPRECATED dsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
