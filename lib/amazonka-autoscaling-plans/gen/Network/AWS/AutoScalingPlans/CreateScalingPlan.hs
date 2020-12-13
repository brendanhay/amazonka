{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.CreateScalingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scaling plan.
module Network.AWS.AutoScalingPlans.CreateScalingPlan
  ( -- * Creating a request
    CreateScalingPlan (..),
    mkCreateScalingPlan,

    -- ** Request lenses
    cspScalingInstructions,
    cspScalingPlanName,
    cspApplicationSource,

    -- * Destructuring the response
    CreateScalingPlanResponse (..),
    mkCreateScalingPlanResponse,

    -- ** Response lenses
    csprsScalingPlanVersion,
    csprsResponseStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateScalingPlan' smart constructor.
data CreateScalingPlan = CreateScalingPlan'
  { -- | The scaling instructions.
    scalingInstructions :: [ScalingInstruction],
    -- | The name of the scaling plan. Names cannot contain vertical bars, colons, or forward slashes.
    scalingPlanName :: Lude.Text,
    -- | A CloudFormation stack or set of tags. You can create one scaling plan per application source.
    applicationSource :: ApplicationSource
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScalingPlan' with the minimum fields required to make a request.
--
-- * 'scalingInstructions' - The scaling instructions.
-- * 'scalingPlanName' - The name of the scaling plan. Names cannot contain vertical bars, colons, or forward slashes.
-- * 'applicationSource' - A CloudFormation stack or set of tags. You can create one scaling plan per application source.
mkCreateScalingPlan ::
  -- | 'scalingPlanName'
  Lude.Text ->
  -- | 'applicationSource'
  ApplicationSource ->
  CreateScalingPlan
mkCreateScalingPlan pScalingPlanName_ pApplicationSource_ =
  CreateScalingPlan'
    { scalingInstructions = Lude.mempty,
      scalingPlanName = pScalingPlanName_,
      applicationSource = pApplicationSource_
    }

-- | The scaling instructions.
--
-- /Note:/ Consider using 'scalingInstructions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspScalingInstructions :: Lens.Lens' CreateScalingPlan [ScalingInstruction]
cspScalingInstructions = Lens.lens (scalingInstructions :: CreateScalingPlan -> [ScalingInstruction]) (\s a -> s {scalingInstructions = a} :: CreateScalingPlan)
{-# DEPRECATED cspScalingInstructions "Use generic-lens or generic-optics with 'scalingInstructions' instead." #-}

-- | The name of the scaling plan. Names cannot contain vertical bars, colons, or forward slashes.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspScalingPlanName :: Lens.Lens' CreateScalingPlan Lude.Text
cspScalingPlanName = Lens.lens (scalingPlanName :: CreateScalingPlan -> Lude.Text) (\s a -> s {scalingPlanName = a} :: CreateScalingPlan)
{-# DEPRECATED cspScalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead." #-}

-- | A CloudFormation stack or set of tags. You can create one scaling plan per application source.
--
-- /Note:/ Consider using 'applicationSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspApplicationSource :: Lens.Lens' CreateScalingPlan ApplicationSource
cspApplicationSource = Lens.lens (applicationSource :: CreateScalingPlan -> ApplicationSource) (\s a -> s {applicationSource = a} :: CreateScalingPlan)
{-# DEPRECATED cspApplicationSource "Use generic-lens or generic-optics with 'applicationSource' instead." #-}

instance Lude.AWSRequest CreateScalingPlan where
  type Rs CreateScalingPlan = CreateScalingPlanResponse
  request = Req.postJSON autoScalingPlansService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateScalingPlanResponse'
            Lude.<$> (x Lude..:> "ScalingPlanVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateScalingPlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AnyScaleScalingPlannerFrontendService.CreateScalingPlan" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateScalingPlan where
  toJSON CreateScalingPlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ScalingInstructions" Lude..= scalingInstructions),
            Lude.Just ("ScalingPlanName" Lude..= scalingPlanName),
            Lude.Just ("ApplicationSource" Lude..= applicationSource)
          ]
      )

instance Lude.ToPath CreateScalingPlan where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateScalingPlan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateScalingPlanResponse' smart constructor.
data CreateScalingPlanResponse = CreateScalingPlanResponse'
  { -- | The version number of the scaling plan. This value is always 1.
    --
    -- Currently, you cannot specify multiple scaling plan versions.
    scalingPlanVersion :: Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScalingPlanResponse' with the minimum fields required to make a request.
--
-- * 'scalingPlanVersion' - The version number of the scaling plan. This value is always 1.
--
-- Currently, you cannot specify multiple scaling plan versions.
-- * 'responseStatus' - The response status code.
mkCreateScalingPlanResponse ::
  -- | 'scalingPlanVersion'
  Lude.Integer ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateScalingPlanResponse
mkCreateScalingPlanResponse pScalingPlanVersion_ pResponseStatus_ =
  CreateScalingPlanResponse'
    { scalingPlanVersion =
        pScalingPlanVersion_,
      responseStatus = pResponseStatus_
    }

-- | The version number of the scaling plan. This value is always 1.
--
-- Currently, you cannot specify multiple scaling plan versions.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprsScalingPlanVersion :: Lens.Lens' CreateScalingPlanResponse Lude.Integer
csprsScalingPlanVersion = Lens.lens (scalingPlanVersion :: CreateScalingPlanResponse -> Lude.Integer) (\s a -> s {scalingPlanVersion = a} :: CreateScalingPlanResponse)
{-# DEPRECATED csprsScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprsResponseStatus :: Lens.Lens' CreateScalingPlanResponse Lude.Int
csprsResponseStatus = Lens.lens (responseStatus :: CreateScalingPlanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateScalingPlanResponse)
{-# DEPRECATED csprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
