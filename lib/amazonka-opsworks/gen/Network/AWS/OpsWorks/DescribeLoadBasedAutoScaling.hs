{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes load-based auto scaling configurations for specified layers.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
  ( -- * Creating a request
    DescribeLoadBasedAutoScaling (..),
    mkDescribeLoadBasedAutoScaling,

    -- ** Request lenses
    dlbasLayerIds,

    -- * Destructuring the response
    DescribeLoadBasedAutoScalingResponse (..),
    mkDescribeLoadBasedAutoScalingResponse,

    -- ** Response lenses
    dlbasrsLoadBasedAutoScalingConfigurations,
    dlbasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLoadBasedAutoScaling' smart constructor.
newtype DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling'
  { -- | An array of layer IDs.
    layerIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBasedAutoScaling' with the minimum fields required to make a request.
--
-- * 'layerIds' - An array of layer IDs.
mkDescribeLoadBasedAutoScaling ::
  DescribeLoadBasedAutoScaling
mkDescribeLoadBasedAutoScaling =
  DescribeLoadBasedAutoScaling' {layerIds = Lude.mempty}

-- | An array of layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbasLayerIds :: Lens.Lens' DescribeLoadBasedAutoScaling [Lude.Text]
dlbasLayerIds = Lens.lens (layerIds :: DescribeLoadBasedAutoScaling -> [Lude.Text]) (\s a -> s {layerIds = a} :: DescribeLoadBasedAutoScaling)
{-# DEPRECATED dlbasLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

instance Lude.AWSRequest DescribeLoadBasedAutoScaling where
  type
    Rs DescribeLoadBasedAutoScaling =
      DescribeLoadBasedAutoScalingResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLoadBasedAutoScalingResponse'
            Lude.<$> ( x Lude..?> "LoadBasedAutoScalingConfigurations"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoadBasedAutoScaling where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorks_20130218.DescribeLoadBasedAutoScaling" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLoadBasedAutoScaling where
  toJSON DescribeLoadBasedAutoScaling' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("LayerIds" Lude..= layerIds)])

instance Lude.ToPath DescribeLoadBasedAutoScaling where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoadBasedAutoScaling where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeLoadBasedAutoScaling@ request.
--
-- /See:/ 'mkDescribeLoadBasedAutoScalingResponse' smart constructor.
data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse'
  { -- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe each layer's configuration.
    loadBasedAutoScalingConfigurations :: Lude.Maybe [LoadBasedAutoScalingConfiguration],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBasedAutoScalingResponse' with the minimum fields required to make a request.
--
-- * 'loadBasedAutoScalingConfigurations' - An array of @LoadBasedAutoScalingConfiguration@ objects that describe each layer's configuration.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBasedAutoScalingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBasedAutoScalingResponse
mkDescribeLoadBasedAutoScalingResponse pResponseStatus_ =
  DescribeLoadBasedAutoScalingResponse'
    { loadBasedAutoScalingConfigurations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe each layer's configuration.
--
-- /Note:/ Consider using 'loadBasedAutoScalingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbasrsLoadBasedAutoScalingConfigurations :: Lens.Lens' DescribeLoadBasedAutoScalingResponse (Lude.Maybe [LoadBasedAutoScalingConfiguration])
dlbasrsLoadBasedAutoScalingConfigurations = Lens.lens (loadBasedAutoScalingConfigurations :: DescribeLoadBasedAutoScalingResponse -> Lude.Maybe [LoadBasedAutoScalingConfiguration]) (\s a -> s {loadBasedAutoScalingConfigurations = a} :: DescribeLoadBasedAutoScalingResponse)
{-# DEPRECATED dlbasrsLoadBasedAutoScalingConfigurations "Use generic-lens or generic-optics with 'loadBasedAutoScalingConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbasrsResponseStatus :: Lens.Lens' DescribeLoadBasedAutoScalingResponse Lude.Int
dlbasrsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBasedAutoScalingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBasedAutoScalingResponse)
{-# DEPRECATED dlbasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
