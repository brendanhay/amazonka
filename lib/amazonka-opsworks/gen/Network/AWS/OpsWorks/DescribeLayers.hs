{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeLayers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more layers in a specified stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeLayers
  ( -- * Creating a request
    DescribeLayers (..),
    mkDescribeLayers,

    -- ** Request lenses
    dlLayerIds,
    dlStackId,

    -- * Destructuring the response
    DescribeLayersResponse (..),
    mkDescribeLayersResponse,

    -- ** Response lenses
    dlrsLayers,
    dlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLayers' smart constructor.
data DescribeLayers = DescribeLayers'
  { -- | An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
    layerIds :: Lude.Maybe [Lude.Text],
    -- | The stack ID.
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLayers' with the minimum fields required to make a request.
--
-- * 'layerIds' - An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
-- * 'stackId' - The stack ID.
mkDescribeLayers ::
  DescribeLayers
mkDescribeLayers =
  DescribeLayers' {layerIds = Lude.Nothing, stackId = Lude.Nothing}

-- | An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLayerIds :: Lens.Lens' DescribeLayers (Lude.Maybe [Lude.Text])
dlLayerIds = Lens.lens (layerIds :: DescribeLayers -> Lude.Maybe [Lude.Text]) (\s a -> s {layerIds = a} :: DescribeLayers)
{-# DEPRECATED dlLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlStackId :: Lens.Lens' DescribeLayers (Lude.Maybe Lude.Text)
dlStackId = Lens.lens (stackId :: DescribeLayers -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeLayers)
{-# DEPRECATED dlStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeLayers where
  type Rs DescribeLayers = DescribeLayersResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLayersResponse'
            Lude.<$> (x Lude..?> "Layers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLayers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeLayers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLayers where
  toJSON DescribeLayers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LayerIds" Lude..=) Lude.<$> layerIds,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeLayers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLayers where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeLayers@ request.
--
-- /See:/ 'mkDescribeLayersResponse' smart constructor.
data DescribeLayersResponse = DescribeLayersResponse'
  { -- | An array of @Layer@ objects that describe the layers.
    layers :: Lude.Maybe [Layer],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLayersResponse' with the minimum fields required to make a request.
--
-- * 'layers' - An array of @Layer@ objects that describe the layers.
-- * 'responseStatus' - The response status code.
mkDescribeLayersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLayersResponse
mkDescribeLayersResponse pResponseStatus_ =
  DescribeLayersResponse'
    { layers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Layer@ objects that describe the layers.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsLayers :: Lens.Lens' DescribeLayersResponse (Lude.Maybe [Layer])
dlrsLayers = Lens.lens (layers :: DescribeLayersResponse -> Lude.Maybe [Layer]) (\s a -> s {layers = a} :: DescribeLayersResponse)
{-# DEPRECATED dlrsLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DescribeLayersResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DescribeLayersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLayersResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
