{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.CreateResourceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group using the specified set of tags (key and value pairs) that are used to select the EC2 instances to be included in an Amazon Inspector assessment target. The created resource group is then used to create an Amazon Inspector assessment target. For more information, see 'CreateAssessmentTarget' .
module Network.AWS.Inspector.CreateResourceGroup
  ( -- * Creating a request
    CreateResourceGroup (..),
    mkCreateResourceGroup,

    -- ** Request lenses
    crgResourceGroupTags,

    -- * Destructuring the response
    CreateResourceGroupResponse (..),
    mkCreateResourceGroupResponse,

    -- ** Response lenses
    crgrsResponseStatus,
    crgrsResourceGroupARN,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateResourceGroup' smart constructor.
newtype CreateResourceGroup = CreateResourceGroup'
  { resourceGroupTags ::
      Lude.NonEmpty ResourceGroupTag
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceGroup' with the minimum fields required to make a request.
--
-- * 'resourceGroupTags' - A collection of keys and an array of possible values, '[{"key":"key1","values":["Value1","Value2"]},{"key":"Key2","values":["Value3"]}]'.
--
-- For example,'[{"key":"Name","values":["TestEC2Instance"]}]'.
mkCreateResourceGroup ::
  -- | 'resourceGroupTags'
  Lude.NonEmpty ResourceGroupTag ->
  CreateResourceGroup
mkCreateResourceGroup pResourceGroupTags_ =
  CreateResourceGroup' {resourceGroupTags = pResourceGroupTags_}

-- | A collection of keys and an array of possible values, '[{"key":"key1","values":["Value1","Value2"]},{"key":"Key2","values":["Value3"]}]'.
--
-- For example,'[{"key":"Name","values":["TestEC2Instance"]}]'.
--
-- /Note:/ Consider using 'resourceGroupTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgResourceGroupTags :: Lens.Lens' CreateResourceGroup (Lude.NonEmpty ResourceGroupTag)
crgResourceGroupTags = Lens.lens (resourceGroupTags :: CreateResourceGroup -> Lude.NonEmpty ResourceGroupTag) (\s a -> s {resourceGroupTags = a} :: CreateResourceGroup)
{-# DEPRECATED crgResourceGroupTags "Use generic-lens or generic-optics with 'resourceGroupTags' instead." #-}

instance Lude.AWSRequest CreateResourceGroup where
  type Rs CreateResourceGroup = CreateResourceGroupResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateResourceGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "resourceGroupArn")
      )

instance Lude.ToHeaders CreateResourceGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.CreateResourceGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateResourceGroup where
  toJSON CreateResourceGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("resourceGroupTags" Lude..= resourceGroupTags)]
      )

instance Lude.ToPath CreateResourceGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateResourceGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateResourceGroupResponse' smart constructor.
data CreateResourceGroupResponse = CreateResourceGroupResponse'
  { responseStatus ::
      Lude.Int,
    resourceGroupARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceGroupResponse' with the minimum fields required to make a request.
--
-- * 'resourceGroupARN' - The ARN that specifies the resource group that is created.
-- * 'responseStatus' - The response status code.
mkCreateResourceGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'resourceGroupARN'
  Lude.Text ->
  CreateResourceGroupResponse
mkCreateResourceGroupResponse pResponseStatus_ pResourceGroupARN_ =
  CreateResourceGroupResponse'
    { responseStatus = pResponseStatus_,
      resourceGroupARN = pResourceGroupARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsResponseStatus :: Lens.Lens' CreateResourceGroupResponse Lude.Int
crgrsResponseStatus = Lens.lens (responseStatus :: CreateResourceGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateResourceGroupResponse)
{-# DEPRECATED crgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ARN that specifies the resource group that is created.
--
-- /Note:/ Consider using 'resourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsResourceGroupARN :: Lens.Lens' CreateResourceGroupResponse Lude.Text
crgrsResourceGroupARN = Lens.lens (resourceGroupARN :: CreateResourceGroupResponse -> Lude.Text) (\s a -> s {resourceGroupARN = a} :: CreateResourceGroupResponse)
{-# DEPRECATED crgrsResourceGroupARN "Use generic-lens or generic-optics with 'resourceGroupARN' instead." #-}
