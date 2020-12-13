{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.CreateCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new capacity provider. Capacity providers are associated with an Amazon ECS cluster and are used in capacity provider strategies to facilitate cluster auto scaling.
--
-- Only capacity providers using an Auto Scaling group can be created. Amazon ECS tasks on AWS Fargate use the @FARGATE@ and @FARGATE_SPOT@ capacity providers which are already created and available to all accounts in Regions supported by AWS Fargate.
module Network.AWS.ECS.CreateCapacityProvider
  ( -- * Creating a request
    CreateCapacityProvider (..),
    mkCreateCapacityProvider,

    -- ** Request lenses
    ccpAutoScalingGroupProvider,
    ccpName,
    ccpTags,

    -- * Destructuring the response
    CreateCapacityProviderResponse (..),
    mkCreateCapacityProviderResponse,

    -- ** Response lenses
    ccprsCapacityProvider,
    ccprsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCapacityProvider' smart constructor.
data CreateCapacityProvider = CreateCapacityProvider'
  { -- | The details of the Auto Scaling group for the capacity provider.
    autoScalingGroupProvider :: AutoScalingGroupProvider,
    -- | The name of the capacity provider. Up to 255 characters are allowed, including letters (upper and lowercase), numbers, underscores, and hyphens. The name cannot be prefixed with "@aws@ ", "@ecs@ ", or "@fargate@ ".
    name :: Lude.Text,
    -- | The metadata that you apply to the capacity provider to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per resource - 50
    --
    --
    --     * For each resource, each tag key must be unique, and each tag key can have only one value.
    --
    --
    --     * Maximum key length - 128 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length - 256 Unicode characters in UTF-8
    --
    --
    --     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
    --
    --
    --     * Tag keys and values are case-sensitive.
    --
    --
    --     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCapacityProvider' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupProvider' - The details of the Auto Scaling group for the capacity provider.
-- * 'name' - The name of the capacity provider. Up to 255 characters are allowed, including letters (upper and lowercase), numbers, underscores, and hyphens. The name cannot be prefixed with "@aws@ ", "@ecs@ ", or "@fargate@ ".
-- * 'tags' - The metadata that you apply to the capacity provider to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
mkCreateCapacityProvider ::
  -- | 'autoScalingGroupProvider'
  AutoScalingGroupProvider ->
  -- | 'name'
  Lude.Text ->
  CreateCapacityProvider
mkCreateCapacityProvider pAutoScalingGroupProvider_ pName_ =
  CreateCapacityProvider'
    { autoScalingGroupProvider =
        pAutoScalingGroupProvider_,
      name = pName_,
      tags = Lude.Nothing
    }

-- | The details of the Auto Scaling group for the capacity provider.
--
-- /Note:/ Consider using 'autoScalingGroupProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpAutoScalingGroupProvider :: Lens.Lens' CreateCapacityProvider AutoScalingGroupProvider
ccpAutoScalingGroupProvider = Lens.lens (autoScalingGroupProvider :: CreateCapacityProvider -> AutoScalingGroupProvider) (\s a -> s {autoScalingGroupProvider = a} :: CreateCapacityProvider)
{-# DEPRECATED ccpAutoScalingGroupProvider "Use generic-lens or generic-optics with 'autoScalingGroupProvider' instead." #-}

-- | The name of the capacity provider. Up to 255 characters are allowed, including letters (upper and lowercase), numbers, underscores, and hyphens. The name cannot be prefixed with "@aws@ ", "@ecs@ ", or "@fargate@ ".
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpName :: Lens.Lens' CreateCapacityProvider Lude.Text
ccpName = Lens.lens (name :: CreateCapacityProvider -> Lude.Text) (\s a -> s {name = a} :: CreateCapacityProvider)
{-# DEPRECATED ccpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The metadata that you apply to the capacity provider to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpTags :: Lens.Lens' CreateCapacityProvider (Lude.Maybe [Tag])
ccpTags = Lens.lens (tags :: CreateCapacityProvider -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCapacityProvider)
{-# DEPRECATED ccpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateCapacityProvider where
  type Rs CreateCapacityProvider = CreateCapacityProviderResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCapacityProviderResponse'
            Lude.<$> (x Lude..?> "capacityProvider")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCapacityProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.CreateCapacityProvider" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCapacityProvider where
  toJSON CreateCapacityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("autoScalingGroupProvider" Lude..= autoScalingGroupProvider),
            Lude.Just ("name" Lude..= name),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateCapacityProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCapacityProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCapacityProviderResponse' smart constructor.
data CreateCapacityProviderResponse = CreateCapacityProviderResponse'
  { -- | The full description of the new capacity provider.
    capacityProvider :: Lude.Maybe CapacityProvider,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCapacityProviderResponse' with the minimum fields required to make a request.
--
-- * 'capacityProvider' - The full description of the new capacity provider.
-- * 'responseStatus' - The response status code.
mkCreateCapacityProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCapacityProviderResponse
mkCreateCapacityProviderResponse pResponseStatus_ =
  CreateCapacityProviderResponse'
    { capacityProvider = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full description of the new capacity provider.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsCapacityProvider :: Lens.Lens' CreateCapacityProviderResponse (Lude.Maybe CapacityProvider)
ccprsCapacityProvider = Lens.lens (capacityProvider :: CreateCapacityProviderResponse -> Lude.Maybe CapacityProvider) (\s a -> s {capacityProvider = a} :: CreateCapacityProviderResponse)
{-# DEPRECATED ccprsCapacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsResponseStatus :: Lens.Lens' CreateCapacityProviderResponse Lude.Int
ccprsResponseStatus = Lens.lens (responseStatus :: CreateCapacityProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCapacityProviderResponse)
{-# DEPRECATED ccprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
