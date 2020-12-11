{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTaskDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a task definition. You can specify a @family@ and @revision@ to find information about a specific task definition, or you can simply specify the family to find the latest @ACTIVE@ revision in that family.
module Network.AWS.ECS.DescribeTaskDefinition
  ( -- * Creating a request
    DescribeTaskDefinition (..),
    mkDescribeTaskDefinition,

    -- ** Request lenses
    dtdInclude,
    dtdTaskDefinition,

    -- * Destructuring the response
    DescribeTaskDefinitionResponse (..),
    mkDescribeTaskDefinitionResponse,

    -- ** Response lenses
    desrsTaskDefinition,
    desrsTags,
    desrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTaskDefinition' smart constructor.
data DescribeTaskDefinition = DescribeTaskDefinition'
  { include ::
      Lude.Maybe [TaskDefinitionField],
    taskDefinition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTaskDefinition' with the minimum fields required to make a request.
--
-- * 'include' - Specifies whether to see the resource tags for the task definition. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
-- * 'taskDefinition' - The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@ (@family:revision@ ) for a specific revision in the family, or full Amazon Resource Name (ARN) of the task definition to describe.
mkDescribeTaskDefinition ::
  -- | 'taskDefinition'
  Lude.Text ->
  DescribeTaskDefinition
mkDescribeTaskDefinition pTaskDefinition_ =
  DescribeTaskDefinition'
    { include = Lude.Nothing,
      taskDefinition = pTaskDefinition_
    }

-- | Specifies whether to see the resource tags for the task definition. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdInclude :: Lens.Lens' DescribeTaskDefinition (Lude.Maybe [TaskDefinitionField])
dtdInclude = Lens.lens (include :: DescribeTaskDefinition -> Lude.Maybe [TaskDefinitionField]) (\s a -> s {include = a} :: DescribeTaskDefinition)
{-# DEPRECATED dtdInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@ (@family:revision@ ) for a specific revision in the family, or full Amazon Resource Name (ARN) of the task definition to describe.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdTaskDefinition :: Lens.Lens' DescribeTaskDefinition Lude.Text
dtdTaskDefinition = Lens.lens (taskDefinition :: DescribeTaskDefinition -> Lude.Text) (\s a -> s {taskDefinition = a} :: DescribeTaskDefinition)
{-# DEPRECATED dtdTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

instance Lude.AWSRequest DescribeTaskDefinition where
  type Rs DescribeTaskDefinition = DescribeTaskDefinitionResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTaskDefinitionResponse'
            Lude.<$> (x Lude..?> "taskDefinition")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTaskDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTaskDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTaskDefinition where
  toJSON DescribeTaskDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("include" Lude..=) Lude.<$> include,
            Lude.Just ("taskDefinition" Lude..= taskDefinition)
          ]
      )

instance Lude.ToPath DescribeTaskDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTaskDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTaskDefinitionResponse' smart constructor.
data DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'
  { taskDefinition ::
      Lude.Maybe TaskDefinition,
    tags :: Lude.Maybe [Tag],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTaskDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tags' - The metadata that is applied to the task definition to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
-- * 'taskDefinition' - The full task definition description.
mkDescribeTaskDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTaskDefinitionResponse
mkDescribeTaskDefinitionResponse pResponseStatus_ =
  DescribeTaskDefinitionResponse'
    { taskDefinition = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full task definition description.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsTaskDefinition :: Lens.Lens' DescribeTaskDefinitionResponse (Lude.Maybe TaskDefinition)
desrsTaskDefinition = Lens.lens (taskDefinition :: DescribeTaskDefinitionResponse -> Lude.Maybe TaskDefinition) (\s a -> s {taskDefinition = a} :: DescribeTaskDefinitionResponse)
{-# DEPRECATED desrsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The metadata that is applied to the task definition to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
desrsTags :: Lens.Lens' DescribeTaskDefinitionResponse (Lude.Maybe [Tag])
desrsTags = Lens.lens (tags :: DescribeTaskDefinitionResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeTaskDefinitionResponse)
{-# DEPRECATED desrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeTaskDefinitionResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeTaskDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTaskDefinitionResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
