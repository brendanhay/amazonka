{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new workflow.
module Network.AWS.Glue.CreateWorkflow
  ( -- * Creating a request
    CreateWorkflow (..),
    mkCreateWorkflow,

    -- ** Request lenses
    cwMaxConcurrentRuns,
    cwDefaultRunProperties,
    cwDescription,
    cwTags,
    cwName,

    -- * Destructuring the response
    CreateWorkflowResponse (..),
    mkCreateWorkflowResponse,

    -- ** Response lenses
    cwrsName,
    cwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateWorkflow' smart constructor.
data CreateWorkflow = CreateWorkflow'
  { maxConcurrentRuns ::
      Lude.Maybe Lude.Int,
    defaultRunProperties ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkflow' with the minimum fields required to make a request.
--
-- * 'defaultRunProperties' - A collection of properties to be used as part of each execution of the workflow.
-- * 'description' - A description of the workflow.
-- * 'maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
-- * 'name' - The name to be assigned to the workflow. It should be unique within your account.
-- * 'tags' - The tags to be used with this workflow.
mkCreateWorkflow ::
  -- | 'name'
  Lude.Text ->
  CreateWorkflow
mkCreateWorkflow pName_ =
  CreateWorkflow'
    { maxConcurrentRuns = Lude.Nothing,
      defaultRunProperties = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- /Note:/ Consider using 'maxConcurrentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwMaxConcurrentRuns :: Lens.Lens' CreateWorkflow (Lude.Maybe Lude.Int)
cwMaxConcurrentRuns = Lens.lens (maxConcurrentRuns :: CreateWorkflow -> Lude.Maybe Lude.Int) (\s a -> s {maxConcurrentRuns = a} :: CreateWorkflow)
{-# DEPRECATED cwMaxConcurrentRuns "Use generic-lens or generic-optics with 'maxConcurrentRuns' instead." #-}

-- | A collection of properties to be used as part of each execution of the workflow.
--
-- /Note:/ Consider using 'defaultRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwDefaultRunProperties :: Lens.Lens' CreateWorkflow (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cwDefaultRunProperties = Lens.lens (defaultRunProperties :: CreateWorkflow -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {defaultRunProperties = a} :: CreateWorkflow)
{-# DEPRECATED cwDefaultRunProperties "Use generic-lens or generic-optics with 'defaultRunProperties' instead." #-}

-- | A description of the workflow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwDescription :: Lens.Lens' CreateWorkflow (Lude.Maybe Lude.Text)
cwDescription = Lens.lens (description :: CreateWorkflow -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateWorkflow)
{-# DEPRECATED cwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to be used with this workflow.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwTags :: Lens.Lens' CreateWorkflow (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cwTags = Lens.lens (tags :: CreateWorkflow -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateWorkflow)
{-# DEPRECATED cwTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name to be assigned to the workflow. It should be unique within your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwName :: Lens.Lens' CreateWorkflow Lude.Text
cwName = Lens.lens (name :: CreateWorkflow -> Lude.Text) (\s a -> s {name = a} :: CreateWorkflow)
{-# DEPRECATED cwName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateWorkflow where
  type Rs CreateWorkflow = CreateWorkflowResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateWorkflowResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWorkflow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateWorkflow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWorkflow where
  toJSON CreateWorkflow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxConcurrentRuns" Lude..=) Lude.<$> maxConcurrentRuns,
            ("DefaultRunProperties" Lude..=) Lude.<$> defaultRunProperties,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateWorkflow where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWorkflow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWorkflowResponse' smart constructor.
data CreateWorkflowResponse = CreateWorkflowResponse'
  { name ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateWorkflowResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the workflow which was provided as part of the request.
-- * 'responseStatus' - The response status code.
mkCreateWorkflowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateWorkflowResponse
mkCreateWorkflowResponse pResponseStatus_ =
  CreateWorkflowResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the workflow which was provided as part of the request.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsName :: Lens.Lens' CreateWorkflowResponse (Lude.Maybe Lude.Text)
cwrsName = Lens.lens (name :: CreateWorkflowResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateWorkflowResponse)
{-# DEPRECATED cwrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsResponseStatus :: Lens.Lens' CreateWorkflowResponse Lude.Int
cwrsResponseStatus = Lens.lens (responseStatus :: CreateWorkflowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWorkflowResponse)
{-# DEPRECATED cwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
