{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing workflow.
module Network.AWS.Glue.UpdateWorkflow
  ( -- * Creating a request
    UpdateWorkflow (..),
    mkUpdateWorkflow,

    -- ** Request lenses
    uwMaxConcurrentRuns,
    uwDefaultRunProperties,
    uwName,
    uwDescription,

    -- * Destructuring the response
    UpdateWorkflowResponse (..),
    mkUpdateWorkflowResponse,

    -- ** Response lenses
    uwrsName,
    uwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateWorkflow' smart constructor.
data UpdateWorkflow = UpdateWorkflow'
  { -- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
    maxConcurrentRuns :: Lude.Maybe Lude.Int,
    -- | A collection of properties to be used as part of each execution of the workflow.
    defaultRunProperties :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Name of the workflow to be updated.
    name :: Lude.Text,
    -- | The description of the workflow.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkflow' with the minimum fields required to make a request.
--
-- * 'maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
-- * 'defaultRunProperties' - A collection of properties to be used as part of each execution of the workflow.
-- * 'name' - Name of the workflow to be updated.
-- * 'description' - The description of the workflow.
mkUpdateWorkflow ::
  -- | 'name'
  Lude.Text ->
  UpdateWorkflow
mkUpdateWorkflow pName_ =
  UpdateWorkflow'
    { maxConcurrentRuns = Lude.Nothing,
      defaultRunProperties = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing
    }

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- /Note:/ Consider using 'maxConcurrentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwMaxConcurrentRuns :: Lens.Lens' UpdateWorkflow (Lude.Maybe Lude.Int)
uwMaxConcurrentRuns = Lens.lens (maxConcurrentRuns :: UpdateWorkflow -> Lude.Maybe Lude.Int) (\s a -> s {maxConcurrentRuns = a} :: UpdateWorkflow)
{-# DEPRECATED uwMaxConcurrentRuns "Use generic-lens or generic-optics with 'maxConcurrentRuns' instead." #-}

-- | A collection of properties to be used as part of each execution of the workflow.
--
-- /Note:/ Consider using 'defaultRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwDefaultRunProperties :: Lens.Lens' UpdateWorkflow (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uwDefaultRunProperties = Lens.lens (defaultRunProperties :: UpdateWorkflow -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {defaultRunProperties = a} :: UpdateWorkflow)
{-# DEPRECATED uwDefaultRunProperties "Use generic-lens or generic-optics with 'defaultRunProperties' instead." #-}

-- | Name of the workflow to be updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwName :: Lens.Lens' UpdateWorkflow Lude.Text
uwName = Lens.lens (name :: UpdateWorkflow -> Lude.Text) (\s a -> s {name = a} :: UpdateWorkflow)
{-# DEPRECATED uwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the workflow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwDescription :: Lens.Lens' UpdateWorkflow (Lude.Maybe Lude.Text)
uwDescription = Lens.lens (description :: UpdateWorkflow -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateWorkflow)
{-# DEPRECATED uwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateWorkflow where
  type Rs UpdateWorkflow = UpdateWorkflowResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateWorkflowResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateWorkflow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateWorkflow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateWorkflow where
  toJSON UpdateWorkflow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxConcurrentRuns" Lude..=) Lude.<$> maxConcurrentRuns,
            ("DefaultRunProperties" Lude..=) Lude.<$> defaultRunProperties,
            Lude.Just ("Name" Lude..= name),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateWorkflow where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateWorkflow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateWorkflowResponse' smart constructor.
data UpdateWorkflowResponse = UpdateWorkflowResponse'
  { -- | The name of the workflow which was specified in input.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkflowResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the workflow which was specified in input.
-- * 'responseStatus' - The response status code.
mkUpdateWorkflowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateWorkflowResponse
mkUpdateWorkflowResponse pResponseStatus_ =
  UpdateWorkflowResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the workflow which was specified in input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrsName :: Lens.Lens' UpdateWorkflowResponse (Lude.Maybe Lude.Text)
uwrsName = Lens.lens (name :: UpdateWorkflowResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateWorkflowResponse)
{-# DEPRECATED uwrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrsResponseStatus :: Lens.Lens' UpdateWorkflowResponse Lude.Int
uwrsResponseStatus = Lens.lens (responseStatus :: UpdateWorkflowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateWorkflowResponse)
{-# DEPRECATED uwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
