{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflowRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the workflow run properties which were set during the run.
module Network.AWS.Glue.GetWorkflowRunProperties
  ( -- * Creating a request
    GetWorkflowRunProperties (..),
    mkGetWorkflowRunProperties,

    -- ** Request lenses
    gwrpName,
    gwrpRunId,

    -- * Destructuring the response
    GetWorkflowRunPropertiesResponse (..),
    mkGetWorkflowRunPropertiesResponse,

    -- ** Response lenses
    gwrprsRunProperties,
    gwrprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetWorkflowRunProperties' smart constructor.
data GetWorkflowRunProperties = GetWorkflowRunProperties'
  { name ::
      Lude.Text,
    runId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkflowRunProperties' with the minimum fields required to make a request.
--
-- * 'name' - Name of the workflow which was run.
-- * 'runId' - The ID of the workflow run whose run properties should be returned.
mkGetWorkflowRunProperties ::
  -- | 'name'
  Lude.Text ->
  -- | 'runId'
  Lude.Text ->
  GetWorkflowRunProperties
mkGetWorkflowRunProperties pName_ pRunId_ =
  GetWorkflowRunProperties' {name = pName_, runId = pRunId_}

-- | Name of the workflow which was run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrpName :: Lens.Lens' GetWorkflowRunProperties Lude.Text
gwrpName = Lens.lens (name :: GetWorkflowRunProperties -> Lude.Text) (\s a -> s {name = a} :: GetWorkflowRunProperties)
{-# DEPRECATED gwrpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run whose run properties should be returned.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrpRunId :: Lens.Lens' GetWorkflowRunProperties Lude.Text
gwrpRunId = Lens.lens (runId :: GetWorkflowRunProperties -> Lude.Text) (\s a -> s {runId = a} :: GetWorkflowRunProperties)
{-# DEPRECATED gwrpRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.AWSRequest GetWorkflowRunProperties where
  type Rs GetWorkflowRunProperties = GetWorkflowRunPropertiesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWorkflowRunPropertiesResponse'
            Lude.<$> (x Lude..?> "RunProperties" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetWorkflowRunProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetWorkflowRunProperties" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWorkflowRunProperties where
  toJSON GetWorkflowRunProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("RunId" Lude..= runId)
          ]
      )

instance Lude.ToPath GetWorkflowRunProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWorkflowRunProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetWorkflowRunPropertiesResponse' smart constructor.
data GetWorkflowRunPropertiesResponse = GetWorkflowRunPropertiesResponse'
  { runProperties ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkflowRunPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'runProperties' - The workflow run properties which were set during the specified run.
mkGetWorkflowRunPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWorkflowRunPropertiesResponse
mkGetWorkflowRunPropertiesResponse pResponseStatus_ =
  GetWorkflowRunPropertiesResponse'
    { runProperties = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The workflow run properties which were set during the specified run.
--
-- /Note:/ Consider using 'runProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrprsRunProperties :: Lens.Lens' GetWorkflowRunPropertiesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gwrprsRunProperties = Lens.lens (runProperties :: GetWorkflowRunPropertiesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {runProperties = a} :: GetWorkflowRunPropertiesResponse)
{-# DEPRECATED gwrprsRunProperties "Use generic-lens or generic-optics with 'runProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrprsResponseStatus :: Lens.Lens' GetWorkflowRunPropertiesResponse Lude.Int
gwrprsResponseStatus = Lens.lens (responseStatus :: GetWorkflowRunPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWorkflowRunPropertiesResponse)
{-# DEPRECATED gwrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
