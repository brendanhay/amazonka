{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutWorkflowRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the specified workflow run properties for the given workflow run. If a property already exists for the specified run, then it overrides the value otherwise adds the property to existing properties.
module Network.AWS.Glue.PutWorkflowRunProperties
  ( -- * Creating a request
    PutWorkflowRunProperties (..),
    mkPutWorkflowRunProperties,

    -- ** Request lenses
    pwrpName,
    pwrpRunId,
    pwrpRunProperties,

    -- * Destructuring the response
    PutWorkflowRunPropertiesResponse (..),
    mkPutWorkflowRunPropertiesResponse,

    -- ** Response lenses
    pwrprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutWorkflowRunProperties' smart constructor.
data PutWorkflowRunProperties = PutWorkflowRunProperties'
  { name ::
      Lude.Text,
    runId :: Lude.Text,
    runProperties ::
      Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutWorkflowRunProperties' with the minimum fields required to make a request.
--
-- * 'name' - Name of the workflow which was run.
-- * 'runId' - The ID of the workflow run for which the run properties should be updated.
-- * 'runProperties' - The properties to put for the specified run.
mkPutWorkflowRunProperties ::
  -- | 'name'
  Lude.Text ->
  -- | 'runId'
  Lude.Text ->
  PutWorkflowRunProperties
mkPutWorkflowRunProperties pName_ pRunId_ =
  PutWorkflowRunProperties'
    { name = pName_,
      runId = pRunId_,
      runProperties = Lude.mempty
    }

-- | Name of the workflow which was run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpName :: Lens.Lens' PutWorkflowRunProperties Lude.Text
pwrpName = Lens.lens (name :: PutWorkflowRunProperties -> Lude.Text) (\s a -> s {name = a} :: PutWorkflowRunProperties)
{-# DEPRECATED pwrpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run for which the run properties should be updated.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpRunId :: Lens.Lens' PutWorkflowRunProperties Lude.Text
pwrpRunId = Lens.lens (runId :: PutWorkflowRunProperties -> Lude.Text) (\s a -> s {runId = a} :: PutWorkflowRunProperties)
{-# DEPRECATED pwrpRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The properties to put for the specified run.
--
-- /Note:/ Consider using 'runProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpRunProperties :: Lens.Lens' PutWorkflowRunProperties (Lude.HashMap Lude.Text (Lude.Text))
pwrpRunProperties = Lens.lens (runProperties :: PutWorkflowRunProperties -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {runProperties = a} :: PutWorkflowRunProperties)
{-# DEPRECATED pwrpRunProperties "Use generic-lens or generic-optics with 'runProperties' instead." #-}

instance Lude.AWSRequest PutWorkflowRunProperties where
  type Rs PutWorkflowRunProperties = PutWorkflowRunPropertiesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutWorkflowRunPropertiesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutWorkflowRunProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.PutWorkflowRunProperties" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutWorkflowRunProperties where
  toJSON PutWorkflowRunProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("RunId" Lude..= runId),
            Lude.Just ("RunProperties" Lude..= runProperties)
          ]
      )

instance Lude.ToPath PutWorkflowRunProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery PutWorkflowRunProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutWorkflowRunPropertiesResponse' smart constructor.
newtype PutWorkflowRunPropertiesResponse = PutWorkflowRunPropertiesResponse'
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

-- | Creates a value of 'PutWorkflowRunPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutWorkflowRunPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutWorkflowRunPropertiesResponse
mkPutWorkflowRunPropertiesResponse pResponseStatus_ =
  PutWorkflowRunPropertiesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrprsResponseStatus :: Lens.Lens' PutWorkflowRunPropertiesResponse Lude.Int
pwrprsResponseStatus = Lens.lens (responseStatus :: PutWorkflowRunPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutWorkflowRunPropertiesResponse)
{-# DEPRECATED pwrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
