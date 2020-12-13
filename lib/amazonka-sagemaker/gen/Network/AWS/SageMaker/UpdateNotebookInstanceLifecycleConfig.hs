{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance lifecycle configuration created with the 'CreateNotebookInstanceLifecycleConfig' API.
module Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
  ( -- * Creating a request
    UpdateNotebookInstanceLifecycleConfig (..),
    mkUpdateNotebookInstanceLifecycleConfig,

    -- ** Request lenses
    unilcOnCreate,
    unilcOnStart,
    unilcNotebookInstanceLifecycleConfigName,

    -- * Destructuring the response
    UpdateNotebookInstanceLifecycleConfigResponse (..),
    mkUpdateNotebookInstanceLifecycleConfigResponse,

    -- ** Response lenses
    unilcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateNotebookInstanceLifecycleConfig' smart constructor.
data UpdateNotebookInstanceLifecycleConfig = UpdateNotebookInstanceLifecycleConfig'
  { -- | The shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
    onCreate :: Lude.Maybe [NotebookInstanceLifecycleHook],
    -- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
    onStart :: Lude.Maybe [NotebookInstanceLifecycleHook],
    -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- * 'onCreate' - The shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
-- * 'onStart' - The shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
-- * 'notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
mkUpdateNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Lude.Text ->
  UpdateNotebookInstanceLifecycleConfig
mkUpdateNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    UpdateNotebookInstanceLifecycleConfig'
      { onCreate = Lude.Nothing,
        onStart = Lude.Nothing,
        notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcOnCreate :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Lude.Maybe [NotebookInstanceLifecycleHook])
unilcOnCreate = Lens.lens (onCreate :: UpdateNotebookInstanceLifecycleConfig -> Lude.Maybe [NotebookInstanceLifecycleHook]) (\s a -> s {onCreate = a} :: UpdateNotebookInstanceLifecycleConfig)
{-# DEPRECATED unilcOnCreate "Use generic-lens or generic-optics with 'onCreate' instead." #-}

-- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcOnStart :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Lude.Maybe [NotebookInstanceLifecycleHook])
unilcOnStart = Lens.lens (onStart :: UpdateNotebookInstanceLifecycleConfig -> Lude.Maybe [NotebookInstanceLifecycleHook]) (\s a -> s {onStart = a} :: UpdateNotebookInstanceLifecycleConfig)
{-# DEPRECATED unilcOnStart "Use generic-lens or generic-optics with 'onStart' instead." #-}

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcNotebookInstanceLifecycleConfigName :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig Lude.Text
unilcNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: UpdateNotebookInstanceLifecycleConfig -> Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: UpdateNotebookInstanceLifecycleConfig)
{-# DEPRECATED unilcNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

instance Lude.AWSRequest UpdateNotebookInstanceLifecycleConfig where
  type
    Rs UpdateNotebookInstanceLifecycleConfig =
      UpdateNotebookInstanceLifecycleConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceLifecycleConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNotebookInstanceLifecycleConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.UpdateNotebookInstanceLifecycleConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNotebookInstanceLifecycleConfig where
  toJSON UpdateNotebookInstanceLifecycleConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OnCreate" Lude..=) Lude.<$> onCreate,
            ("OnStart" Lude..=) Lude.<$> onStart,
            Lude.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Lude..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance Lude.ToPath UpdateNotebookInstanceLifecycleConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNotebookInstanceLifecycleConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateNotebookInstanceLifecycleConfigResponse' smart constructor.
newtype UpdateNotebookInstanceLifecycleConfigResponse = UpdateNotebookInstanceLifecycleConfigResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateNotebookInstanceLifecycleConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNotebookInstanceLifecycleConfigResponse
mkUpdateNotebookInstanceLifecycleConfigResponse pResponseStatus_ =
  UpdateNotebookInstanceLifecycleConfigResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcrsResponseStatus :: Lens.Lens' UpdateNotebookInstanceLifecycleConfigResponse Lude.Int
unilcrsResponseStatus = Lens.lens (responseStatus :: UpdateNotebookInstanceLifecycleConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED unilcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
