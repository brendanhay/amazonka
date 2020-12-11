{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a lifecycle configuration that you can associate with a notebook instance. A /lifecycle configuration/ is a collection of shell scripts that run when you create or start a notebook instance.
--
-- Each lifecycle configuration script has a limit of 16384 characters.
-- The value of the @> PATH@ environment variable that is available to both scripts is @/sbin:bin:/usr/sbin:/usr/bin@ .
-- View CloudWatch Logs for notebook instance lifecycle configurations in log group @/aws/sagemaker/NotebookInstances@ in log stream @[notebook-instance-name]/[LifecycleConfigHook]@ .
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If a script runs for longer than 5 minutes, it fails and the notebook instance is not created or started.
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
module Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
  ( -- * Creating a request
    CreateNotebookInstanceLifecycleConfig (..),
    mkCreateNotebookInstanceLifecycleConfig,

    -- ** Request lenses
    cnilcOnCreate,
    cnilcOnStart,
    cnilcNotebookInstanceLifecycleConfigName,

    -- * Destructuring the response
    CreateNotebookInstanceLifecycleConfigResponse (..),
    mkCreateNotebookInstanceLifecycleConfigResponse,

    -- ** Response lenses
    cnilcrsNotebookInstanceLifecycleConfigARN,
    cnilcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateNotebookInstanceLifecycleConfig' smart constructor.
data CreateNotebookInstanceLifecycleConfig = CreateNotebookInstanceLifecycleConfig'
  { onCreate ::
      Lude.Maybe
        [NotebookInstanceLifecycleHook],
    onStart ::
      Lude.Maybe
        [NotebookInstanceLifecycleHook],
    notebookInstanceLifecycleConfigName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- * 'notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
-- * 'onCreate' - A shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
-- * 'onStart' - A shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
mkCreateNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Lude.Text ->
  CreateNotebookInstanceLifecycleConfig
mkCreateNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    CreateNotebookInstanceLifecycleConfig'
      { onCreate = Lude.Nothing,
        onStart = Lude.Nothing,
        notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | A shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcOnCreate :: Lens.Lens' CreateNotebookInstanceLifecycleConfig (Lude.Maybe [NotebookInstanceLifecycleHook])
cnilcOnCreate = Lens.lens (onCreate :: CreateNotebookInstanceLifecycleConfig -> Lude.Maybe [NotebookInstanceLifecycleHook]) (\s a -> s {onCreate = a} :: CreateNotebookInstanceLifecycleConfig)
{-# DEPRECATED cnilcOnCreate "Use generic-lens or generic-optics with 'onCreate' instead." #-}

-- | A shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcOnStart :: Lens.Lens' CreateNotebookInstanceLifecycleConfig (Lude.Maybe [NotebookInstanceLifecycleHook])
cnilcOnStart = Lens.lens (onStart :: CreateNotebookInstanceLifecycleConfig -> Lude.Maybe [NotebookInstanceLifecycleHook]) (\s a -> s {onStart = a} :: CreateNotebookInstanceLifecycleConfig)
{-# DEPRECATED cnilcOnStart "Use generic-lens or generic-optics with 'onStart' instead." #-}

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcNotebookInstanceLifecycleConfigName :: Lens.Lens' CreateNotebookInstanceLifecycleConfig Lude.Text
cnilcNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: CreateNotebookInstanceLifecycleConfig -> Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: CreateNotebookInstanceLifecycleConfig)
{-# DEPRECATED cnilcNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

instance Lude.AWSRequest CreateNotebookInstanceLifecycleConfig where
  type
    Rs CreateNotebookInstanceLifecycleConfig =
      CreateNotebookInstanceLifecycleConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateNotebookInstanceLifecycleConfigResponse'
            Lude.<$> (x Lude..?> "NotebookInstanceLifecycleConfigArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNotebookInstanceLifecycleConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.CreateNotebookInstanceLifecycleConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNotebookInstanceLifecycleConfig where
  toJSON CreateNotebookInstanceLifecycleConfig' {..} =
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

instance Lude.ToPath CreateNotebookInstanceLifecycleConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNotebookInstanceLifecycleConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateNotebookInstanceLifecycleConfigResponse' smart constructor.
data CreateNotebookInstanceLifecycleConfigResponse = CreateNotebookInstanceLifecycleConfigResponse'
  { notebookInstanceLifecycleConfigARN ::
      Lude.Maybe
        Lude.Text,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CreateNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
--
-- * 'notebookInstanceLifecycleConfigARN' - The Amazon Resource Name (ARN) of the lifecycle configuration.
-- * 'responseStatus' - The response status code.
mkCreateNotebookInstanceLifecycleConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNotebookInstanceLifecycleConfigResponse
mkCreateNotebookInstanceLifecycleConfigResponse pResponseStatus_ =
  CreateNotebookInstanceLifecycleConfigResponse'
    { notebookInstanceLifecycleConfigARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcrsNotebookInstanceLifecycleConfigARN :: Lens.Lens' CreateNotebookInstanceLifecycleConfigResponse (Lude.Maybe Lude.Text)
cnilcrsNotebookInstanceLifecycleConfigARN = Lens.lens (notebookInstanceLifecycleConfigARN :: CreateNotebookInstanceLifecycleConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigARN = a} :: CreateNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED cnilcrsNotebookInstanceLifecycleConfigARN "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcrsResponseStatus :: Lens.Lens' CreateNotebookInstanceLifecycleConfigResponse Lude.Int
cnilcrsResponseStatus = Lens.lens (responseStatus :: CreateNotebookInstanceLifecycleConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED cnilcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
