{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
module Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
  ( -- * Creating a request
    DescribeNotebookInstanceLifecycleConfig (..),
    mkDescribeNotebookInstanceLifecycleConfig,

    -- ** Request lenses
    dNotebookInstanceLifecycleConfigName,

    -- * Destructuring the response
    DescribeNotebookInstanceLifecycleConfigResponse (..),
    mkDescribeNotebookInstanceLifecycleConfigResponse,

    -- ** Response lenses
    dnilcrsCreationTime,
    dnilcrsOnCreate,
    dnilcrsLastModifiedTime,
    dnilcrsNotebookInstanceLifecycleConfigARN,
    dnilcrsOnStart,
    dnilcrsNotebookInstanceLifecycleConfigName,
    dnilcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeNotebookInstanceLifecycleConfig' smart constructor.
newtype DescribeNotebookInstanceLifecycleConfig = DescribeNotebookInstanceLifecycleConfig'
  { notebookInstanceLifecycleConfigName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- * 'notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration to describe.
mkDescribeNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Lude.Text ->
  DescribeNotebookInstanceLifecycleConfig
mkDescribeNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    DescribeNotebookInstanceLifecycleConfig'
      { notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The name of the lifecycle configuration to describe.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNotebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfig Lude.Text
dNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: DescribeNotebookInstanceLifecycleConfig -> Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceLifecycleConfig)
{-# DEPRECATED dNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

instance Lude.AWSRequest DescribeNotebookInstanceLifecycleConfig where
  type
    Rs DescribeNotebookInstanceLifecycleConfig =
      DescribeNotebookInstanceLifecycleConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeNotebookInstanceLifecycleConfigResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "OnCreate" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "NotebookInstanceLifecycleConfigArn")
            Lude.<*> (x Lude..?> "OnStart" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NotebookInstanceLifecycleConfigName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNotebookInstanceLifecycleConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.DescribeNotebookInstanceLifecycleConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeNotebookInstanceLifecycleConfig where
  toJSON DescribeNotebookInstanceLifecycleConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Lude..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance Lude.ToPath DescribeNotebookInstanceLifecycleConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNotebookInstanceLifecycleConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeNotebookInstanceLifecycleConfigResponse' smart constructor.
data DescribeNotebookInstanceLifecycleConfigResponse = DescribeNotebookInstanceLifecycleConfigResponse'
  { creationTime ::
      Lude.Maybe
        Lude.Timestamp,
    onCreate ::
      Lude.Maybe
        [NotebookInstanceLifecycleHook],
    lastModifiedTime ::
      Lude.Maybe
        Lude.Timestamp,
    notebookInstanceLifecycleConfigARN ::
      Lude.Maybe
        Lude.Text,
    onStart ::
      Lude.Maybe
        [NotebookInstanceLifecycleHook],
    notebookInstanceLifecycleConfigName ::
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

-- | Creates a value of 'DescribeNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that tells when the lifecycle configuration was created.
-- * 'lastModifiedTime' - A timestamp that tells when the lifecycle configuration was last modified.
-- * 'notebookInstanceLifecycleConfigARN' - The Amazon Resource Name (ARN) of the lifecycle configuration.
-- * 'notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
-- * 'onCreate' - The shell script that runs only once, when you create a notebook instance.
-- * 'onStart' - The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
-- * 'responseStatus' - The response status code.
mkDescribeNotebookInstanceLifecycleConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNotebookInstanceLifecycleConfigResponse
mkDescribeNotebookInstanceLifecycleConfigResponse pResponseStatus_ =
  DescribeNotebookInstanceLifecycleConfigResponse'
    { creationTime =
        Lude.Nothing,
      onCreate = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      notebookInstanceLifecycleConfigARN =
        Lude.Nothing,
      onStart = Lude.Nothing,
      notebookInstanceLifecycleConfigName =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A timestamp that tells when the lifecycle configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrsCreationTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Lude.Maybe Lude.Timestamp)
dnilcrsCreationTime = Lens.lens (creationTime :: DescribeNotebookInstanceLifecycleConfigResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED dnilcrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The shell script that runs only once, when you create a notebook instance.
--
-- /Note:/ Consider using 'onCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrsOnCreate :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Lude.Maybe [NotebookInstanceLifecycleHook])
dnilcrsOnCreate = Lens.lens (onCreate :: DescribeNotebookInstanceLifecycleConfigResponse -> Lude.Maybe [NotebookInstanceLifecycleHook]) (\s a -> s {onCreate = a} :: DescribeNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED dnilcrsOnCreate "Use generic-lens or generic-optics with 'onCreate' instead." #-}

-- | A timestamp that tells when the lifecycle configuration was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrsLastModifiedTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Lude.Maybe Lude.Timestamp)
dnilcrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeNotebookInstanceLifecycleConfigResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED dnilcrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrsNotebookInstanceLifecycleConfigARN :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Lude.Maybe Lude.Text)
dnilcrsNotebookInstanceLifecycleConfigARN = Lens.lens (notebookInstanceLifecycleConfigARN :: DescribeNotebookInstanceLifecycleConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigARN = a} :: DescribeNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED dnilcrsNotebookInstanceLifecycleConfigARN "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigARN' instead." #-}

-- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
--
-- /Note:/ Consider using 'onStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrsOnStart :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Lude.Maybe [NotebookInstanceLifecycleHook])
dnilcrsOnStart = Lens.lens (onStart :: DescribeNotebookInstanceLifecycleConfigResponse -> Lude.Maybe [NotebookInstanceLifecycleHook]) (\s a -> s {onStart = a} :: DescribeNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED dnilcrsOnStart "Use generic-lens or generic-optics with 'onStart' instead." #-}

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrsNotebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Lude.Maybe Lude.Text)
dnilcrsNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: DescribeNotebookInstanceLifecycleConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED dnilcrsNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrsResponseStatus :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse Lude.Int
dnilcrsResponseStatus = Lens.lens (responseStatus :: DescribeNotebookInstanceLifecycleConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNotebookInstanceLifecycleConfigResponse)
{-# DEPRECATED dnilcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
