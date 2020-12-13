{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notebook instance lifecycle configuration.
module Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
  ( -- * Creating a request
    DeleteNotebookInstanceLifecycleConfig (..),
    mkDeleteNotebookInstanceLifecycleConfig,

    -- ** Request lenses
    dNotebookInstanceLifecycleConfigName,

    -- * Destructuring the response
    DeleteNotebookInstanceLifecycleConfigResponse (..),
    mkDeleteNotebookInstanceLifecycleConfigResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteNotebookInstanceLifecycleConfig' smart constructor.
newtype DeleteNotebookInstanceLifecycleConfig = DeleteNotebookInstanceLifecycleConfig'
  { -- | The name of the lifecycle configuration to delete.
    notebookInstanceLifecycleConfigName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- * 'notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration to delete.
mkDeleteNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Lude.Text ->
  DeleteNotebookInstanceLifecycleConfig
mkDeleteNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    DeleteNotebookInstanceLifecycleConfig'
      { notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The name of the lifecycle configuration to delete.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNotebookInstanceLifecycleConfigName :: Lens.Lens' DeleteNotebookInstanceLifecycleConfig Lude.Text
dNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: DeleteNotebookInstanceLifecycleConfig -> Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: DeleteNotebookInstanceLifecycleConfig)
{-# DEPRECATED dNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

instance Lude.AWSRequest DeleteNotebookInstanceLifecycleConfig where
  type
    Rs DeleteNotebookInstanceLifecycleConfig =
      DeleteNotebookInstanceLifecycleConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveNull DeleteNotebookInstanceLifecycleConfigResponse'

instance Lude.ToHeaders DeleteNotebookInstanceLifecycleConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.DeleteNotebookInstanceLifecycleConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteNotebookInstanceLifecycleConfig where
  toJSON DeleteNotebookInstanceLifecycleConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Lude..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance Lude.ToPath DeleteNotebookInstanceLifecycleConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNotebookInstanceLifecycleConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteNotebookInstanceLifecycleConfigResponse' smart constructor.
data DeleteNotebookInstanceLifecycleConfigResponse = DeleteNotebookInstanceLifecycleConfigResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
mkDeleteNotebookInstanceLifecycleConfigResponse ::
  DeleteNotebookInstanceLifecycleConfigResponse
mkDeleteNotebookInstanceLifecycleConfigResponse =
  DeleteNotebookInstanceLifecycleConfigResponse'
