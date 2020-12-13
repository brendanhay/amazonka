{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker notebook instance. Before you can delete a notebook instance, you must call the @StopNotebookInstance@ API.
--
-- /Important:/ When you delete a notebook instance, you lose all of your data. Amazon SageMaker removes the ML compute instance, and deletes the ML storage volume and the network interface associated with the notebook instance.
module Network.AWS.SageMaker.DeleteNotebookInstance
  ( -- * Creating a request
    DeleteNotebookInstance (..),
    mkDeleteNotebookInstance,

    -- ** Request lenses
    dniNotebookInstanceName,

    -- * Destructuring the response
    DeleteNotebookInstanceResponse (..),
    mkDeleteNotebookInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteNotebookInstance' smart constructor.
newtype DeleteNotebookInstance = DeleteNotebookInstance'
  { -- | The name of the Amazon SageMaker notebook instance to delete.
    notebookInstanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotebookInstance' with the minimum fields required to make a request.
--
-- * 'notebookInstanceName' - The name of the Amazon SageMaker notebook instance to delete.
mkDeleteNotebookInstance ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  DeleteNotebookInstance
mkDeleteNotebookInstance pNotebookInstanceName_ =
  DeleteNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the Amazon SageMaker notebook instance to delete.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniNotebookInstanceName :: Lens.Lens' DeleteNotebookInstance Lude.Text
dniNotebookInstanceName = Lens.lens (notebookInstanceName :: DeleteNotebookInstance -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: DeleteNotebookInstance)
{-# DEPRECATED dniNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Lude.AWSRequest DeleteNotebookInstance where
  type Rs DeleteNotebookInstance = DeleteNotebookInstanceResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteNotebookInstanceResponse'

instance Lude.ToHeaders DeleteNotebookInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteNotebookInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteNotebookInstance where
  toJSON DeleteNotebookInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NotebookInstanceName" Lude..= notebookInstanceName)]
      )

instance Lude.ToPath DeleteNotebookInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNotebookInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteNotebookInstanceResponse' smart constructor.
data DeleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotebookInstanceResponse' with the minimum fields required to make a request.
mkDeleteNotebookInstanceResponse ::
  DeleteNotebookInstanceResponse
mkDeleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'
