{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StartNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an ML compute instance with the latest version of the libraries and attaches your ML storage volume. After configuring the notebook instance, Amazon SageMaker sets the notebook instance status to @InService@ . A notebook instance's status must be @InService@ before you can connect to your Jupyter notebook.
module Network.AWS.SageMaker.StartNotebookInstance
  ( -- * Creating a request
    StartNotebookInstance (..),
    mkStartNotebookInstance,

    -- ** Request lenses
    sNotebookInstanceName,

    -- * Destructuring the response
    StartNotebookInstanceResponse (..),
    mkStartNotebookInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStartNotebookInstance' smart constructor.
newtype StartNotebookInstance = StartNotebookInstance'
  { -- | The name of the notebook instance to start.
    notebookInstanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartNotebookInstance' with the minimum fields required to make a request.
--
-- * 'notebookInstanceName' - The name of the notebook instance to start.
mkStartNotebookInstance ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  StartNotebookInstance
mkStartNotebookInstance pNotebookInstanceName_ =
  StartNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the notebook instance to start.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNotebookInstanceName :: Lens.Lens' StartNotebookInstance Lude.Text
sNotebookInstanceName = Lens.lens (notebookInstanceName :: StartNotebookInstance -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: StartNotebookInstance)
{-# DEPRECATED sNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Lude.AWSRequest StartNotebookInstance where
  type Rs StartNotebookInstance = StartNotebookInstanceResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StartNotebookInstanceResponse'

instance Lude.ToHeaders StartNotebookInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StartNotebookInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartNotebookInstance where
  toJSON StartNotebookInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NotebookInstanceName" Lude..= notebookInstanceName)]
      )

instance Lude.ToPath StartNotebookInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StartNotebookInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartNotebookInstanceResponse' smart constructor.
data StartNotebookInstanceResponse = StartNotebookInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartNotebookInstanceResponse' with the minimum fields required to make a request.
mkStartNotebookInstanceResponse ::
  StartNotebookInstanceResponse
mkStartNotebookInstanceResponse = StartNotebookInstanceResponse'
