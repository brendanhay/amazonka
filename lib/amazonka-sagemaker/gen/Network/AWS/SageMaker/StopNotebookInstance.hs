{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the ML compute instance. Before terminating the instance, Amazon SageMaker disconnects the ML storage volume from it. Amazon SageMaker preserves the ML storage volume. Amazon SageMaker stops charging you for the ML compute instance when you call @StopNotebookInstance@ .
--
-- To access data on the ML storage volume for a notebook instance that has been terminated, call the @StartNotebookInstance@ API. @StartNotebookInstance@ launches another ML compute instance, configures it, and attaches the preserved ML storage volume so you can continue your work.
module Network.AWS.SageMaker.StopNotebookInstance
  ( -- * Creating a request
    StopNotebookInstance (..),
    mkStopNotebookInstance,

    -- ** Request lenses
    sniNotebookInstanceName,

    -- * Destructuring the response
    StopNotebookInstanceResponse (..),
    mkStopNotebookInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopNotebookInstance' smart constructor.
newtype StopNotebookInstance = StopNotebookInstance'
  { -- | The name of the notebook instance to terminate.
    notebookInstanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopNotebookInstance' with the minimum fields required to make a request.
--
-- * 'notebookInstanceName' - The name of the notebook instance to terminate.
mkStopNotebookInstance ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  StopNotebookInstance
mkStopNotebookInstance pNotebookInstanceName_ =
  StopNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the notebook instance to terminate.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sniNotebookInstanceName :: Lens.Lens' StopNotebookInstance Lude.Text
sniNotebookInstanceName = Lens.lens (notebookInstanceName :: StopNotebookInstance -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: StopNotebookInstance)
{-# DEPRECATED sniNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Lude.AWSRequest StopNotebookInstance where
  type Rs StopNotebookInstance = StopNotebookInstanceResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopNotebookInstanceResponse'

instance Lude.ToHeaders StopNotebookInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopNotebookInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopNotebookInstance where
  toJSON StopNotebookInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NotebookInstanceName" Lude..= notebookInstanceName)]
      )

instance Lude.ToPath StopNotebookInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StopNotebookInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopNotebookInstanceResponse' smart constructor.
data StopNotebookInstanceResponse = StopNotebookInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopNotebookInstanceResponse' with the minimum fields required to make a request.
mkStopNotebookInstanceResponse ::
  StopNotebookInstanceResponse
mkStopNotebookInstanceResponse = StopNotebookInstanceResponse'
