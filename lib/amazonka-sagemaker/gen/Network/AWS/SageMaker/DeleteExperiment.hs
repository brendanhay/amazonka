{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker experiment. All trials associated with the experiment must be deleted first. Use the 'ListTrials' API to get a list of the trials associated with the experiment.
module Network.AWS.SageMaker.DeleteExperiment
  ( -- * Creating a request
    DeleteExperiment (..),
    mkDeleteExperiment,

    -- ** Request lenses
    dExperimentName,

    -- * Destructuring the response
    DeleteExperimentResponse (..),
    mkDeleteExperimentResponse,

    -- ** Response lenses
    delersExperimentARN,
    delersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteExperiment' smart constructor.
newtype DeleteExperiment = DeleteExperiment'
  { experimentName ::
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

-- | Creates a value of 'DeleteExperiment' with the minimum fields required to make a request.
--
-- * 'experimentName' - The name of the experiment to delete.
mkDeleteExperiment ::
  -- | 'experimentName'
  Lude.Text ->
  DeleteExperiment
mkDeleteExperiment pExperimentName_ =
  DeleteExperiment' {experimentName = pExperimentName_}

-- | The name of the experiment to delete.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExperimentName :: Lens.Lens' DeleteExperiment Lude.Text
dExperimentName = Lens.lens (experimentName :: DeleteExperiment -> Lude.Text) (\s a -> s {experimentName = a} :: DeleteExperiment)
{-# DEPRECATED dExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

instance Lude.AWSRequest DeleteExperiment where
  type Rs DeleteExperiment = DeleteExperimentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteExperimentResponse'
            Lude.<$> (x Lude..?> "ExperimentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteExperiment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteExperiment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteExperiment where
  toJSON DeleteExperiment' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ExperimentName" Lude..= experimentName)]
      )

instance Lude.ToPath DeleteExperiment where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteExperiment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteExperimentResponse' smart constructor.
data DeleteExperimentResponse = DeleteExperimentResponse'
  { experimentARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteExperimentResponse' with the minimum fields required to make a request.
--
-- * 'experimentARN' - The Amazon Resource Name (ARN) of the experiment that is being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteExperimentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteExperimentResponse
mkDeleteExperimentResponse pResponseStatus_ =
  DeleteExperimentResponse'
    { experimentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment that is being deleted.
--
-- /Note:/ Consider using 'experimentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delersExperimentARN :: Lens.Lens' DeleteExperimentResponse (Lude.Maybe Lude.Text)
delersExperimentARN = Lens.lens (experimentARN :: DeleteExperimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {experimentARN = a} :: DeleteExperimentResponse)
{-# DEPRECATED delersExperimentARN "Use generic-lens or generic-optics with 'experimentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delersResponseStatus :: Lens.Lens' DeleteExperimentResponse Lude.Int
delersResponseStatus = Lens.lens (responseStatus :: DeleteExperimentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteExperimentResponse)
{-# DEPRECATED delersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
