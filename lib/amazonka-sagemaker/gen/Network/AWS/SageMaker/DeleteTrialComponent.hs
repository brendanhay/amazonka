{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial component. A trial component must be disassociated from all trials before the trial component can be deleted. To disassociate a trial component from a trial, call the 'DisassociateTrialComponent' API.
module Network.AWS.SageMaker.DeleteTrialComponent
  ( -- * Creating a request
    DeleteTrialComponent (..),
    mkDeleteTrialComponent,

    -- ** Request lenses
    dTrialComponentName,

    -- * Destructuring the response
    DeleteTrialComponentResponse (..),
    mkDeleteTrialComponentResponse,

    -- ** Response lenses
    dtctrsTrialComponentARN,
    dtctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteTrialComponent' smart constructor.
newtype DeleteTrialComponent = DeleteTrialComponent'
  { trialComponentName ::
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

-- | Creates a value of 'DeleteTrialComponent' with the minimum fields required to make a request.
--
-- * 'trialComponentName' - The name of the component to delete.
mkDeleteTrialComponent ::
  -- | 'trialComponentName'
  Lude.Text ->
  DeleteTrialComponent
mkDeleteTrialComponent pTrialComponentName_ =
  DeleteTrialComponent' {trialComponentName = pTrialComponentName_}

-- | The name of the component to delete.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTrialComponentName :: Lens.Lens' DeleteTrialComponent Lude.Text
dTrialComponentName = Lens.lens (trialComponentName :: DeleteTrialComponent -> Lude.Text) (\s a -> s {trialComponentName = a} :: DeleteTrialComponent)
{-# DEPRECATED dTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

instance Lude.AWSRequest DeleteTrialComponent where
  type Rs DeleteTrialComponent = DeleteTrialComponentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTrialComponentResponse'
            Lude.<$> (x Lude..?> "TrialComponentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrialComponent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteTrialComponent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTrialComponent where
  toJSON DeleteTrialComponent' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TrialComponentName" Lude..= trialComponentName)]
      )

instance Lude.ToPath DeleteTrialComponent where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrialComponent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTrialComponentResponse' smart constructor.
data DeleteTrialComponentResponse = DeleteTrialComponentResponse'
  { trialComponentARN ::
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

-- | Creates a value of 'DeleteTrialComponentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trialComponentARN' - The Amazon Resource Name (ARN) of the component is being deleted.
mkDeleteTrialComponentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrialComponentResponse
mkDeleteTrialComponentResponse pResponseStatus_ =
  DeleteTrialComponentResponse'
    { trialComponentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the component is being deleted.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtctrsTrialComponentARN :: Lens.Lens' DeleteTrialComponentResponse (Lude.Maybe Lude.Text)
dtctrsTrialComponentARN = Lens.lens (trialComponentARN :: DeleteTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: DeleteTrialComponentResponse)
{-# DEPRECATED dtctrsTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtctrsResponseStatus :: Lens.Lens' DeleteTrialComponentResponse Lude.Int
dtctrsResponseStatus = Lens.lens (responseStatus :: DeleteTrialComponentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrialComponentResponse)
{-# DEPRECATED dtctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
