{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial. All trial components that make up the trial must be deleted first. Use the 'DescribeTrialComponent' API to get the list of trial components.
module Network.AWS.SageMaker.DeleteTrial
  ( -- * Creating a request
    DeleteTrial (..),
    mkDeleteTrial,

    -- ** Request lenses
    dtTrialName,

    -- * Destructuring the response
    DeleteTrialResponse (..),
    mkDeleteTrialResponse,

    -- ** Response lenses
    dtfrsTrialARN,
    dtfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteTrial' smart constructor.
newtype DeleteTrial = DeleteTrial'
  { -- | The name of the trial to delete.
    trialName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrial' with the minimum fields required to make a request.
--
-- * 'trialName' - The name of the trial to delete.
mkDeleteTrial ::
  -- | 'trialName'
  Lude.Text ->
  DeleteTrial
mkDeleteTrial pTrialName_ = DeleteTrial' {trialName = pTrialName_}

-- | The name of the trial to delete.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTrialName :: Lens.Lens' DeleteTrial Lude.Text
dtTrialName = Lens.lens (trialName :: DeleteTrial -> Lude.Text) (\s a -> s {trialName = a} :: DeleteTrial)
{-# DEPRECATED dtTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.AWSRequest DeleteTrial where
  type Rs DeleteTrial = DeleteTrialResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTrialResponse'
            Lude.<$> (x Lude..?> "TrialArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrial where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteTrial" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTrial where
  toJSON DeleteTrial' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TrialName" Lude..= trialName)])

instance Lude.ToPath DeleteTrial where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrial where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTrialResponse' smart constructor.
data DeleteTrialResponse = DeleteTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial that is being deleted.
    trialARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrialResponse' with the minimum fields required to make a request.
--
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial that is being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteTrialResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrialResponse
mkDeleteTrialResponse pResponseStatus_ =
  DeleteTrialResponse'
    { trialARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial that is being deleted.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfrsTrialARN :: Lens.Lens' DeleteTrialResponse (Lude.Maybe Lude.Text)
dtfrsTrialARN = Lens.lens (trialARN :: DeleteTrialResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: DeleteTrialResponse)
{-# DEPRECATED dtfrsTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfrsResponseStatus :: Lens.Lens' DeleteTrialResponse Lude.Int
dtfrsResponseStatus = Lens.lens (responseStatus :: DeleteTrialResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrialResponse)
{-# DEPRECATED dtfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
