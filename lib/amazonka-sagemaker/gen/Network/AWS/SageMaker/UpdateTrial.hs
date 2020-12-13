{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the display name of a trial.
module Network.AWS.SageMaker.UpdateTrial
  ( -- * Creating a request
    UpdateTrial (..),
    mkUpdateTrial,

    -- ** Request lenses
    utDisplayName,
    utTrialName,

    -- * Destructuring the response
    UpdateTrialResponse (..),
    mkUpdateTrialResponse,

    -- ** Response lenses
    utrsTrialARN,
    utrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateTrial' smart constructor.
data UpdateTrial = UpdateTrial'
  { -- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    -- | The name of the trial to update.
    trialName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrial' with the minimum fields required to make a request.
--
-- * 'displayName' - The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
-- * 'trialName' - The name of the trial to update.
mkUpdateTrial ::
  -- | 'trialName'
  Lude.Text ->
  UpdateTrial
mkUpdateTrial pTrialName_ =
  UpdateTrial' {displayName = Lude.Nothing, trialName = pTrialName_}

-- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDisplayName :: Lens.Lens' UpdateTrial (Lude.Maybe Lude.Text)
utDisplayName = Lens.lens (displayName :: UpdateTrial -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdateTrial)
{-# DEPRECATED utDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the trial to update.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTrialName :: Lens.Lens' UpdateTrial Lude.Text
utTrialName = Lens.lens (trialName :: UpdateTrial -> Lude.Text) (\s a -> s {trialName = a} :: UpdateTrial)
{-# DEPRECATED utTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.AWSRequest UpdateTrial where
  type Rs UpdateTrial = UpdateTrialResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTrialResponse'
            Lude.<$> (x Lude..?> "TrialArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTrial where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateTrial" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTrial where
  toJSON UpdateTrial' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayName" Lude..=) Lude.<$> displayName,
            Lude.Just ("TrialName" Lude..= trialName)
          ]
      )

instance Lude.ToPath UpdateTrial where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTrial where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTrialResponse' smart constructor.
data UpdateTrialResponse = UpdateTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrialResponse' with the minimum fields required to make a request.
--
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial.
-- * 'responseStatus' - The response status code.
mkUpdateTrialResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTrialResponse
mkUpdateTrialResponse pResponseStatus_ =
  UpdateTrialResponse'
    { trialARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsTrialARN :: Lens.Lens' UpdateTrialResponse (Lude.Maybe Lude.Text)
utrsTrialARN = Lens.lens (trialARN :: UpdateTrialResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: UpdateTrialResponse)
{-# DEPRECATED utrsTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateTrialResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateTrialResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTrialResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
