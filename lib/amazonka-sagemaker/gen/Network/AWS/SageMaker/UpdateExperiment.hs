{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, updates, or removes the description of an experiment. Updates the display name of an experiment.
module Network.AWS.SageMaker.UpdateExperiment
  ( -- * Creating a request
    UpdateExperiment (..),
    mkUpdateExperiment,

    -- ** Request lenses
    ueExperimentName,
    ueDisplayName,
    ueDescription,

    -- * Destructuring the response
    UpdateExperimentResponse (..),
    mkUpdateExperimentResponse,

    -- ** Response lenses
    ursExperimentARN,
    ursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateExperiment' smart constructor.
data UpdateExperiment = UpdateExperiment'
  { -- | The name of the experiment to update.
    experimentName :: Lude.Text,
    -- | The name of the experiment as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    -- | The description of the experiment.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateExperiment' with the minimum fields required to make a request.
--
-- * 'experimentName' - The name of the experiment to update.
-- * 'displayName' - The name of the experiment as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
-- * 'description' - The description of the experiment.
mkUpdateExperiment ::
  -- | 'experimentName'
  Lude.Text ->
  UpdateExperiment
mkUpdateExperiment pExperimentName_ =
  UpdateExperiment'
    { experimentName = pExperimentName_,
      displayName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the experiment to update.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueExperimentName :: Lens.Lens' UpdateExperiment Lude.Text
ueExperimentName = Lens.lens (experimentName :: UpdateExperiment -> Lude.Text) (\s a -> s {experimentName = a} :: UpdateExperiment)
{-# DEPRECATED ueExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The name of the experiment as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDisplayName :: Lens.Lens' UpdateExperiment (Lude.Maybe Lude.Text)
ueDisplayName = Lens.lens (displayName :: UpdateExperiment -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdateExperiment)
{-# DEPRECATED ueDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDescription :: Lens.Lens' UpdateExperiment (Lude.Maybe Lude.Text)
ueDescription = Lens.lens (description :: UpdateExperiment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateExperiment)
{-# DEPRECATED ueDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateExperiment where
  type Rs UpdateExperiment = UpdateExperimentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateExperimentResponse'
            Lude.<$> (x Lude..?> "ExperimentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateExperiment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateExperiment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateExperiment where
  toJSON UpdateExperiment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ExperimentName" Lude..= experimentName),
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateExperiment where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateExperiment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateExperimentResponse' smart constructor.
data UpdateExperimentResponse = UpdateExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateExperimentResponse' with the minimum fields required to make a request.
--
-- * 'experimentARN' - The Amazon Resource Name (ARN) of the experiment.
-- * 'responseStatus' - The response status code.
mkUpdateExperimentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateExperimentResponse
mkUpdateExperimentResponse pResponseStatus_ =
  UpdateExperimentResponse'
    { experimentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursExperimentARN :: Lens.Lens' UpdateExperimentResponse (Lude.Maybe Lude.Text)
ursExperimentARN = Lens.lens (experimentARN :: UpdateExperimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {experimentARN = a} :: UpdateExperimentResponse)
{-# DEPRECATED ursExperimentARN "Use generic-lens or generic-optics with 'experimentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateExperimentResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UpdateExperimentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateExperimentResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
