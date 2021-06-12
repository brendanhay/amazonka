{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateExperiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, updates, or removes the description of an experiment. Updates the
-- display name of an experiment.
module Network.AWS.SageMaker.UpdateExperiment
  ( -- * Creating a Request
    UpdateExperiment (..),
    newUpdateExperiment,

    -- * Request Lenses
    updateExperiment_description,
    updateExperiment_displayName,
    updateExperiment_experimentName,

    -- * Destructuring the Response
    UpdateExperimentResponse (..),
    newUpdateExperimentResponse,

    -- * Response Lenses
    updateExperimentResponse_experimentArn,
    updateExperimentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateExperiment' smart constructor.
data UpdateExperiment = UpdateExperiment'
  { -- | The description of the experiment.
    description :: Core.Maybe Core.Text,
    -- | The name of the experiment as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @ExperimentName@ is
    -- displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the experiment to update.
    experimentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateExperiment_description' - The description of the experiment.
--
-- 'displayName', 'updateExperiment_displayName' - The name of the experiment as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @ExperimentName@ is
-- displayed.
--
-- 'experimentName', 'updateExperiment_experimentName' - The name of the experiment to update.
newUpdateExperiment ::
  -- | 'experimentName'
  Core.Text ->
  UpdateExperiment
newUpdateExperiment pExperimentName_ =
  UpdateExperiment'
    { description = Core.Nothing,
      displayName = Core.Nothing,
      experimentName = pExperimentName_
    }

-- | The description of the experiment.
updateExperiment_description :: Lens.Lens' UpdateExperiment (Core.Maybe Core.Text)
updateExperiment_description = Lens.lens (\UpdateExperiment' {description} -> description) (\s@UpdateExperiment' {} a -> s {description = a} :: UpdateExperiment)

-- | The name of the experiment as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @ExperimentName@ is
-- displayed.
updateExperiment_displayName :: Lens.Lens' UpdateExperiment (Core.Maybe Core.Text)
updateExperiment_displayName = Lens.lens (\UpdateExperiment' {displayName} -> displayName) (\s@UpdateExperiment' {} a -> s {displayName = a} :: UpdateExperiment)

-- | The name of the experiment to update.
updateExperiment_experimentName :: Lens.Lens' UpdateExperiment Core.Text
updateExperiment_experimentName = Lens.lens (\UpdateExperiment' {experimentName} -> experimentName) (\s@UpdateExperiment' {} a -> s {experimentName = a} :: UpdateExperiment)

instance Core.AWSRequest UpdateExperiment where
  type
    AWSResponse UpdateExperiment =
      UpdateExperimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateExperimentResponse'
            Core.<$> (x Core..?> "ExperimentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateExperiment

instance Core.NFData UpdateExperiment

instance Core.ToHeaders UpdateExperiment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateExperiment" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateExperiment where
  toJSON UpdateExperiment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            Core.Just ("ExperimentName" Core..= experimentName)
          ]
      )

instance Core.ToPath UpdateExperiment where
  toPath = Core.const "/"

instance Core.ToQuery UpdateExperiment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateExperimentResponse' smart constructor.
data UpdateExperimentResponse = UpdateExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentArn', 'updateExperimentResponse_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'httpStatus', 'updateExperimentResponse_httpStatus' - The response's http status code.
newUpdateExperimentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateExperimentResponse
newUpdateExperimentResponse pHttpStatus_ =
  UpdateExperimentResponse'
    { experimentArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
updateExperimentResponse_experimentArn :: Lens.Lens' UpdateExperimentResponse (Core.Maybe Core.Text)
updateExperimentResponse_experimentArn = Lens.lens (\UpdateExperimentResponse' {experimentArn} -> experimentArn) (\s@UpdateExperimentResponse' {} a -> s {experimentArn = a} :: UpdateExperimentResponse)

-- | The response's http status code.
updateExperimentResponse_httpStatus :: Lens.Lens' UpdateExperimentResponse Core.Int
updateExperimentResponse_httpStatus = Lens.lens (\UpdateExperimentResponse' {httpStatus} -> httpStatus) (\s@UpdateExperimentResponse' {} a -> s {httpStatus = a} :: UpdateExperimentResponse)

instance Core.NFData UpdateExperimentResponse
