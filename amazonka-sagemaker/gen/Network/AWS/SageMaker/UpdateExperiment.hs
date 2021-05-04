{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateExperiment' smart constructor.
data UpdateExperiment = UpdateExperiment'
  { -- | The description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the experiment as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @ExperimentName@ is
    -- displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the experiment to update.
    experimentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateExperiment
newUpdateExperiment pExperimentName_ =
  UpdateExperiment'
    { description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      experimentName = pExperimentName_
    }

-- | The description of the experiment.
updateExperiment_description :: Lens.Lens' UpdateExperiment (Prelude.Maybe Prelude.Text)
updateExperiment_description = Lens.lens (\UpdateExperiment' {description} -> description) (\s@UpdateExperiment' {} a -> s {description = a} :: UpdateExperiment)

-- | The name of the experiment as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @ExperimentName@ is
-- displayed.
updateExperiment_displayName :: Lens.Lens' UpdateExperiment (Prelude.Maybe Prelude.Text)
updateExperiment_displayName = Lens.lens (\UpdateExperiment' {displayName} -> displayName) (\s@UpdateExperiment' {} a -> s {displayName = a} :: UpdateExperiment)

-- | The name of the experiment to update.
updateExperiment_experimentName :: Lens.Lens' UpdateExperiment Prelude.Text
updateExperiment_experimentName = Lens.lens (\UpdateExperiment' {experimentName} -> experimentName) (\s@UpdateExperiment' {} a -> s {experimentName = a} :: UpdateExperiment)

instance Prelude.AWSRequest UpdateExperiment where
  type Rs UpdateExperiment = UpdateExperimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateExperimentResponse'
            Prelude.<$> (x Prelude..?> "ExperimentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateExperiment

instance Prelude.NFData UpdateExperiment

instance Prelude.ToHeaders UpdateExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.UpdateExperiment" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateExperiment where
  toJSON UpdateExperiment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Description" Prelude..=) Prelude.<$> description,
            ("DisplayName" Prelude..=) Prelude.<$> displayName,
            Prelude.Just
              ("ExperimentName" Prelude..= experimentName)
          ]
      )

instance Prelude.ToPath UpdateExperiment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateExperimentResponse' smart constructor.
data UpdateExperimentResponse = UpdateExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateExperimentResponse
newUpdateExperimentResponse pHttpStatus_ =
  UpdateExperimentResponse'
    { experimentArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
updateExperimentResponse_experimentArn :: Lens.Lens' UpdateExperimentResponse (Prelude.Maybe Prelude.Text)
updateExperimentResponse_experimentArn = Lens.lens (\UpdateExperimentResponse' {experimentArn} -> experimentArn) (\s@UpdateExperimentResponse' {} a -> s {experimentArn = a} :: UpdateExperimentResponse)

-- | The response's http status code.
updateExperimentResponse_httpStatus :: Lens.Lens' UpdateExperimentResponse Prelude.Int
updateExperimentResponse_httpStatus = Lens.lens (\UpdateExperimentResponse' {httpStatus} -> httpStatus) (\s@UpdateExperimentResponse' {} a -> s {httpStatus = a} :: UpdateExperimentResponse)

instance Prelude.NFData UpdateExperimentResponse
