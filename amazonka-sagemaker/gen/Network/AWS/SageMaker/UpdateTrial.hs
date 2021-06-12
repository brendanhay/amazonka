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
-- Module      : Network.AWS.SageMaker.UpdateTrial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the display name of a trial.
module Network.AWS.SageMaker.UpdateTrial
  ( -- * Creating a Request
    UpdateTrial (..),
    newUpdateTrial,

    -- * Request Lenses
    updateTrial_displayName,
    updateTrial_trialName,

    -- * Destructuring the Response
    UpdateTrialResponse (..),
    newUpdateTrialResponse,

    -- * Response Lenses
    updateTrialResponse_trialArn,
    updateTrialResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateTrial' smart constructor.
data UpdateTrial = UpdateTrial'
  { -- | The name of the trial as displayed. The name doesn\'t need to be unique.
    -- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the trial to update.
    trialName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'updateTrial_displayName' - The name of the trial as displayed. The name doesn\'t need to be unique.
-- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
--
-- 'trialName', 'updateTrial_trialName' - The name of the trial to update.
newUpdateTrial ::
  -- | 'trialName'
  Core.Text ->
  UpdateTrial
newUpdateTrial pTrialName_ =
  UpdateTrial'
    { displayName = Core.Nothing,
      trialName = pTrialName_
    }

-- | The name of the trial as displayed. The name doesn\'t need to be unique.
-- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
updateTrial_displayName :: Lens.Lens' UpdateTrial (Core.Maybe Core.Text)
updateTrial_displayName = Lens.lens (\UpdateTrial' {displayName} -> displayName) (\s@UpdateTrial' {} a -> s {displayName = a} :: UpdateTrial)

-- | The name of the trial to update.
updateTrial_trialName :: Lens.Lens' UpdateTrial Core.Text
updateTrial_trialName = Lens.lens (\UpdateTrial' {trialName} -> trialName) (\s@UpdateTrial' {} a -> s {trialName = a} :: UpdateTrial)

instance Core.AWSRequest UpdateTrial where
  type AWSResponse UpdateTrial = UpdateTrialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrialResponse'
            Core.<$> (x Core..?> "TrialArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTrial

instance Core.NFData UpdateTrial

instance Core.ToHeaders UpdateTrial where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateTrial" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTrial where
  toJSON UpdateTrial' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DisplayName" Core..=) Core.<$> displayName,
            Core.Just ("TrialName" Core..= trialName)
          ]
      )

instance Core.ToPath UpdateTrial where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTrial where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateTrialResponse' smart constructor.
data UpdateTrialResponse = UpdateTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTrialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'updateTrialResponse_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'httpStatus', 'updateTrialResponse_httpStatus' - The response's http status code.
newUpdateTrialResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTrialResponse
newUpdateTrialResponse pHttpStatus_ =
  UpdateTrialResponse'
    { trialArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
updateTrialResponse_trialArn :: Lens.Lens' UpdateTrialResponse (Core.Maybe Core.Text)
updateTrialResponse_trialArn = Lens.lens (\UpdateTrialResponse' {trialArn} -> trialArn) (\s@UpdateTrialResponse' {} a -> s {trialArn = a} :: UpdateTrialResponse)

-- | The response's http status code.
updateTrialResponse_httpStatus :: Lens.Lens' UpdateTrialResponse Core.Int
updateTrialResponse_httpStatus = Lens.lens (\UpdateTrialResponse' {httpStatus} -> httpStatus) (\s@UpdateTrialResponse' {} a -> s {httpStatus = a} :: UpdateTrialResponse)

instance Core.NFData UpdateTrialResponse
