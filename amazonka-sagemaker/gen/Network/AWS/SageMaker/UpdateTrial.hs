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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateTrial' smart constructor.
data UpdateTrial = UpdateTrial'
  { -- | The name of the trial as displayed. The name doesn\'t need to be unique.
    -- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial to update.
    trialName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateTrial
newUpdateTrial pTrialName_ =
  UpdateTrial'
    { displayName = Prelude.Nothing,
      trialName = pTrialName_
    }

-- | The name of the trial as displayed. The name doesn\'t need to be unique.
-- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
updateTrial_displayName :: Lens.Lens' UpdateTrial (Prelude.Maybe Prelude.Text)
updateTrial_displayName = Lens.lens (\UpdateTrial' {displayName} -> displayName) (\s@UpdateTrial' {} a -> s {displayName = a} :: UpdateTrial)

-- | The name of the trial to update.
updateTrial_trialName :: Lens.Lens' UpdateTrial Prelude.Text
updateTrial_trialName = Lens.lens (\UpdateTrial' {trialName} -> trialName) (\s@UpdateTrial' {} a -> s {trialName = a} :: UpdateTrial)

instance Prelude.AWSRequest UpdateTrial where
  type Rs UpdateTrial = UpdateTrialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrialResponse'
            Prelude.<$> (x Prelude..?> "TrialArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrial

instance Prelude.NFData UpdateTrial

instance Prelude.ToHeaders UpdateTrial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.UpdateTrial" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTrial where
  toJSON UpdateTrial' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DisplayName" Prelude..=) Prelude.<$> displayName,
            Prelude.Just ("TrialName" Prelude..= trialName)
          ]
      )

instance Prelude.ToPath UpdateTrial where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTrial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTrialResponse' smart constructor.
data UpdateTrialResponse = UpdateTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTrialResponse
newUpdateTrialResponse pHttpStatus_ =
  UpdateTrialResponse'
    { trialArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
updateTrialResponse_trialArn :: Lens.Lens' UpdateTrialResponse (Prelude.Maybe Prelude.Text)
updateTrialResponse_trialArn = Lens.lens (\UpdateTrialResponse' {trialArn} -> trialArn) (\s@UpdateTrialResponse' {} a -> s {trialArn = a} :: UpdateTrialResponse)

-- | The response's http status code.
updateTrialResponse_httpStatus :: Lens.Lens' UpdateTrialResponse Prelude.Int
updateTrialResponse_httpStatus = Lens.lens (\UpdateTrialResponse' {httpStatus} -> httpStatus) (\s@UpdateTrialResponse' {} a -> s {httpStatus = a} :: UpdateTrialResponse)

instance Prelude.NFData UpdateTrialResponse
