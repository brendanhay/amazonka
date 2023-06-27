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
-- Module      : Amazonka.IVSRealtime.UpdateStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stage’s configuration.
module Amazonka.IVSRealtime.UpdateStage
  ( -- * Creating a Request
    UpdateStage (..),
    newUpdateStage,

    -- * Request Lenses
    updateStage_name,
    updateStage_arn,

    -- * Destructuring the Response
    UpdateStageResponse (..),
    newUpdateStageResponse,

    -- * Response Lenses
    updateStageResponse_stage,
    updateStageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStage' smart constructor.
data UpdateStage = UpdateStage'
  { -- | Name of the stage to be updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | ARN of the stage to be updated.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateStage_name' - Name of the stage to be updated.
--
-- 'arn', 'updateStage_arn' - ARN of the stage to be updated.
newUpdateStage ::
  -- | 'arn'
  Prelude.Text ->
  UpdateStage
newUpdateStage pArn_ =
  UpdateStage' {name = Prelude.Nothing, arn = pArn_}

-- | Name of the stage to be updated.
updateStage_name :: Lens.Lens' UpdateStage (Prelude.Maybe Prelude.Text)
updateStage_name = Lens.lens (\UpdateStage' {name} -> name) (\s@UpdateStage' {} a -> s {name = a} :: UpdateStage)

-- | ARN of the stage to be updated.
updateStage_arn :: Lens.Lens' UpdateStage Prelude.Text
updateStage_arn = Lens.lens (\UpdateStage' {arn} -> arn) (\s@UpdateStage' {} a -> s {arn = a} :: UpdateStage)

instance Core.AWSRequest UpdateStage where
  type AWSResponse UpdateStage = UpdateStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStageResponse'
            Prelude.<$> (x Data..?> "stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStage where
  hashWithSalt _salt UpdateStage' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateStage where
  rnf UpdateStage' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStage where
  toJSON UpdateStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateStage where
  toPath = Prelude.const "/UpdateStage"

instance Data.ToQuery UpdateStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStageResponse' smart constructor.
data UpdateStageResponse = UpdateStageResponse'
  { -- | The updated stage.
    stage :: Prelude.Maybe Stage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'updateStageResponse_stage' - The updated stage.
--
-- 'httpStatus', 'updateStageResponse_httpStatus' - The response's http status code.
newUpdateStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStageResponse
newUpdateStageResponse pHttpStatus_ =
  UpdateStageResponse'
    { stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated stage.
updateStageResponse_stage :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Stage)
updateStageResponse_stage = Lens.lens (\UpdateStageResponse' {stage} -> stage) (\s@UpdateStageResponse' {} a -> s {stage = a} :: UpdateStageResponse)

-- | The response's http status code.
updateStageResponse_httpStatus :: Lens.Lens' UpdateStageResponse Prelude.Int
updateStageResponse_httpStatus = Lens.lens (\UpdateStageResponse' {httpStatus} -> httpStatus) (\s@UpdateStageResponse' {} a -> s {httpStatus = a} :: UpdateStageResponse)

instance Prelude.NFData UpdateStageResponse where
  rnf UpdateStageResponse' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
