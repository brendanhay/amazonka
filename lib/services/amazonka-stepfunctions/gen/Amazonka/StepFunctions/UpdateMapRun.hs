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
-- Module      : Amazonka.StepFunctions.UpdateMapRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an in-progress Map Run\'s configuration to include changes to
-- the settings that control maximum concurrency and Map Run failure.
module Amazonka.StepFunctions.UpdateMapRun
  ( -- * Creating a Request
    UpdateMapRun (..),
    newUpdateMapRun,

    -- * Request Lenses
    updateMapRun_maxConcurrency,
    updateMapRun_toleratedFailureCount,
    updateMapRun_toleratedFailurePercentage,
    updateMapRun_mapRunArn,

    -- * Destructuring the Response
    UpdateMapRunResponse (..),
    newUpdateMapRunResponse,

    -- * Response Lenses
    updateMapRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newUpdateMapRun' smart constructor.
data UpdateMapRun = UpdateMapRun'
  { -- | The maximum number of child workflow executions that can be specified to
    -- run in parallel for the Map Run at the same time.
    maxConcurrency :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of failed items before the Map Run fails.
    toleratedFailureCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum percentage of failed items before the Map Run fails.
    toleratedFailurePercentage :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Resource Name (ARN) of a Map Run.
    mapRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMapRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrency', 'updateMapRun_maxConcurrency' - The maximum number of child workflow executions that can be specified to
-- run in parallel for the Map Run at the same time.
--
-- 'toleratedFailureCount', 'updateMapRun_toleratedFailureCount' - The maximum number of failed items before the Map Run fails.
--
-- 'toleratedFailurePercentage', 'updateMapRun_toleratedFailurePercentage' - The maximum percentage of failed items before the Map Run fails.
--
-- 'mapRunArn', 'updateMapRun_mapRunArn' - The Amazon Resource Name (ARN) of a Map Run.
newUpdateMapRun ::
  -- | 'mapRunArn'
  Prelude.Text ->
  UpdateMapRun
newUpdateMapRun pMapRunArn_ =
  UpdateMapRun'
    { maxConcurrency = Prelude.Nothing,
      toleratedFailureCount = Prelude.Nothing,
      toleratedFailurePercentage = Prelude.Nothing,
      mapRunArn = pMapRunArn_
    }

-- | The maximum number of child workflow executions that can be specified to
-- run in parallel for the Map Run at the same time.
updateMapRun_maxConcurrency :: Lens.Lens' UpdateMapRun (Prelude.Maybe Prelude.Natural)
updateMapRun_maxConcurrency = Lens.lens (\UpdateMapRun' {maxConcurrency} -> maxConcurrency) (\s@UpdateMapRun' {} a -> s {maxConcurrency = a} :: UpdateMapRun)

-- | The maximum number of failed items before the Map Run fails.
updateMapRun_toleratedFailureCount :: Lens.Lens' UpdateMapRun (Prelude.Maybe Prelude.Natural)
updateMapRun_toleratedFailureCount = Lens.lens (\UpdateMapRun' {toleratedFailureCount} -> toleratedFailureCount) (\s@UpdateMapRun' {} a -> s {toleratedFailureCount = a} :: UpdateMapRun)

-- | The maximum percentage of failed items before the Map Run fails.
updateMapRun_toleratedFailurePercentage :: Lens.Lens' UpdateMapRun (Prelude.Maybe Prelude.Double)
updateMapRun_toleratedFailurePercentage = Lens.lens (\UpdateMapRun' {toleratedFailurePercentage} -> toleratedFailurePercentage) (\s@UpdateMapRun' {} a -> s {toleratedFailurePercentage = a} :: UpdateMapRun)

-- | The Amazon Resource Name (ARN) of a Map Run.
updateMapRun_mapRunArn :: Lens.Lens' UpdateMapRun Prelude.Text
updateMapRun_mapRunArn = Lens.lens (\UpdateMapRun' {mapRunArn} -> mapRunArn) (\s@UpdateMapRun' {} a -> s {mapRunArn = a} :: UpdateMapRun)

instance Core.AWSRequest UpdateMapRun where
  type AWSResponse UpdateMapRun = UpdateMapRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMapRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMapRun where
  hashWithSalt _salt UpdateMapRun' {..} =
    _salt `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` toleratedFailureCount
      `Prelude.hashWithSalt` toleratedFailurePercentage
      `Prelude.hashWithSalt` mapRunArn

instance Prelude.NFData UpdateMapRun where
  rnf UpdateMapRun' {..} =
    Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf toleratedFailureCount
      `Prelude.seq` Prelude.rnf toleratedFailurePercentage
      `Prelude.seq` Prelude.rnf mapRunArn

instance Data.ToHeaders UpdateMapRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.UpdateMapRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMapRun where
  toJSON UpdateMapRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxConcurrency" Data..=)
              Prelude.<$> maxConcurrency,
            ("toleratedFailureCount" Data..=)
              Prelude.<$> toleratedFailureCount,
            ("toleratedFailurePercentage" Data..=)
              Prelude.<$> toleratedFailurePercentage,
            Prelude.Just ("mapRunArn" Data..= mapRunArn)
          ]
      )

instance Data.ToPath UpdateMapRun where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMapRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMapRunResponse' smart constructor.
data UpdateMapRunResponse = UpdateMapRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMapRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMapRunResponse_httpStatus' - The response's http status code.
newUpdateMapRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMapRunResponse
newUpdateMapRunResponse pHttpStatus_ =
  UpdateMapRunResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateMapRunResponse_httpStatus :: Lens.Lens' UpdateMapRunResponse Prelude.Int
updateMapRunResponse_httpStatus = Lens.lens (\UpdateMapRunResponse' {httpStatus} -> httpStatus) (\s@UpdateMapRunResponse' {} a -> s {httpStatus = a} :: UpdateMapRunResponse)

instance Prelude.NFData UpdateMapRunResponse where
  rnf UpdateMapRunResponse' {..} =
    Prelude.rnf httpStatus
