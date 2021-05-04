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
-- Module      : Network.AWS.IoTAnalytics.CancelPipelineReprocessing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the reprocessing of data through the pipeline.
module Network.AWS.IoTAnalytics.CancelPipelineReprocessing
  ( -- * Creating a Request
    CancelPipelineReprocessing (..),
    newCancelPipelineReprocessing,

    -- * Request Lenses
    cancelPipelineReprocessing_pipelineName,
    cancelPipelineReprocessing_reprocessingId,

    -- * Destructuring the Response
    CancelPipelineReprocessingResponse (..),
    newCancelPipelineReprocessingResponse,

    -- * Response Lenses
    cancelPipelineReprocessingResponse_httpStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelPipelineReprocessing' smart constructor.
data CancelPipelineReprocessing = CancelPipelineReprocessing'
  { -- | The name of pipeline for which data reprocessing is canceled.
    pipelineName :: Prelude.Text,
    -- | The ID of the reprocessing task (returned by
    -- @StartPipelineReprocessing@).
    reprocessingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelPipelineReprocessing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'cancelPipelineReprocessing_pipelineName' - The name of pipeline for which data reprocessing is canceled.
--
-- 'reprocessingId', 'cancelPipelineReprocessing_reprocessingId' - The ID of the reprocessing task (returned by
-- @StartPipelineReprocessing@).
newCancelPipelineReprocessing ::
  -- | 'pipelineName'
  Prelude.Text ->
  -- | 'reprocessingId'
  Prelude.Text ->
  CancelPipelineReprocessing
newCancelPipelineReprocessing
  pPipelineName_
  pReprocessingId_ =
    CancelPipelineReprocessing'
      { pipelineName =
          pPipelineName_,
        reprocessingId = pReprocessingId_
      }

-- | The name of pipeline for which data reprocessing is canceled.
cancelPipelineReprocessing_pipelineName :: Lens.Lens' CancelPipelineReprocessing Prelude.Text
cancelPipelineReprocessing_pipelineName = Lens.lens (\CancelPipelineReprocessing' {pipelineName} -> pipelineName) (\s@CancelPipelineReprocessing' {} a -> s {pipelineName = a} :: CancelPipelineReprocessing)

-- | The ID of the reprocessing task (returned by
-- @StartPipelineReprocessing@).
cancelPipelineReprocessing_reprocessingId :: Lens.Lens' CancelPipelineReprocessing Prelude.Text
cancelPipelineReprocessing_reprocessingId = Lens.lens (\CancelPipelineReprocessing' {reprocessingId} -> reprocessingId) (\s@CancelPipelineReprocessing' {} a -> s {reprocessingId = a} :: CancelPipelineReprocessing)

instance
  Prelude.AWSRequest
    CancelPipelineReprocessing
  where
  type
    Rs CancelPipelineReprocessing =
      CancelPipelineReprocessingResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelPipelineReprocessingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelPipelineReprocessing

instance Prelude.NFData CancelPipelineReprocessing

instance Prelude.ToHeaders CancelPipelineReprocessing where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelPipelineReprocessing where
  toPath CancelPipelineReprocessing' {..} =
    Prelude.mconcat
      [ "/pipelines/",
        Prelude.toBS pipelineName,
        "/reprocessing/",
        Prelude.toBS reprocessingId
      ]

instance Prelude.ToQuery CancelPipelineReprocessing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelPipelineReprocessingResponse' smart constructor.
data CancelPipelineReprocessingResponse = CancelPipelineReprocessingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelPipelineReprocessingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelPipelineReprocessingResponse_httpStatus' - The response's http status code.
newCancelPipelineReprocessingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelPipelineReprocessingResponse
newCancelPipelineReprocessingResponse pHttpStatus_ =
  CancelPipelineReprocessingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelPipelineReprocessingResponse_httpStatus :: Lens.Lens' CancelPipelineReprocessingResponse Prelude.Int
cancelPipelineReprocessingResponse_httpStatus = Lens.lens (\CancelPipelineReprocessingResponse' {httpStatus} -> httpStatus) (\s@CancelPipelineReprocessingResponse' {} a -> s {httpStatus = a} :: CancelPipelineReprocessingResponse)

instance
  Prelude.NFData
    CancelPipelineReprocessingResponse
