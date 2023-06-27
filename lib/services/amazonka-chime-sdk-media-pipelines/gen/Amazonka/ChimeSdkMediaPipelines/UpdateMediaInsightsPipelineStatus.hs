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
-- Module      : Amazonka.ChimeSdkMediaPipelines.UpdateMediaInsightsPipelineStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a media insights pipeline.
module Amazonka.ChimeSdkMediaPipelines.UpdateMediaInsightsPipelineStatus
  ( -- * Creating a Request
    UpdateMediaInsightsPipelineStatus (..),
    newUpdateMediaInsightsPipelineStatus,

    -- * Request Lenses
    updateMediaInsightsPipelineStatus_identifier,
    updateMediaInsightsPipelineStatus_updateStatus,

    -- * Destructuring the Response
    UpdateMediaInsightsPipelineStatusResponse (..),
    newUpdateMediaInsightsPipelineStatusResponse,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMediaInsightsPipelineStatus' smart constructor.
data UpdateMediaInsightsPipelineStatus = UpdateMediaInsightsPipelineStatus'
  { -- | The unique identifier of the resource to be updated. Valid values
    -- include the ID and ARN of the media insights pipeline.
    identifier :: Prelude.Text,
    -- | The requested status of the media insights pipeline.
    updateStatus :: MediaPipelineStatusUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMediaInsightsPipelineStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'updateMediaInsightsPipelineStatus_identifier' - The unique identifier of the resource to be updated. Valid values
-- include the ID and ARN of the media insights pipeline.
--
-- 'updateStatus', 'updateMediaInsightsPipelineStatus_updateStatus' - The requested status of the media insights pipeline.
newUpdateMediaInsightsPipelineStatus ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'updateStatus'
  MediaPipelineStatusUpdate ->
  UpdateMediaInsightsPipelineStatus
newUpdateMediaInsightsPipelineStatus
  pIdentifier_
  pUpdateStatus_ =
    UpdateMediaInsightsPipelineStatus'
      { identifier =
          pIdentifier_,
        updateStatus = pUpdateStatus_
      }

-- | The unique identifier of the resource to be updated. Valid values
-- include the ID and ARN of the media insights pipeline.
updateMediaInsightsPipelineStatus_identifier :: Lens.Lens' UpdateMediaInsightsPipelineStatus Prelude.Text
updateMediaInsightsPipelineStatus_identifier = Lens.lens (\UpdateMediaInsightsPipelineStatus' {identifier} -> identifier) (\s@UpdateMediaInsightsPipelineStatus' {} a -> s {identifier = a} :: UpdateMediaInsightsPipelineStatus)

-- | The requested status of the media insights pipeline.
updateMediaInsightsPipelineStatus_updateStatus :: Lens.Lens' UpdateMediaInsightsPipelineStatus MediaPipelineStatusUpdate
updateMediaInsightsPipelineStatus_updateStatus = Lens.lens (\UpdateMediaInsightsPipelineStatus' {updateStatus} -> updateStatus) (\s@UpdateMediaInsightsPipelineStatus' {} a -> s {updateStatus = a} :: UpdateMediaInsightsPipelineStatus)

instance
  Core.AWSRequest
    UpdateMediaInsightsPipelineStatus
  where
  type
    AWSResponse UpdateMediaInsightsPipelineStatus =
      UpdateMediaInsightsPipelineStatusResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateMediaInsightsPipelineStatusResponse'

instance
  Prelude.Hashable
    UpdateMediaInsightsPipelineStatus
  where
  hashWithSalt
    _salt
    UpdateMediaInsightsPipelineStatus' {..} =
      _salt
        `Prelude.hashWithSalt` identifier
        `Prelude.hashWithSalt` updateStatus

instance
  Prelude.NFData
    UpdateMediaInsightsPipelineStatus
  where
  rnf UpdateMediaInsightsPipelineStatus' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf updateStatus

instance
  Data.ToHeaders
    UpdateMediaInsightsPipelineStatus
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    UpdateMediaInsightsPipelineStatus
  where
  toJSON UpdateMediaInsightsPipelineStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("UpdateStatus" Data..= updateStatus)]
      )

instance
  Data.ToPath
    UpdateMediaInsightsPipelineStatus
  where
  toPath UpdateMediaInsightsPipelineStatus' {..} =
    Prelude.mconcat
      [ "/media-insights-pipeline-status/",
        Data.toBS identifier
      ]

instance
  Data.ToQuery
    UpdateMediaInsightsPipelineStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMediaInsightsPipelineStatusResponse' smart constructor.
data UpdateMediaInsightsPipelineStatusResponse = UpdateMediaInsightsPipelineStatusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMediaInsightsPipelineStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateMediaInsightsPipelineStatusResponse ::
  UpdateMediaInsightsPipelineStatusResponse
newUpdateMediaInsightsPipelineStatusResponse =
  UpdateMediaInsightsPipelineStatusResponse'

instance
  Prelude.NFData
    UpdateMediaInsightsPipelineStatusResponse
  where
  rnf _ = ()
