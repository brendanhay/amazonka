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
-- Module      : Network.AWS.Discovery.StopContinuousExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop the continuous flow of agent\'s discovered data into Amazon Athena.
module Network.AWS.Discovery.StopContinuousExport
  ( -- * Creating a Request
    StopContinuousExport (..),
    newStopContinuousExport,

    -- * Request Lenses
    stopContinuousExport_exportId,

    -- * Destructuring the Response
    StopContinuousExportResponse (..),
    newStopContinuousExportResponse,

    -- * Response Lenses
    stopContinuousExportResponse_startTime,
    stopContinuousExportResponse_stopTime,
    stopContinuousExportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopContinuousExport' smart constructor.
data StopContinuousExport = StopContinuousExport'
  { -- | The unique ID assigned to this export.
    exportId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopContinuousExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportId', 'stopContinuousExport_exportId' - The unique ID assigned to this export.
newStopContinuousExport ::
  -- | 'exportId'
  Core.Text ->
  StopContinuousExport
newStopContinuousExport pExportId_ =
  StopContinuousExport' {exportId = pExportId_}

-- | The unique ID assigned to this export.
stopContinuousExport_exportId :: Lens.Lens' StopContinuousExport Core.Text
stopContinuousExport_exportId = Lens.lens (\StopContinuousExport' {exportId} -> exportId) (\s@StopContinuousExport' {} a -> s {exportId = a} :: StopContinuousExport)

instance Core.AWSRequest StopContinuousExport where
  type
    AWSResponse StopContinuousExport =
      StopContinuousExportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopContinuousExportResponse'
            Core.<$> (x Core..?> "startTime")
            Core.<*> (x Core..?> "stopTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopContinuousExport

instance Core.NFData StopContinuousExport

instance Core.ToHeaders StopContinuousExport where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.StopContinuousExport" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopContinuousExport where
  toJSON StopContinuousExport' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("exportId" Core..= exportId)]
      )

instance Core.ToPath StopContinuousExport where
  toPath = Core.const "/"

instance Core.ToQuery StopContinuousExport where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopContinuousExportResponse' smart constructor.
data StopContinuousExportResponse = StopContinuousExportResponse'
  { -- | Timestamp that represents when this continuous export started collecting
    -- data.
    startTime :: Core.Maybe Core.POSIX,
    -- | Timestamp that represents when this continuous export was stopped.
    stopTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopContinuousExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'stopContinuousExportResponse_startTime' - Timestamp that represents when this continuous export started collecting
-- data.
--
-- 'stopTime', 'stopContinuousExportResponse_stopTime' - Timestamp that represents when this continuous export was stopped.
--
-- 'httpStatus', 'stopContinuousExportResponse_httpStatus' - The response's http status code.
newStopContinuousExportResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopContinuousExportResponse
newStopContinuousExportResponse pHttpStatus_ =
  StopContinuousExportResponse'
    { startTime =
        Core.Nothing,
      stopTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Timestamp that represents when this continuous export started collecting
-- data.
stopContinuousExportResponse_startTime :: Lens.Lens' StopContinuousExportResponse (Core.Maybe Core.UTCTime)
stopContinuousExportResponse_startTime = Lens.lens (\StopContinuousExportResponse' {startTime} -> startTime) (\s@StopContinuousExportResponse' {} a -> s {startTime = a} :: StopContinuousExportResponse) Core.. Lens.mapping Core._Time

-- | Timestamp that represents when this continuous export was stopped.
stopContinuousExportResponse_stopTime :: Lens.Lens' StopContinuousExportResponse (Core.Maybe Core.UTCTime)
stopContinuousExportResponse_stopTime = Lens.lens (\StopContinuousExportResponse' {stopTime} -> stopTime) (\s@StopContinuousExportResponse' {} a -> s {stopTime = a} :: StopContinuousExportResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
stopContinuousExportResponse_httpStatus :: Lens.Lens' StopContinuousExportResponse Core.Int
stopContinuousExportResponse_httpStatus = Lens.lens (\StopContinuousExportResponse' {httpStatus} -> httpStatus) (\s@StopContinuousExportResponse' {} a -> s {httpStatus = a} :: StopContinuousExportResponse)

instance Core.NFData StopContinuousExportResponse
