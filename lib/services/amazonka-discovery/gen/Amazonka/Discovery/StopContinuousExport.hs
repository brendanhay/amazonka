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
-- Module      : Amazonka.Discovery.StopContinuousExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop the continuous flow of agent\'s discovered data into Amazon Athena.
module Amazonka.Discovery.StopContinuousExport
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopContinuousExport' smart constructor.
data StopContinuousExport = StopContinuousExport'
  { -- | The unique ID assigned to this export.
    exportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopContinuousExport
newStopContinuousExport pExportId_ =
  StopContinuousExport' {exportId = pExportId_}

-- | The unique ID assigned to this export.
stopContinuousExport_exportId :: Lens.Lens' StopContinuousExport Prelude.Text
stopContinuousExport_exportId = Lens.lens (\StopContinuousExport' {exportId} -> exportId) (\s@StopContinuousExport' {} a -> s {exportId = a} :: StopContinuousExport)

instance Core.AWSRequest StopContinuousExport where
  type
    AWSResponse StopContinuousExport =
      StopContinuousExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopContinuousExportResponse'
            Prelude.<$> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "stopTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopContinuousExport where
  hashWithSalt _salt StopContinuousExport' {..} =
    _salt `Prelude.hashWithSalt` exportId

instance Prelude.NFData StopContinuousExport where
  rnf StopContinuousExport' {..} = Prelude.rnf exportId

instance Data.ToHeaders StopContinuousExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.StopContinuousExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopContinuousExport where
  toJSON StopContinuousExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("exportId" Data..= exportId)]
      )

instance Data.ToPath StopContinuousExport where
  toPath = Prelude.const "/"

instance Data.ToQuery StopContinuousExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopContinuousExportResponse' smart constructor.
data StopContinuousExportResponse = StopContinuousExportResponse'
  { -- | Timestamp that represents when this continuous export started collecting
    -- data.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Timestamp that represents when this continuous export was stopped.
    stopTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StopContinuousExportResponse
newStopContinuousExportResponse pHttpStatus_ =
  StopContinuousExportResponse'
    { startTime =
        Prelude.Nothing,
      stopTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Timestamp that represents when this continuous export started collecting
-- data.
stopContinuousExportResponse_startTime :: Lens.Lens' StopContinuousExportResponse (Prelude.Maybe Prelude.UTCTime)
stopContinuousExportResponse_startTime = Lens.lens (\StopContinuousExportResponse' {startTime} -> startTime) (\s@StopContinuousExportResponse' {} a -> s {startTime = a} :: StopContinuousExportResponse) Prelude.. Lens.mapping Data._Time

-- | Timestamp that represents when this continuous export was stopped.
stopContinuousExportResponse_stopTime :: Lens.Lens' StopContinuousExportResponse (Prelude.Maybe Prelude.UTCTime)
stopContinuousExportResponse_stopTime = Lens.lens (\StopContinuousExportResponse' {stopTime} -> stopTime) (\s@StopContinuousExportResponse' {} a -> s {stopTime = a} :: StopContinuousExportResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
stopContinuousExportResponse_httpStatus :: Lens.Lens' StopContinuousExportResponse Prelude.Int
stopContinuousExportResponse_httpStatus = Lens.lens (\StopContinuousExportResponse' {httpStatus} -> httpStatus) (\s@StopContinuousExportResponse' {} a -> s {httpStatus = a} :: StopContinuousExportResponse)

instance Prelude.NFData StopContinuousExportResponse where
  rnf StopContinuousExportResponse' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf httpStatus
