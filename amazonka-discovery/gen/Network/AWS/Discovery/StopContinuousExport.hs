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

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopContinuousExport' smart constructor.
data StopContinuousExport = StopContinuousExport'
  { -- | The unique ID assigned to this export.
    exportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StopContinuousExport where
  type
    Rs StopContinuousExport =
      StopContinuousExportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopContinuousExportResponse'
            Prelude.<$> (x Prelude..?> "startTime")
            Prelude.<*> (x Prelude..?> "stopTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopContinuousExport

instance Prelude.NFData StopContinuousExport

instance Prelude.ToHeaders StopContinuousExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSPoseidonService_V2015_11_01.StopContinuousExport" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopContinuousExport where
  toJSON StopContinuousExport' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("exportId" Prelude..= exportId)]
      )

instance Prelude.ToPath StopContinuousExport where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopContinuousExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopContinuousExportResponse' smart constructor.
data StopContinuousExportResponse = StopContinuousExportResponse'
  { -- | Timestamp that represents when this continuous export started collecting
    -- data.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | Timestamp that represents when this continuous export was stopped.
    stopTime :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
stopContinuousExportResponse_startTime = Lens.lens (\StopContinuousExportResponse' {startTime} -> startTime) (\s@StopContinuousExportResponse' {} a -> s {startTime = a} :: StopContinuousExportResponse) Prelude.. Lens.mapping Prelude._Time

-- | Timestamp that represents when this continuous export was stopped.
stopContinuousExportResponse_stopTime :: Lens.Lens' StopContinuousExportResponse (Prelude.Maybe Prelude.UTCTime)
stopContinuousExportResponse_stopTime = Lens.lens (\StopContinuousExportResponse' {stopTime} -> stopTime) (\s@StopContinuousExportResponse' {} a -> s {stopTime = a} :: StopContinuousExportResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
stopContinuousExportResponse_httpStatus :: Lens.Lens' StopContinuousExportResponse Prelude.Int
stopContinuousExportResponse_httpStatus = Lens.lens (\StopContinuousExportResponse' {httpStatus} -> httpStatus) (\s@StopContinuousExportResponse' {} a -> s {httpStatus = a} :: StopContinuousExportResponse)

instance Prelude.NFData StopContinuousExportResponse
