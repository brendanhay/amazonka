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
-- Module      : Amazonka.EC2.CancelExportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active export task. The request removes all artifacts of the
-- export, including any partially-created Amazon S3 objects. If the export
-- task is complete or is in the process of transferring the final disk
-- image, the command fails and returns an error.
module Amazonka.EC2.CancelExportTask
  ( -- * Creating a Request
    CancelExportTask (..),
    newCancelExportTask,

    -- * Request Lenses
    cancelExportTask_exportTaskId,

    -- * Destructuring the Response
    CancelExportTaskResponse (..),
    newCancelExportTaskResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelExportTask' smart constructor.
data CancelExportTask = CancelExportTask'
  { -- | The ID of the export task. This is the ID returned by
    -- @CreateInstanceExportTask@.
    exportTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportTaskId', 'cancelExportTask_exportTaskId' - The ID of the export task. This is the ID returned by
-- @CreateInstanceExportTask@.
newCancelExportTask ::
  -- | 'exportTaskId'
  Prelude.Text ->
  CancelExportTask
newCancelExportTask pExportTaskId_ =
  CancelExportTask' {exportTaskId = pExportTaskId_}

-- | The ID of the export task. This is the ID returned by
-- @CreateInstanceExportTask@.
cancelExportTask_exportTaskId :: Lens.Lens' CancelExportTask Prelude.Text
cancelExportTask_exportTaskId = Lens.lens (\CancelExportTask' {exportTaskId} -> exportTaskId) (\s@CancelExportTask' {} a -> s {exportTaskId = a} :: CancelExportTask)

instance Core.AWSRequest CancelExportTask where
  type
    AWSResponse CancelExportTask =
      CancelExportTaskResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull CancelExportTaskResponse'

instance Prelude.Hashable CancelExportTask where
  hashWithSalt _salt CancelExportTask' {..} =
    _salt `Prelude.hashWithSalt` exportTaskId

instance Prelude.NFData CancelExportTask where
  rnf CancelExportTask' {..} = Prelude.rnf exportTaskId

instance Data.ToHeaders CancelExportTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelExportTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelExportTask where
  toQuery CancelExportTask' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CancelExportTask" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ExportTaskId" Data.=: exportTaskId
      ]

-- | /See:/ 'newCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelExportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelExportTaskResponse ::
  CancelExportTaskResponse
newCancelExportTaskResponse =
  CancelExportTaskResponse'

instance Prelude.NFData CancelExportTaskResponse where
  rnf _ = ()
