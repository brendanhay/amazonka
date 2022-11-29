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
-- Module      : Amazonka.RDS.CancelExportTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an export task in progress that is exporting a snapshot to
-- Amazon S3. Any data that has already been written to the S3 bucket
-- isn\'t removed.
module Amazonka.RDS.CancelExportTask
  ( -- * Creating a Request
    CancelExportTask (..),
    newCancelExportTask,

    -- * Request Lenses
    cancelExportTask_exportTaskIdentifier,

    -- * Destructuring the Response
    ExportTask (..),
    newExportTask,

    -- * Response Lenses
    exportTask_s3Bucket,
    exportTask_percentProgress,
    exportTask_taskStartTime,
    exportTask_exportOnly,
    exportTask_sourceArn,
    exportTask_totalExtractedDataInGB,
    exportTask_status,
    exportTask_sourceType,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_exportTaskIdentifier,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_taskEndTime,
    exportTask_s3Prefix,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelExportTask' smart constructor.
data CancelExportTask = CancelExportTask'
  { -- | The identifier of the snapshot export task to cancel.
    exportTaskIdentifier :: Prelude.Text
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
-- 'exportTaskIdentifier', 'cancelExportTask_exportTaskIdentifier' - The identifier of the snapshot export task to cancel.
newCancelExportTask ::
  -- | 'exportTaskIdentifier'
  Prelude.Text ->
  CancelExportTask
newCancelExportTask pExportTaskIdentifier_ =
  CancelExportTask'
    { exportTaskIdentifier =
        pExportTaskIdentifier_
    }

-- | The identifier of the snapshot export task to cancel.
cancelExportTask_exportTaskIdentifier :: Lens.Lens' CancelExportTask Prelude.Text
cancelExportTask_exportTaskIdentifier = Lens.lens (\CancelExportTask' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@CancelExportTask' {} a -> s {exportTaskIdentifier = a} :: CancelExportTask)

instance Core.AWSRequest CancelExportTask where
  type AWSResponse CancelExportTask = ExportTask
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CancelExportTaskResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CancelExportTask where
  hashWithSalt _salt CancelExportTask' {..} =
    _salt `Prelude.hashWithSalt` exportTaskIdentifier

instance Prelude.NFData CancelExportTask where
  rnf CancelExportTask' {..} =
    Prelude.rnf exportTaskIdentifier

instance Core.ToHeaders CancelExportTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CancelExportTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelExportTask where
  toQuery CancelExportTask' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CancelExportTask" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "ExportTaskIdentifier" Core.=: exportTaskIdentifier
      ]
