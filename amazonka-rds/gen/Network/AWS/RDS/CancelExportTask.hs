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
-- Module      : Network.AWS.RDS.CancelExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an export task in progress that is exporting a snapshot to
-- Amazon S3. Any data that has already been written to the S3 bucket
-- isn\'t removed.
module Network.AWS.RDS.CancelExportTask
  ( -- * Creating a Request
    CancelExportTask (..),
    newCancelExportTask,

    -- * Request Lenses
    cancelExportTask_exportTaskIdentifier,

    -- * Destructuring the Response
    ExportTask (..),
    newExportTask,

    -- * Response Lenses
    exportTask_taskEndTime,
    exportTask_iamRoleArn,
    exportTask_status,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_s3Bucket,
    exportTask_exportOnly,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_percentProgress,
    exportTask_sourceArn,
    exportTask_s3Prefix,
    exportTask_taskStartTime,
    exportTask_exportTaskIdentifier,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelExportTask' smart constructor.
data CancelExportTask = CancelExportTask'
  { -- | The identifier of the snapshot export task to cancel.
    exportTaskIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  CancelExportTask
newCancelExportTask pExportTaskIdentifier_ =
  CancelExportTask'
    { exportTaskIdentifier =
        pExportTaskIdentifier_
    }

-- | The identifier of the snapshot export task to cancel.
cancelExportTask_exportTaskIdentifier :: Lens.Lens' CancelExportTask Core.Text
cancelExportTask_exportTaskIdentifier = Lens.lens (\CancelExportTask' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@CancelExportTask' {} a -> s {exportTaskIdentifier = a} :: CancelExportTask)

instance Core.AWSRequest CancelExportTask where
  type AWSResponse CancelExportTask = ExportTask
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CancelExportTaskResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable CancelExportTask

instance Core.NFData CancelExportTask

instance Core.ToHeaders CancelExportTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelExportTask where
  toPath = Core.const "/"

instance Core.ToQuery CancelExportTask where
  toQuery CancelExportTask' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CancelExportTask" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ExportTaskIdentifier" Core.=: exportTaskIdentifier
      ]
