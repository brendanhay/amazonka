{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CancelExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an export task in progress that is exporting a snapshot to Amazon S3. Any data that has already been written to the S3 bucket isn't removed.
module Network.AWS.RDS.CancelExportTask
  ( -- * Creating a request
    CancelExportTask (..),
    mkCancelExportTask,

    -- ** Request lenses
    cetExportTaskIdentifier,

    -- * Destructuring the response
    ExportTask (..),
    mkExportTask,

    -- ** Response lenses
    etTotalExtractedDataInGB,
    etStatus,
    etIAMRoleARN,
    etSourceARN,
    etExportOnly,
    etTaskStartTime,
    etWarningMessage,
    etSnapshotTime,
    etKMSKeyId,
    etTaskEndTime,
    etExportTaskIdentifier,
    etS3Prefix,
    etPercentProgress,
    etS3Bucket,
    etFailureCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { -- | The identifier of the snapshot export task to cancel.
    exportTaskIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelExportTask' with the minimum fields required to make a request.
--
-- * 'exportTaskIdentifier' - The identifier of the snapshot export task to cancel.
mkCancelExportTask ::
  -- | 'exportTaskIdentifier'
  Lude.Text ->
  CancelExportTask
mkCancelExportTask pExportTaskIdentifier_ =
  CancelExportTask' {exportTaskIdentifier = pExportTaskIdentifier_}

-- | The identifier of the snapshot export task to cancel.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetExportTaskIdentifier :: Lens.Lens' CancelExportTask Lude.Text
cetExportTaskIdentifier = Lens.lens (exportTaskIdentifier :: CancelExportTask -> Lude.Text) (\s a -> s {exportTaskIdentifier = a} :: CancelExportTask)
{-# DEPRECATED cetExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

instance Lude.AWSRequest CancelExportTask where
  type Rs CancelExportTask = ExportTask
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CancelExportTaskResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CancelExportTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelExportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelExportTask where
  toQuery CancelExportTask' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelExportTask" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ExportTaskIdentifier" Lude.=: exportTaskIdentifier
      ]
