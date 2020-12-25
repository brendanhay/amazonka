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
    Types.ExportTask (..),
    Types.mkExportTask,

    -- ** Response lenses
    Types.etExportOnly,
    Types.etExportTaskIdentifier,
    Types.etFailureCause,
    Types.etIamRoleArn,
    Types.etKmsKeyId,
    Types.etPercentProgress,
    Types.etS3Bucket,
    Types.etS3Prefix,
    Types.etSnapshotTime,
    Types.etSourceArn,
    Types.etStatus,
    Types.etTaskEndTime,
    Types.etTaskStartTime,
    Types.etTotalExtractedDataInGB,
    Types.etWarningMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { -- | The identifier of the snapshot export task to cancel.
    exportTaskIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelExportTask' value with any optional fields omitted.
mkCancelExportTask ::
  -- | 'exportTaskIdentifier'
  Types.String ->
  CancelExportTask
mkCancelExportTask exportTaskIdentifier =
  CancelExportTask' {exportTaskIdentifier}

-- | The identifier of the snapshot export task to cancel.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetExportTaskIdentifier :: Lens.Lens' CancelExportTask Types.String
cetExportTaskIdentifier = Lens.field @"exportTaskIdentifier"
{-# DEPRECATED cetExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

instance Core.AWSRequest CancelExportTask where
  type Rs CancelExportTask = Types.ExportTask
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CancelExportTask")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "ExportTaskIdentifier" exportTaskIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CancelExportTaskResult"
      (\s h x -> Core.parseXML x)
