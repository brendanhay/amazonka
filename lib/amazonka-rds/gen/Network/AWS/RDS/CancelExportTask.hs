{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CancelExportTask (..)
    , mkCancelExportTask
    -- ** Request lenses
    , cetExportTaskIdentifier

     -- * Destructuring the response
    , Types.ExportTask (..)
    , Types.mkExportTask
    -- ** Response lenses
    , Types.etExportOnly
    , Types.etExportTaskIdentifier
    , Types.etFailureCause
    , Types.etIamRoleArn
    , Types.etKmsKeyId
    , Types.etPercentProgress
    , Types.etS3Bucket
    , Types.etS3Prefix
    , Types.etSnapshotTime
    , Types.etSourceArn
    , Types.etStatus
    , Types.etTaskEndTime
    , Types.etTaskStartTime
    , Types.etTotalExtractedDataInGB
    , Types.etWarningMessage
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { exportTaskIdentifier :: Core.Text
    -- ^ The identifier of the snapshot export task to cancel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelExportTask' value with any optional fields omitted.
mkCancelExportTask
    :: Core.Text -- ^ 'exportTaskIdentifier'
    -> CancelExportTask
mkCancelExportTask exportTaskIdentifier
  = CancelExportTask'{exportTaskIdentifier}

-- | The identifier of the snapshot export task to cancel.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetExportTaskIdentifier :: Lens.Lens' CancelExportTask Core.Text
cetExportTaskIdentifier = Lens.field @"exportTaskIdentifier"
{-# INLINEABLE cetExportTaskIdentifier #-}
{-# DEPRECATED exportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead"  #-}

instance Core.ToQuery CancelExportTask where
        toQuery CancelExportTask{..}
          = Core.toQueryPair "Action" ("CancelExportTask" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "ExportTaskIdentifier" exportTaskIdentifier

instance Core.ToHeaders CancelExportTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelExportTask where
        type Rs CancelExportTask = Types.ExportTask
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CancelExportTaskResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
