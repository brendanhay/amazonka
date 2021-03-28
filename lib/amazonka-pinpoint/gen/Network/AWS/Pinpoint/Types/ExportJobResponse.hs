{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ExportJobResponse
  ( ExportJobResponse (..)
  -- * Smart constructor
  , mkExportJobResponse
  -- * Lenses
  , ejrJobStatus
  , ejrCreationDate
  , ejrType
  , ejrDefinition
  , ejrId
  , ejrApplicationId
  , ejrCompletedPieces
  , ejrCompletionDate
  , ejrFailedPieces
  , ejrFailures
  , ejrTotalFailures
  , ejrTotalPieces
  , ejrTotalProcessed
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ExportJobResource as Types
import qualified Network.AWS.Pinpoint.Types.JobStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of a job that exports endpoint definitions to a file. The file can be added directly to an Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon Pinpoint API or downloaded directly to a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkExportJobResponse' smart constructor.
data ExportJobResponse = ExportJobResponse'
  { jobStatus :: Types.JobStatus
    -- ^ The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
  , creationDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the export job was created.
  , type' :: Core.Text
    -- ^ The job type. This value is EXPORT for export jobs.
  , definition :: Types.ExportJobResource
    -- ^ The resource settings that apply to the export job.
  , id :: Core.Text
    -- ^ The unique identifier for the export job.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application that's associated with the export job.
  , completedPieces :: Core.Maybe Core.Int
    -- ^ The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
  , completionDate :: Core.Maybe Core.Text
    -- ^ The date, in ISO 8601 format, when the export job was completed.
  , failedPieces :: Core.Maybe Core.Int
    -- ^ The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
  , failures :: Core.Maybe [Core.Text]
    -- ^ An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
  , totalFailures :: Core.Maybe Core.Int
    -- ^ The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
  , totalPieces :: Core.Maybe Core.Int
    -- ^ The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
  , totalProcessed :: Core.Maybe Core.Int
    -- ^ The total number of endpoint definitions that were processed by the export job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportJobResponse' value with any optional fields omitted.
mkExportJobResponse
    :: Types.JobStatus -- ^ 'jobStatus'
    -> Core.Text -- ^ 'creationDate'
    -> Core.Text -- ^ 'type\''
    -> Types.ExportJobResource -- ^ 'definition'
    -> Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'applicationId'
    -> ExportJobResponse
mkExportJobResponse jobStatus creationDate type' definition id
  applicationId
  = ExportJobResponse'{jobStatus, creationDate, type', definition,
                       id, applicationId, completedPieces = Core.Nothing,
                       completionDate = Core.Nothing, failedPieces = Core.Nothing,
                       failures = Core.Nothing, totalFailures = Core.Nothing,
                       totalPieces = Core.Nothing, totalProcessed = Core.Nothing}

-- | The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrJobStatus :: Lens.Lens' ExportJobResponse Types.JobStatus
ejrJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE ejrJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The date, in ISO 8601 format, when the export job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrCreationDate :: Lens.Lens' ExportJobResponse Core.Text
ejrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE ejrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The job type. This value is EXPORT for export jobs.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrType :: Lens.Lens' ExportJobResponse Core.Text
ejrType = Lens.field @"type'"
{-# INLINEABLE ejrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The resource settings that apply to the export job.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrDefinition :: Lens.Lens' ExportJobResponse Types.ExportJobResource
ejrDefinition = Lens.field @"definition"
{-# INLINEABLE ejrDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The unique identifier for the export job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrId :: Lens.Lens' ExportJobResponse Core.Text
ejrId = Lens.field @"id"
{-# INLINEABLE ejrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The unique identifier for the application that's associated with the export job.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrApplicationId :: Lens.Lens' ExportJobResponse Core.Text
ejrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ejrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
--
-- /Note:/ Consider using 'completedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrCompletedPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrCompletedPieces = Lens.field @"completedPieces"
{-# INLINEABLE ejrCompletedPieces #-}
{-# DEPRECATED completedPieces "Use generic-lens or generic-optics with 'completedPieces' instead"  #-}

-- | The date, in ISO 8601 format, when the export job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrCompletionDate :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Text)
ejrCompletionDate = Lens.field @"completionDate"
{-# INLINEABLE ejrCompletionDate #-}
{-# DEPRECATED completionDate "Use generic-lens or generic-optics with 'completionDate' instead"  #-}

-- | The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
--
-- /Note:/ Consider using 'failedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrFailedPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrFailedPieces = Lens.field @"failedPieces"
{-# INLINEABLE ejrFailedPieces #-}
{-# DEPRECATED failedPieces "Use generic-lens or generic-optics with 'failedPieces' instead"  #-}

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrFailures :: Lens.Lens' ExportJobResponse (Core.Maybe [Core.Text])
ejrFailures = Lens.field @"failures"
{-# INLINEABLE ejrFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
--
-- /Note:/ Consider using 'totalFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrTotalFailures :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrTotalFailures = Lens.field @"totalFailures"
{-# INLINEABLE ejrTotalFailures #-}
{-# DEPRECATED totalFailures "Use generic-lens or generic-optics with 'totalFailures' instead"  #-}

-- | The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
--
-- /Note:/ Consider using 'totalPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrTotalPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrTotalPieces = Lens.field @"totalPieces"
{-# INLINEABLE ejrTotalPieces #-}
{-# DEPRECATED totalPieces "Use generic-lens or generic-optics with 'totalPieces' instead"  #-}

-- | The total number of endpoint definitions that were processed by the export job.
--
-- /Note:/ Consider using 'totalProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrTotalProcessed :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrTotalProcessed = Lens.field @"totalProcessed"
{-# INLINEABLE ejrTotalProcessed #-}
{-# DEPRECATED totalProcessed "Use generic-lens or generic-optics with 'totalProcessed' instead"  #-}

instance Core.FromJSON ExportJobResponse where
        parseJSON
          = Core.withObject "ExportJobResponse" Core.$
              \ x ->
                ExportJobResponse' Core.<$>
                  (x Core..: "JobStatus") Core.<*> x Core..: "CreationDate" Core.<*>
                    x Core..: "Type"
                    Core.<*> x Core..: "Definition"
                    Core.<*> x Core..: "Id"
                    Core.<*> x Core..: "ApplicationId"
                    Core.<*> x Core..:? "CompletedPieces"
                    Core.<*> x Core..:? "CompletionDate"
                    Core.<*> x Core..:? "FailedPieces"
                    Core.<*> x Core..:? "Failures"
                    Core.<*> x Core..:? "TotalFailures"
                    Core.<*> x Core..:? "TotalPieces"
                    Core.<*> x Core..:? "TotalProcessed"
