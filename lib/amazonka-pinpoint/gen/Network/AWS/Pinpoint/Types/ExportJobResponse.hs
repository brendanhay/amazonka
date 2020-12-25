{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobResponse
  ( ExportJobResponse (..),

    -- * Smart constructor
    mkExportJobResponse,

    -- * Lenses
    ejrJobStatus,
    ejrCreationDate,
    ejrType,
    ejrDefinition,
    ejrId,
    ejrApplicationId,
    ejrCompletedPieces,
    ejrCompletionDate,
    ejrFailedPieces,
    ejrFailures,
    ejrTotalFailures,
    ejrTotalPieces,
    ejrTotalProcessed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ExportJobResource as Types
import qualified Network.AWS.Pinpoint.Types.JobStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of a job that exports endpoint definitions to a file. The file can be added directly to an Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon Pinpoint API or downloaded directly to a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkExportJobResponse' smart constructor.
data ExportJobResponse = ExportJobResponse'
  { -- | The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
    jobStatus :: Types.JobStatus,
    -- | The date, in ISO 8601 format, when the export job was created.
    creationDate :: Core.Text,
    -- | The job type. This value is EXPORT for export jobs.
    type' :: Core.Text,
    -- | The resource settings that apply to the export job.
    definition :: Types.ExportJobResource,
    -- | The unique identifier for the export job.
    id :: Core.Text,
    -- | The unique identifier for the application that's associated with the export job.
    applicationId :: Core.Text,
    -- | The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
    completedPieces :: Core.Maybe Core.Int,
    -- | The date, in ISO 8601 format, when the export job was completed.
    completionDate :: Core.Maybe Core.Text,
    -- | The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
    failedPieces :: Core.Maybe Core.Int,
    -- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
    failures :: Core.Maybe [Core.Text],
    -- | The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
    totalFailures :: Core.Maybe Core.Int,
    -- | The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
    totalPieces :: Core.Maybe Core.Int,
    -- | The total number of endpoint definitions that were processed by the export job.
    totalProcessed :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportJobResponse' value with any optional fields omitted.
mkExportJobResponse ::
  -- | 'jobStatus'
  Types.JobStatus ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'type\''
  Core.Text ->
  -- | 'definition'
  Types.ExportJobResource ->
  -- | 'id'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  ExportJobResponse
mkExportJobResponse
  jobStatus
  creationDate
  type'
  definition
  id
  applicationId =
    ExportJobResponse'
      { jobStatus,
        creationDate,
        type',
        definition,
        id,
        applicationId,
        completedPieces = Core.Nothing,
        completionDate = Core.Nothing,
        failedPieces = Core.Nothing,
        failures = Core.Nothing,
        totalFailures = Core.Nothing,
        totalPieces = Core.Nothing,
        totalProcessed = Core.Nothing
      }

-- | The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrJobStatus :: Lens.Lens' ExportJobResponse Types.JobStatus
ejrJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED ejrJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The date, in ISO 8601 format, when the export job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrCreationDate :: Lens.Lens' ExportJobResponse Core.Text
ejrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED ejrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The job type. This value is EXPORT for export jobs.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrType :: Lens.Lens' ExportJobResponse Core.Text
ejrType = Lens.field @"type'"
{-# DEPRECATED ejrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The resource settings that apply to the export job.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrDefinition :: Lens.Lens' ExportJobResponse Types.ExportJobResource
ejrDefinition = Lens.field @"definition"
{-# DEPRECATED ejrDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The unique identifier for the export job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrId :: Lens.Lens' ExportJobResponse Core.Text
ejrId = Lens.field @"id"
{-# DEPRECATED ejrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique identifier for the application that's associated with the export job.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrApplicationId :: Lens.Lens' ExportJobResponse Core.Text
ejrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ejrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
--
-- /Note:/ Consider using 'completedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrCompletedPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrCompletedPieces = Lens.field @"completedPieces"
{-# DEPRECATED ejrCompletedPieces "Use generic-lens or generic-optics with 'completedPieces' instead." #-}

-- | The date, in ISO 8601 format, when the export job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrCompletionDate :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Text)
ejrCompletionDate = Lens.field @"completionDate"
{-# DEPRECATED ejrCompletionDate "Use generic-lens or generic-optics with 'completionDate' instead." #-}

-- | The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
--
-- /Note:/ Consider using 'failedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrFailedPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrFailedPieces = Lens.field @"failedPieces"
{-# DEPRECATED ejrFailedPieces "Use generic-lens or generic-optics with 'failedPieces' instead." #-}

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrFailures :: Lens.Lens' ExportJobResponse (Core.Maybe [Core.Text])
ejrFailures = Lens.field @"failures"
{-# DEPRECATED ejrFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
--
-- /Note:/ Consider using 'totalFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrTotalFailures :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrTotalFailures = Lens.field @"totalFailures"
{-# DEPRECATED ejrTotalFailures "Use generic-lens or generic-optics with 'totalFailures' instead." #-}

-- | The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
--
-- /Note:/ Consider using 'totalPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrTotalPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrTotalPieces = Lens.field @"totalPieces"
{-# DEPRECATED ejrTotalPieces "Use generic-lens or generic-optics with 'totalPieces' instead." #-}

-- | The total number of endpoint definitions that were processed by the export job.
--
-- /Note:/ Consider using 'totalProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrTotalProcessed :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
ejrTotalProcessed = Lens.field @"totalProcessed"
{-# DEPRECATED ejrTotalProcessed "Use generic-lens or generic-optics with 'totalProcessed' instead." #-}

instance Core.FromJSON ExportJobResponse where
  parseJSON =
    Core.withObject "ExportJobResponse" Core.$
      \x ->
        ExportJobResponse'
          Core.<$> (x Core..: "JobStatus")
          Core.<*> (x Core..: "CreationDate")
          Core.<*> (x Core..: "Type")
          Core.<*> (x Core..: "Definition")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..: "ApplicationId")
          Core.<*> (x Core..:? "CompletedPieces")
          Core.<*> (x Core..:? "CompletionDate")
          Core.<*> (x Core..:? "FailedPieces")
          Core.<*> (x Core..:? "Failures")
          Core.<*> (x Core..:? "TotalFailures")
          Core.<*> (x Core..:? "TotalPieces")
          Core.<*> (x Core..:? "TotalProcessed")
