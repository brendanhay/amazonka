{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobResponse
  ( ImportJobResponse (..),

    -- * Smart constructor
    mkImportJobResponse,

    -- * Lenses
    ijrJobStatus,
    ijrCreationDate,
    ijrType,
    ijrDefinition,
    ijrId,
    ijrApplicationId,
    ijrCompletedPieces,
    ijrCompletionDate,
    ijrFailedPieces,
    ijrFailures,
    ijrTotalFailures,
    ijrTotalPieces,
    ijrTotalProcessed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ImportJobResource as Types
import qualified Network.AWS.Pinpoint.Types.JobStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of a job that imports endpoint definitions from one or more files. The files can be stored in an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly from a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkImportJobResponse' smart constructor.
data ImportJobResponse = ImportJobResponse'
  { -- | The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
    jobStatus :: Types.JobStatus,
    -- | The date, in ISO 8601 format, when the import job was created.
    creationDate :: Core.Text,
    -- | The job type. This value is IMPORT for import jobs.
    type' :: Core.Text,
    -- | The resource settings that apply to the import job.
    definition :: Types.ImportJobResource,
    -- | The unique identifier for the import job.
    id :: Core.Text,
    -- | The unique identifier for the application that's associated with the import job.
    applicationId :: Core.Text,
    -- | The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
    completedPieces :: Core.Maybe Core.Int,
    -- | The date, in ISO 8601 format, when the import job was completed.
    completionDate :: Core.Maybe Core.Text,
    -- | The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
    failedPieces :: Core.Maybe Core.Int,
    -- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
    failures :: Core.Maybe [Core.Text],
    -- | The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
    totalFailures :: Core.Maybe Core.Int,
    -- | The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
    totalPieces :: Core.Maybe Core.Int,
    -- | The total number of endpoint definitions that were processed by the import job.
    totalProcessed :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportJobResponse' value with any optional fields omitted.
mkImportJobResponse ::
  -- | 'jobStatus'
  Types.JobStatus ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'type\''
  Core.Text ->
  -- | 'definition'
  Types.ImportJobResource ->
  -- | 'id'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  ImportJobResponse
mkImportJobResponse
  jobStatus
  creationDate
  type'
  definition
  id
  applicationId =
    ImportJobResponse'
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

-- | The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrJobStatus :: Lens.Lens' ImportJobResponse Types.JobStatus
ijrJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED ijrJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The date, in ISO 8601 format, when the import job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrCreationDate :: Lens.Lens' ImportJobResponse Core.Text
ijrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED ijrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The job type. This value is IMPORT for import jobs.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrType :: Lens.Lens' ImportJobResponse Core.Text
ijrType = Lens.field @"type'"
{-# DEPRECATED ijrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The resource settings that apply to the import job.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrDefinition :: Lens.Lens' ImportJobResponse Types.ImportJobResource
ijrDefinition = Lens.field @"definition"
{-# DEPRECATED ijrDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The unique identifier for the import job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrId :: Lens.Lens' ImportJobResponse Core.Text
ijrId = Lens.field @"id"
{-# DEPRECATED ijrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique identifier for the application that's associated with the import job.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrApplicationId :: Lens.Lens' ImportJobResponse Core.Text
ijrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ijrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
--
-- /Note:/ Consider using 'completedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrCompletedPieces :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrCompletedPieces = Lens.field @"completedPieces"
{-# DEPRECATED ijrCompletedPieces "Use generic-lens or generic-optics with 'completedPieces' instead." #-}

-- | The date, in ISO 8601 format, when the import job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrCompletionDate :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Text)
ijrCompletionDate = Lens.field @"completionDate"
{-# DEPRECATED ijrCompletionDate "Use generic-lens or generic-optics with 'completionDate' instead." #-}

-- | The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
--
-- /Note:/ Consider using 'failedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrFailedPieces :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrFailedPieces = Lens.field @"failedPieces"
{-# DEPRECATED ijrFailedPieces "Use generic-lens or generic-optics with 'failedPieces' instead." #-}

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrFailures :: Lens.Lens' ImportJobResponse (Core.Maybe [Core.Text])
ijrFailures = Lens.field @"failures"
{-# DEPRECATED ijrFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
--
-- /Note:/ Consider using 'totalFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrTotalFailures :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrTotalFailures = Lens.field @"totalFailures"
{-# DEPRECATED ijrTotalFailures "Use generic-lens or generic-optics with 'totalFailures' instead." #-}

-- | The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
--
-- /Note:/ Consider using 'totalPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrTotalPieces :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrTotalPieces = Lens.field @"totalPieces"
{-# DEPRECATED ijrTotalPieces "Use generic-lens or generic-optics with 'totalPieces' instead." #-}

-- | The total number of endpoint definitions that were processed by the import job.
--
-- /Note:/ Consider using 'totalProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrTotalProcessed :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrTotalProcessed = Lens.field @"totalProcessed"
{-# DEPRECATED ijrTotalProcessed "Use generic-lens or generic-optics with 'totalProcessed' instead." #-}

instance Core.FromJSON ImportJobResponse where
  parseJSON =
    Core.withObject "ImportJobResponse" Core.$
      \x ->
        ImportJobResponse'
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
