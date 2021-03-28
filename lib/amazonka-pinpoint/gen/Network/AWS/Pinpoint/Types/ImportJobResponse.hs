{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ImportJobResponse
  ( ImportJobResponse (..)
  -- * Smart constructor
  , mkImportJobResponse
  -- * Lenses
  , ijrJobStatus
  , ijrCreationDate
  , ijrType
  , ijrDefinition
  , ijrId
  , ijrApplicationId
  , ijrCompletedPieces
  , ijrCompletionDate
  , ijrFailedPieces
  , ijrFailures
  , ijrTotalFailures
  , ijrTotalPieces
  , ijrTotalProcessed
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ImportJobResource as Types
import qualified Network.AWS.Pinpoint.Types.JobStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of a job that imports endpoint definitions from one or more files. The files can be stored in an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly from a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkImportJobResponse' smart constructor.
data ImportJobResponse = ImportJobResponse'
  { jobStatus :: Types.JobStatus
    -- ^ The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
  , creationDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the import job was created.
  , type' :: Core.Text
    -- ^ The job type. This value is IMPORT for import jobs.
  , definition :: Types.ImportJobResource
    -- ^ The resource settings that apply to the import job.
  , id :: Core.Text
    -- ^ The unique identifier for the import job.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application that's associated with the import job.
  , completedPieces :: Core.Maybe Core.Int
    -- ^ The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
  , completionDate :: Core.Maybe Core.Text
    -- ^ The date, in ISO 8601 format, when the import job was completed.
  , failedPieces :: Core.Maybe Core.Int
    -- ^ The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
  , failures :: Core.Maybe [Core.Text]
    -- ^ An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
  , totalFailures :: Core.Maybe Core.Int
    -- ^ The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
  , totalPieces :: Core.Maybe Core.Int
    -- ^ The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
  , totalProcessed :: Core.Maybe Core.Int
    -- ^ The total number of endpoint definitions that were processed by the import job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportJobResponse' value with any optional fields omitted.
mkImportJobResponse
    :: Types.JobStatus -- ^ 'jobStatus'
    -> Core.Text -- ^ 'creationDate'
    -> Core.Text -- ^ 'type\''
    -> Types.ImportJobResource -- ^ 'definition'
    -> Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'applicationId'
    -> ImportJobResponse
mkImportJobResponse jobStatus creationDate type' definition id
  applicationId
  = ImportJobResponse'{jobStatus, creationDate, type', definition,
                       id, applicationId, completedPieces = Core.Nothing,
                       completionDate = Core.Nothing, failedPieces = Core.Nothing,
                       failures = Core.Nothing, totalFailures = Core.Nothing,
                       totalPieces = Core.Nothing, totalProcessed = Core.Nothing}

-- | The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrJobStatus :: Lens.Lens' ImportJobResponse Types.JobStatus
ijrJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE ijrJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The date, in ISO 8601 format, when the import job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrCreationDate :: Lens.Lens' ImportJobResponse Core.Text
ijrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE ijrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The job type. This value is IMPORT for import jobs.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrType :: Lens.Lens' ImportJobResponse Core.Text
ijrType = Lens.field @"type'"
{-# INLINEABLE ijrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The resource settings that apply to the import job.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrDefinition :: Lens.Lens' ImportJobResponse Types.ImportJobResource
ijrDefinition = Lens.field @"definition"
{-# INLINEABLE ijrDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The unique identifier for the import job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrId :: Lens.Lens' ImportJobResponse Core.Text
ijrId = Lens.field @"id"
{-# INLINEABLE ijrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The unique identifier for the application that's associated with the import job.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrApplicationId :: Lens.Lens' ImportJobResponse Core.Text
ijrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ijrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
--
-- /Note:/ Consider using 'completedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrCompletedPieces :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrCompletedPieces = Lens.field @"completedPieces"
{-# INLINEABLE ijrCompletedPieces #-}
{-# DEPRECATED completedPieces "Use generic-lens or generic-optics with 'completedPieces' instead"  #-}

-- | The date, in ISO 8601 format, when the import job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrCompletionDate :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Text)
ijrCompletionDate = Lens.field @"completionDate"
{-# INLINEABLE ijrCompletionDate #-}
{-# DEPRECATED completionDate "Use generic-lens or generic-optics with 'completionDate' instead"  #-}

-- | The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
--
-- /Note:/ Consider using 'failedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrFailedPieces :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrFailedPieces = Lens.field @"failedPieces"
{-# INLINEABLE ijrFailedPieces #-}
{-# DEPRECATED failedPieces "Use generic-lens or generic-optics with 'failedPieces' instead"  #-}

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrFailures :: Lens.Lens' ImportJobResponse (Core.Maybe [Core.Text])
ijrFailures = Lens.field @"failures"
{-# INLINEABLE ijrFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
--
-- /Note:/ Consider using 'totalFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrTotalFailures :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrTotalFailures = Lens.field @"totalFailures"
{-# INLINEABLE ijrTotalFailures #-}
{-# DEPRECATED totalFailures "Use generic-lens or generic-optics with 'totalFailures' instead"  #-}

-- | The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
--
-- /Note:/ Consider using 'totalPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrTotalPieces :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrTotalPieces = Lens.field @"totalPieces"
{-# INLINEABLE ijrTotalPieces #-}
{-# DEPRECATED totalPieces "Use generic-lens or generic-optics with 'totalPieces' instead"  #-}

-- | The total number of endpoint definitions that were processed by the import job.
--
-- /Note:/ Consider using 'totalProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrTotalProcessed :: Lens.Lens' ImportJobResponse (Core.Maybe Core.Int)
ijrTotalProcessed = Lens.field @"totalProcessed"
{-# INLINEABLE ijrTotalProcessed #-}
{-# DEPRECATED totalProcessed "Use generic-lens or generic-optics with 'totalProcessed' instead"  #-}

instance Core.FromJSON ImportJobResponse where
        parseJSON
          = Core.withObject "ImportJobResponse" Core.$
              \ x ->
                ImportJobResponse' Core.<$>
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
