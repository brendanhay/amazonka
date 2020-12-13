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
    ejCompletedPieces,
    ejFailedPieces,
    ejDefinition,
    ejTotalProcessed,
    ejFailures,
    ejTotalPieces,
    ejApplicationId,
    ejId,
    ejCreationDate,
    ejType,
    ejCompletionDate,
    ejJobStatus,
    ejTotalFailures,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ExportJobResource
import Network.AWS.Pinpoint.Types.JobStatus
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of a job that exports endpoint definitions to a file. The file can be added directly to an Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon Pinpoint API or downloaded directly to a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkExportJobResponse' smart constructor.
data ExportJobResponse = ExportJobResponse'
  { -- | The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
    completedPieces :: Lude.Maybe Lude.Int,
    -- | The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
    failedPieces :: Lude.Maybe Lude.Int,
    -- | The resource settings that apply to the export job.
    definition :: ExportJobResource,
    -- | The total number of endpoint definitions that were processed by the export job.
    totalProcessed :: Lude.Maybe Lude.Int,
    -- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
    failures :: Lude.Maybe [Lude.Text],
    -- | The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
    totalPieces :: Lude.Maybe Lude.Int,
    -- | The unique identifier for the application that's associated with the export job.
    applicationId :: Lude.Text,
    -- | The unique identifier for the export job.
    id :: Lude.Text,
    -- | The date, in ISO 8601 format, when the export job was created.
    creationDate :: Lude.Text,
    -- | The job type. This value is EXPORT for export jobs.
    type' :: Lude.Text,
    -- | The date, in ISO 8601 format, when the export job was completed.
    completionDate :: Lude.Maybe Lude.Text,
    -- | The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
    jobStatus :: JobStatus,
    -- | The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
    totalFailures :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportJobResponse' with the minimum fields required to make a request.
--
-- * 'completedPieces' - The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
-- * 'failedPieces' - The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
-- * 'definition' - The resource settings that apply to the export job.
-- * 'totalProcessed' - The total number of endpoint definitions that were processed by the export job.
-- * 'failures' - An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
-- * 'totalPieces' - The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
-- * 'applicationId' - The unique identifier for the application that's associated with the export job.
-- * 'id' - The unique identifier for the export job.
-- * 'creationDate' - The date, in ISO 8601 format, when the export job was created.
-- * 'type'' - The job type. This value is EXPORT for export jobs.
-- * 'completionDate' - The date, in ISO 8601 format, when the export job was completed.
-- * 'jobStatus' - The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
-- * 'totalFailures' - The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
mkExportJobResponse ::
  -- | 'definition'
  ExportJobResource ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  ExportJobResponse
mkExportJobResponse
  pDefinition_
  pApplicationId_
  pId_
  pCreationDate_
  pType_
  pJobStatus_ =
    ExportJobResponse'
      { completedPieces = Lude.Nothing,
        failedPieces = Lude.Nothing,
        definition = pDefinition_,
        totalProcessed = Lude.Nothing,
        failures = Lude.Nothing,
        totalPieces = Lude.Nothing,
        applicationId = pApplicationId_,
        id = pId_,
        creationDate = pCreationDate_,
        type' = pType_,
        completionDate = Lude.Nothing,
        jobStatus = pJobStatus_,
        totalFailures = Lude.Nothing
      }

-- | The number of pieces that were processed successfully (completed) by the export job, as of the time of the request.
--
-- /Note:/ Consider using 'completedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejCompletedPieces :: Lens.Lens' ExportJobResponse (Lude.Maybe Lude.Int)
ejCompletedPieces = Lens.lens (completedPieces :: ExportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {completedPieces = a} :: ExportJobResponse)
{-# DEPRECATED ejCompletedPieces "Use generic-lens or generic-optics with 'completedPieces' instead." #-}

-- | The number of pieces that weren't processed successfully (failed) by the export job, as of the time of the request.
--
-- /Note:/ Consider using 'failedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejFailedPieces :: Lens.Lens' ExportJobResponse (Lude.Maybe Lude.Int)
ejFailedPieces = Lens.lens (failedPieces :: ExportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {failedPieces = a} :: ExportJobResponse)
{-# DEPRECATED ejFailedPieces "Use generic-lens or generic-optics with 'failedPieces' instead." #-}

-- | The resource settings that apply to the export job.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejDefinition :: Lens.Lens' ExportJobResponse ExportJobResource
ejDefinition = Lens.lens (definition :: ExportJobResponse -> ExportJobResource) (\s a -> s {definition = a} :: ExportJobResponse)
{-# DEPRECATED ejDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The total number of endpoint definitions that were processed by the export job.
--
-- /Note:/ Consider using 'totalProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejTotalProcessed :: Lens.Lens' ExportJobResponse (Lude.Maybe Lude.Int)
ejTotalProcessed = Lens.lens (totalProcessed :: ExportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalProcessed = a} :: ExportJobResponse)
{-# DEPRECATED ejTotalProcessed "Use generic-lens or generic-optics with 'totalProcessed' instead." #-}

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the export job, if any.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejFailures :: Lens.Lens' ExportJobResponse (Lude.Maybe [Lude.Text])
ejFailures = Lens.lens (failures :: ExportJobResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {failures = a} :: ExportJobResponse)
{-# DEPRECATED ejFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The total number of pieces that must be processed to complete the export job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the export job.
--
-- /Note:/ Consider using 'totalPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejTotalPieces :: Lens.Lens' ExportJobResponse (Lude.Maybe Lude.Int)
ejTotalPieces = Lens.lens (totalPieces :: ExportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalPieces = a} :: ExportJobResponse)
{-# DEPRECATED ejTotalPieces "Use generic-lens or generic-optics with 'totalPieces' instead." #-}

-- | The unique identifier for the application that's associated with the export job.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejApplicationId :: Lens.Lens' ExportJobResponse Lude.Text
ejApplicationId = Lens.lens (applicationId :: ExportJobResponse -> Lude.Text) (\s a -> s {applicationId = a} :: ExportJobResponse)
{-# DEPRECATED ejApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the export job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejId :: Lens.Lens' ExportJobResponse Lude.Text
ejId = Lens.lens (id :: ExportJobResponse -> Lude.Text) (\s a -> s {id = a} :: ExportJobResponse)
{-# DEPRECATED ejId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date, in ISO 8601 format, when the export job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejCreationDate :: Lens.Lens' ExportJobResponse Lude.Text
ejCreationDate = Lens.lens (creationDate :: ExportJobResponse -> Lude.Text) (\s a -> s {creationDate = a} :: ExportJobResponse)
{-# DEPRECATED ejCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The job type. This value is EXPORT for export jobs.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejType :: Lens.Lens' ExportJobResponse Lude.Text
ejType = Lens.lens (type' :: ExportJobResponse -> Lude.Text) (\s a -> s {type' = a} :: ExportJobResponse)
{-# DEPRECATED ejType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date, in ISO 8601 format, when the export job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejCompletionDate :: Lens.Lens' ExportJobResponse (Lude.Maybe Lude.Text)
ejCompletionDate = Lens.lens (completionDate :: ExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {completionDate = a} :: ExportJobResponse)
{-# DEPRECATED ejCompletionDate "Use generic-lens or generic-optics with 'completionDate' instead." #-}

-- | The status of the export job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejJobStatus :: Lens.Lens' ExportJobResponse JobStatus
ejJobStatus = Lens.lens (jobStatus :: ExportJobResponse -> JobStatus) (\s a -> s {jobStatus = a} :: ExportJobResponse)
{-# DEPRECATED ejJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the export job, typically because an error, such as a syntax error, occurred.
--
-- /Note:/ Consider using 'totalFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejTotalFailures :: Lens.Lens' ExportJobResponse (Lude.Maybe Lude.Int)
ejTotalFailures = Lens.lens (totalFailures :: ExportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalFailures = a} :: ExportJobResponse)
{-# DEPRECATED ejTotalFailures "Use generic-lens or generic-optics with 'totalFailures' instead." #-}

instance Lude.FromJSON ExportJobResponse where
  parseJSON =
    Lude.withObject
      "ExportJobResponse"
      ( \x ->
          ExportJobResponse'
            Lude.<$> (x Lude..:? "CompletedPieces")
            Lude.<*> (x Lude..:? "FailedPieces")
            Lude.<*> (x Lude..: "Definition")
            Lude.<*> (x Lude..:? "TotalProcessed")
            Lude.<*> (x Lude..:? "Failures" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TotalPieces")
            Lude.<*> (x Lude..: "ApplicationId")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "Type")
            Lude.<*> (x Lude..:? "CompletionDate")
            Lude.<*> (x Lude..: "JobStatus")
            Lude.<*> (x Lude..:? "TotalFailures")
      )
