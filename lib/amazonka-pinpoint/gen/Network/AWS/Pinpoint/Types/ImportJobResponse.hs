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
    ijCompletedPieces,
    ijFailedPieces,
    ijTotalProcessed,
    ijFailures,
    ijTotalPieces,
    ijCompletionDate,
    ijTotalFailures,
    ijJobStatus,
    ijCreationDate,
    ijType,
    ijDefinition,
    ijId,
    ijApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ImportJobResource
import Network.AWS.Pinpoint.Types.JobStatus
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of a job that imports endpoint definitions from one or more files. The files can be stored in an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly from a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkImportJobResponse' smart constructor.
data ImportJobResponse = ImportJobResponse'
  { completedPieces ::
      Lude.Maybe Lude.Int,
    failedPieces :: Lude.Maybe Lude.Int,
    totalProcessed :: Lude.Maybe Lude.Int,
    failures :: Lude.Maybe [Lude.Text],
    totalPieces :: Lude.Maybe Lude.Int,
    completionDate :: Lude.Maybe Lude.Text,
    totalFailures :: Lude.Maybe Lude.Int,
    jobStatus :: JobStatus,
    creationDate :: Lude.Text,
    type' :: Lude.Text,
    definition :: ImportJobResource,
    id :: Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportJobResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that's associated with the import job.
-- * 'completedPieces' - The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
-- * 'completionDate' - The date, in ISO 8601 format, when the import job was completed.
-- * 'creationDate' - The date, in ISO 8601 format, when the import job was created.
-- * 'definition' - The resource settings that apply to the import job.
-- * 'failedPieces' - The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
-- * 'failures' - An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
-- * 'id' - The unique identifier for the import job.
-- * 'jobStatus' - The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
-- * 'totalFailures' - The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
-- * 'totalPieces' - The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
-- * 'totalProcessed' - The total number of endpoint definitions that were processed by the import job.
-- * 'type'' - The job type. This value is IMPORT for import jobs.
mkImportJobResponse ::
  -- | 'jobStatus'
  JobStatus ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  -- | 'definition'
  ImportJobResource ->
  -- | 'id'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  ImportJobResponse
mkImportJobResponse
  pJobStatus_
  pCreationDate_
  pType_
  pDefinition_
  pId_
  pApplicationId_ =
    ImportJobResponse'
      { completedPieces = Lude.Nothing,
        failedPieces = Lude.Nothing,
        totalProcessed = Lude.Nothing,
        failures = Lude.Nothing,
        totalPieces = Lude.Nothing,
        completionDate = Lude.Nothing,
        totalFailures = Lude.Nothing,
        jobStatus = pJobStatus_,
        creationDate = pCreationDate_,
        type' = pType_,
        definition = pDefinition_,
        id = pId_,
        applicationId = pApplicationId_
      }

-- | The number of pieces that were processed successfully (completed) by the import job, as of the time of the request.
--
-- /Note:/ Consider using 'completedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijCompletedPieces :: Lens.Lens' ImportJobResponse (Lude.Maybe Lude.Int)
ijCompletedPieces = Lens.lens (completedPieces :: ImportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {completedPieces = a} :: ImportJobResponse)
{-# DEPRECATED ijCompletedPieces "Use generic-lens or generic-optics with 'completedPieces' instead." #-}

-- | The number of pieces that weren't processed successfully (failed) by the import job, as of the time of the request.
--
-- /Note:/ Consider using 'failedPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijFailedPieces :: Lens.Lens' ImportJobResponse (Lude.Maybe Lude.Int)
ijFailedPieces = Lens.lens (failedPieces :: ImportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {failedPieces = a} :: ImportJobResponse)
{-# DEPRECATED ijFailedPieces "Use generic-lens or generic-optics with 'failedPieces' instead." #-}

-- | The total number of endpoint definitions that were processed by the import job.
--
-- /Note:/ Consider using 'totalProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijTotalProcessed :: Lens.Lens' ImportJobResponse (Lude.Maybe Lude.Int)
ijTotalProcessed = Lens.lens (totalProcessed :: ImportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalProcessed = a} :: ImportJobResponse)
{-# DEPRECATED ijTotalProcessed "Use generic-lens or generic-optics with 'totalProcessed' instead." #-}

-- | An array of entries, one for each of the first 100 entries that weren't processed successfully (failed) by the import job, if any.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijFailures :: Lens.Lens' ImportJobResponse (Lude.Maybe [Lude.Text])
ijFailures = Lens.lens (failures :: ImportJobResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {failures = a} :: ImportJobResponse)
{-# DEPRECATED ijFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The total number of pieces that must be processed to complete the import job. Each piece consists of an approximately equal portion of the endpoint definitions that are part of the import job.
--
-- /Note:/ Consider using 'totalPieces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijTotalPieces :: Lens.Lens' ImportJobResponse (Lude.Maybe Lude.Int)
ijTotalPieces = Lens.lens (totalPieces :: ImportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalPieces = a} :: ImportJobResponse)
{-# DEPRECATED ijTotalPieces "Use generic-lens or generic-optics with 'totalPieces' instead." #-}

-- | The date, in ISO 8601 format, when the import job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijCompletionDate :: Lens.Lens' ImportJobResponse (Lude.Maybe Lude.Text)
ijCompletionDate = Lens.lens (completionDate :: ImportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {completionDate = a} :: ImportJobResponse)
{-# DEPRECATED ijCompletionDate "Use generic-lens or generic-optics with 'completionDate' instead." #-}

-- | The total number of endpoint definitions that weren't processed successfully (failed) by the import job, typically because an error, such as a syntax error, occurred.
--
-- /Note:/ Consider using 'totalFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijTotalFailures :: Lens.Lens' ImportJobResponse (Lude.Maybe Lude.Int)
ijTotalFailures = Lens.lens (totalFailures :: ImportJobResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalFailures = a} :: ImportJobResponse)
{-# DEPRECATED ijTotalFailures "Use generic-lens or generic-optics with 'totalFailures' instead." #-}

-- | The status of the import job. The job status is FAILED if Amazon Pinpoint wasn't able to process one or more pieces in the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijJobStatus :: Lens.Lens' ImportJobResponse JobStatus
ijJobStatus = Lens.lens (jobStatus :: ImportJobResponse -> JobStatus) (\s a -> s {jobStatus = a} :: ImportJobResponse)
{-# DEPRECATED ijJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The date, in ISO 8601 format, when the import job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijCreationDate :: Lens.Lens' ImportJobResponse Lude.Text
ijCreationDate = Lens.lens (creationDate :: ImportJobResponse -> Lude.Text) (\s a -> s {creationDate = a} :: ImportJobResponse)
{-# DEPRECATED ijCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The job type. This value is IMPORT for import jobs.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijType :: Lens.Lens' ImportJobResponse Lude.Text
ijType = Lens.lens (type' :: ImportJobResponse -> Lude.Text) (\s a -> s {type' = a} :: ImportJobResponse)
{-# DEPRECATED ijType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The resource settings that apply to the import job.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijDefinition :: Lens.Lens' ImportJobResponse ImportJobResource
ijDefinition = Lens.lens (definition :: ImportJobResponse -> ImportJobResource) (\s a -> s {definition = a} :: ImportJobResponse)
{-# DEPRECATED ijDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The unique identifier for the import job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijId :: Lens.Lens' ImportJobResponse Lude.Text
ijId = Lens.lens (id :: ImportJobResponse -> Lude.Text) (\s a -> s {id = a} :: ImportJobResponse)
{-# DEPRECATED ijId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique identifier for the application that's associated with the import job.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijApplicationId :: Lens.Lens' ImportJobResponse Lude.Text
ijApplicationId = Lens.lens (applicationId :: ImportJobResponse -> Lude.Text) (\s a -> s {applicationId = a} :: ImportJobResponse)
{-# DEPRECATED ijApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON ImportJobResponse where
  parseJSON =
    Lude.withObject
      "ImportJobResponse"
      ( \x ->
          ImportJobResponse'
            Lude.<$> (x Lude..:? "CompletedPieces")
            Lude.<*> (x Lude..:? "FailedPieces")
            Lude.<*> (x Lude..:? "TotalProcessed")
            Lude.<*> (x Lude..:? "Failures" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TotalPieces")
            Lude.<*> (x Lude..:? "CompletionDate")
            Lude.<*> (x Lude..:? "TotalFailures")
            Lude.<*> (x Lude..: "JobStatus")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "Type")
            Lude.<*> (x Lude..: "Definition")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "ApplicationId")
      )
