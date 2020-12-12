{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.JobParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.JobParameters
  ( JobParameters (..),

    -- * Smart constructor
    mkJobParameters,

    -- * Lenses
    jpArchiveId,
    jpSelectParameters,
    jpFormat,
    jpRetrievalByteRange,
    jpInventoryRetrievalParameters,
    jpSNSTopic,
    jpOutputLocation,
    jpTier,
    jpType,
    jpDescription,
  )
where

import Network.AWS.Glacier.Types.InventoryRetrievalJobInput
import Network.AWS.Glacier.Types.OutputLocation
import Network.AWS.Glacier.Types.SelectParameters
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides options for defining a job.
--
-- /See:/ 'mkJobParameters' smart constructor.
data JobParameters = JobParameters'
  { archiveId ::
      Lude.Maybe Lude.Text,
    selectParameters :: Lude.Maybe SelectParameters,
    format :: Lude.Maybe Lude.Text,
    retrievalByteRange :: Lude.Maybe Lude.Text,
    inventoryRetrievalParameters ::
      Lude.Maybe InventoryRetrievalJobInput,
    snsTopic :: Lude.Maybe Lude.Text,
    outputLocation :: Lude.Maybe OutputLocation,
    tier :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobParameters' with the minimum fields required to make a request.
--
-- * 'archiveId' - The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
-- * 'description' - The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
-- * 'format' - When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
-- * 'inventoryRetrievalParameters' - Input parameters used for range inventory retrieval.
-- * 'outputLocation' - Contains information about the location where the select job results are stored.
-- * 'retrievalByteRange' - The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.
--
-- An error occurs if you specify this field for an inventory retrieval job request.
-- * 'selectParameters' - Contains the parameters that define a job.
-- * 'snsTopic' - The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
-- * 'tier' - The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
-- * 'type'' - The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
mkJobParameters ::
  JobParameters
mkJobParameters =
  JobParameters'
    { archiveId = Lude.Nothing,
      selectParameters = Lude.Nothing,
      format = Lude.Nothing,
      retrievalByteRange = Lude.Nothing,
      inventoryRetrievalParameters = Lude.Nothing,
      snsTopic = Lude.Nothing,
      outputLocation = Lude.Nothing,
      tier = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpArchiveId :: Lens.Lens' JobParameters (Lude.Maybe Lude.Text)
jpArchiveId = Lens.lens (archiveId :: JobParameters -> Lude.Maybe Lude.Text) (\s a -> s {archiveId = a} :: JobParameters)
{-# DEPRECATED jpArchiveId "Use generic-lens or generic-optics with 'archiveId' instead." #-}

-- | Contains the parameters that define a job.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpSelectParameters :: Lens.Lens' JobParameters (Lude.Maybe SelectParameters)
jpSelectParameters = Lens.lens (selectParameters :: JobParameters -> Lude.Maybe SelectParameters) (\s a -> s {selectParameters = a} :: JobParameters)
{-# DEPRECATED jpSelectParameters "Use generic-lens or generic-optics with 'selectParameters' instead." #-}

-- | When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpFormat :: Lens.Lens' JobParameters (Lude.Maybe Lude.Text)
jpFormat = Lens.lens (format :: JobParameters -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: JobParameters)
{-# DEPRECATED jpFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.
--
-- An error occurs if you specify this field for an inventory retrieval job request.
--
-- /Note:/ Consider using 'retrievalByteRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpRetrievalByteRange :: Lens.Lens' JobParameters (Lude.Maybe Lude.Text)
jpRetrievalByteRange = Lens.lens (retrievalByteRange :: JobParameters -> Lude.Maybe Lude.Text) (\s a -> s {retrievalByteRange = a} :: JobParameters)
{-# DEPRECATED jpRetrievalByteRange "Use generic-lens or generic-optics with 'retrievalByteRange' instead." #-}

-- | Input parameters used for range inventory retrieval.
--
-- /Note:/ Consider using 'inventoryRetrievalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpInventoryRetrievalParameters :: Lens.Lens' JobParameters (Lude.Maybe InventoryRetrievalJobInput)
jpInventoryRetrievalParameters = Lens.lens (inventoryRetrievalParameters :: JobParameters -> Lude.Maybe InventoryRetrievalJobInput) (\s a -> s {inventoryRetrievalParameters = a} :: JobParameters)
{-# DEPRECATED jpInventoryRetrievalParameters "Use generic-lens or generic-optics with 'inventoryRetrievalParameters' instead." #-}

-- | The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
--
-- /Note:/ Consider using 'snsTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpSNSTopic :: Lens.Lens' JobParameters (Lude.Maybe Lude.Text)
jpSNSTopic = Lens.lens (snsTopic :: JobParameters -> Lude.Maybe Lude.Text) (\s a -> s {snsTopic = a} :: JobParameters)
{-# DEPRECATED jpSNSTopic "Use generic-lens or generic-optics with 'snsTopic' instead." #-}

-- | Contains information about the location where the select job results are stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpOutputLocation :: Lens.Lens' JobParameters (Lude.Maybe OutputLocation)
jpOutputLocation = Lens.lens (outputLocation :: JobParameters -> Lude.Maybe OutputLocation) (\s a -> s {outputLocation = a} :: JobParameters)
{-# DEPRECATED jpOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpTier :: Lens.Lens' JobParameters (Lude.Maybe Lude.Text)
jpTier = Lens.lens (tier :: JobParameters -> Lude.Maybe Lude.Text) (\s a -> s {tier = a} :: JobParameters)
{-# DEPRECATED jpTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpType :: Lens.Lens' JobParameters (Lude.Maybe Lude.Text)
jpType = Lens.lens (type' :: JobParameters -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: JobParameters)
{-# DEPRECATED jpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpDescription :: Lens.Lens' JobParameters (Lude.Maybe Lude.Text)
jpDescription = Lens.lens (description :: JobParameters -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: JobParameters)
{-# DEPRECATED jpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToJSON JobParameters where
  toJSON JobParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ArchiveId" Lude..=) Lude.<$> archiveId,
            ("SelectParameters" Lude..=) Lude.<$> selectParameters,
            ("Format" Lude..=) Lude.<$> format,
            ("RetrievalByteRange" Lude..=) Lude.<$> retrievalByteRange,
            ("InventoryRetrievalParameters" Lude..=)
              Lude.<$> inventoryRetrievalParameters,
            ("SNSTopic" Lude..=) Lude.<$> snsTopic,
            ("OutputLocation" Lude..=) Lude.<$> outputLocation,
            ("Tier" Lude..=) Lude.<$> tier,
            ("Type" Lude..=) Lude.<$> type',
            ("Description" Lude..=) Lude.<$> description
          ]
      )
