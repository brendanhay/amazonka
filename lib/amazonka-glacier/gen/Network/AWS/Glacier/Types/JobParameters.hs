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
    jpDescription,
    jpFormat,
    jpInventoryRetrievalParameters,
    jpOutputLocation,
    jpRetrievalByteRange,
    jpSNSTopic,
    jpSelectParameters,
    jpTier,
    jpType,
  )
where

import qualified Network.AWS.Glacier.Types.ArchiveId as Types
import qualified Network.AWS.Glacier.Types.Description as Types
import qualified Network.AWS.Glacier.Types.Format as Types
import qualified Network.AWS.Glacier.Types.InventoryRetrievalJobInput as Types
import qualified Network.AWS.Glacier.Types.OutputLocation as Types
import qualified Network.AWS.Glacier.Types.RetrievalByteRange as Types
import qualified Network.AWS.Glacier.Types.SNSTopic as Types
import qualified Network.AWS.Glacier.Types.SelectParameters as Types
import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Glacier.Types.Tier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides options for defining a job.
--
-- /See:/ 'mkJobParameters' smart constructor.
data JobParameters = JobParameters'
  { -- | The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
    archiveId :: Core.Maybe Types.ArchiveId,
    -- | The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
    description :: Core.Maybe Types.Description,
    -- | When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
    format :: Core.Maybe Types.Format,
    -- | Input parameters used for range inventory retrieval.
    inventoryRetrievalParameters :: Core.Maybe Types.InventoryRetrievalJobInput,
    -- | Contains information about the location where the select job results are stored.
    outputLocation :: Core.Maybe Types.OutputLocation,
    -- | The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.
    --
    -- An error occurs if you specify this field for an inventory retrieval job request.
    retrievalByteRange :: Core.Maybe Types.RetrievalByteRange,
    -- | The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
    sNSTopic :: Core.Maybe Types.SNSTopic,
    -- | Contains the parameters that define a job.
    selectParameters :: Core.Maybe Types.SelectParameters,
    -- | The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
    tier :: Core.Maybe Types.Tier,
    -- | The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobParameters' value with any optional fields omitted.
mkJobParameters ::
  JobParameters
mkJobParameters =
  JobParameters'
    { archiveId = Core.Nothing,
      description = Core.Nothing,
      format = Core.Nothing,
      inventoryRetrievalParameters = Core.Nothing,
      outputLocation = Core.Nothing,
      retrievalByteRange = Core.Nothing,
      sNSTopic = Core.Nothing,
      selectParameters = Core.Nothing,
      tier = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpArchiveId :: Lens.Lens' JobParameters (Core.Maybe Types.ArchiveId)
jpArchiveId = Lens.field @"archiveId"
{-# DEPRECATED jpArchiveId "Use generic-lens or generic-optics with 'archiveId' instead." #-}

-- | The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpDescription :: Lens.Lens' JobParameters (Core.Maybe Types.Description)
jpDescription = Lens.field @"description"
{-# DEPRECATED jpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpFormat :: Lens.Lens' JobParameters (Core.Maybe Types.Format)
jpFormat = Lens.field @"format"
{-# DEPRECATED jpFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Input parameters used for range inventory retrieval.
--
-- /Note:/ Consider using 'inventoryRetrievalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpInventoryRetrievalParameters :: Lens.Lens' JobParameters (Core.Maybe Types.InventoryRetrievalJobInput)
jpInventoryRetrievalParameters = Lens.field @"inventoryRetrievalParameters"
{-# DEPRECATED jpInventoryRetrievalParameters "Use generic-lens or generic-optics with 'inventoryRetrievalParameters' instead." #-}

-- | Contains information about the location where the select job results are stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpOutputLocation :: Lens.Lens' JobParameters (Core.Maybe Types.OutputLocation)
jpOutputLocation = Lens.field @"outputLocation"
{-# DEPRECATED jpOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.
--
-- An error occurs if you specify this field for an inventory retrieval job request.
--
-- /Note:/ Consider using 'retrievalByteRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpRetrievalByteRange :: Lens.Lens' JobParameters (Core.Maybe Types.RetrievalByteRange)
jpRetrievalByteRange = Lens.field @"retrievalByteRange"
{-# DEPRECATED jpRetrievalByteRange "Use generic-lens or generic-optics with 'retrievalByteRange' instead." #-}

-- | The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
--
-- /Note:/ Consider using 'sNSTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpSNSTopic :: Lens.Lens' JobParameters (Core.Maybe Types.SNSTopic)
jpSNSTopic = Lens.field @"sNSTopic"
{-# DEPRECATED jpSNSTopic "Use generic-lens or generic-optics with 'sNSTopic' instead." #-}

-- | Contains the parameters that define a job.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpSelectParameters :: Lens.Lens' JobParameters (Core.Maybe Types.SelectParameters)
jpSelectParameters = Lens.field @"selectParameters"
{-# DEPRECATED jpSelectParameters "Use generic-lens or generic-optics with 'selectParameters' instead." #-}

-- | The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpTier :: Lens.Lens' JobParameters (Core.Maybe Types.Tier)
jpTier = Lens.field @"tier"
{-# DEPRECATED jpTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpType :: Lens.Lens' JobParameters (Core.Maybe Types.String)
jpType = Lens.field @"type'"
{-# DEPRECATED jpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON JobParameters where
  toJSON JobParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("ArchiveId" Core..=) Core.<$> archiveId,
            ("Description" Core..=) Core.<$> description,
            ("Format" Core..=) Core.<$> format,
            ("InventoryRetrievalParameters" Core..=)
              Core.<$> inventoryRetrievalParameters,
            ("OutputLocation" Core..=) Core.<$> outputLocation,
            ("RetrievalByteRange" Core..=) Core.<$> retrievalByteRange,
            ("SNSTopic" Core..=) Core.<$> sNSTopic,
            ("SelectParameters" Core..=) Core.<$> selectParameters,
            ("Tier" Core..=) Core.<$> tier,
            ("Type" Core..=) Core.<$> type'
          ]
      )
