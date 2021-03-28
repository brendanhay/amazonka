{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.JobParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.JobParameters
  ( JobParameters (..)
  -- * Smart constructor
  , mkJobParameters
  -- * Lenses
  , jpArchiveId
  , jpDescription
  , jpFormat
  , jpInventoryRetrievalParameters
  , jpOutputLocation
  , jpRetrievalByteRange
  , jpSNSTopic
  , jpSelectParameters
  , jpTier
  , jpType
  ) where

import qualified Network.AWS.Glacier.Types.InventoryRetrievalJobInput as Types
import qualified Network.AWS.Glacier.Types.OutputLocation as Types
import qualified Network.AWS.Glacier.Types.SelectParameters as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides options for defining a job.
--
-- /See:/ 'mkJobParameters' smart constructor.
data JobParameters = JobParameters'
  { archiveId :: Core.Maybe Core.Text
    -- ^ The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request. 
  , description :: Core.Maybe Core.Text
    -- ^ The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
  , format :: Core.Maybe Core.Text
    -- ^ When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
  , inventoryRetrievalParameters :: Core.Maybe Types.InventoryRetrievalJobInput
    -- ^ Input parameters used for range inventory retrieval.
  , outputLocation :: Core.Maybe Types.OutputLocation
    -- ^ Contains information about the location where the select job results are stored.
  , retrievalByteRange :: Core.Maybe Core.Text
    -- ^ The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response. 
--
-- An error occurs if you specify this field for an inventory retrieval job request.
  , sNSTopic :: Core.Maybe Core.Text
    -- ^ The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
  , selectParameters :: Core.Maybe Types.SelectParameters
    -- ^ Contains the parameters that define a job.
  , tier :: Core.Maybe Core.Text
    -- ^ The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
  , type' :: Core.Maybe Core.Text
    -- ^ The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobParameters' value with any optional fields omitted.
mkJobParameters
    :: JobParameters
mkJobParameters
  = JobParameters'{archiveId = Core.Nothing,
                   description = Core.Nothing, format = Core.Nothing,
                   inventoryRetrievalParameters = Core.Nothing,
                   outputLocation = Core.Nothing, retrievalByteRange = Core.Nothing,
                   sNSTopic = Core.Nothing, selectParameters = Core.Nothing,
                   tier = Core.Nothing, type' = Core.Nothing}

-- | The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request. 
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpArchiveId :: Lens.Lens' JobParameters (Core.Maybe Core.Text)
jpArchiveId = Lens.field @"archiveId"
{-# INLINEABLE jpArchiveId #-}
{-# DEPRECATED archiveId "Use generic-lens or generic-optics with 'archiveId' instead"  #-}

-- | The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpDescription :: Lens.Lens' JobParameters (Core.Maybe Core.Text)
jpDescription = Lens.field @"description"
{-# INLINEABLE jpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpFormat :: Lens.Lens' JobParameters (Core.Maybe Core.Text)
jpFormat = Lens.field @"format"
{-# INLINEABLE jpFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | Input parameters used for range inventory retrieval.
--
-- /Note:/ Consider using 'inventoryRetrievalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpInventoryRetrievalParameters :: Lens.Lens' JobParameters (Core.Maybe Types.InventoryRetrievalJobInput)
jpInventoryRetrievalParameters = Lens.field @"inventoryRetrievalParameters"
{-# INLINEABLE jpInventoryRetrievalParameters #-}
{-# DEPRECATED inventoryRetrievalParameters "Use generic-lens or generic-optics with 'inventoryRetrievalParameters' instead"  #-}

-- | Contains information about the location where the select job results are stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpOutputLocation :: Lens.Lens' JobParameters (Core.Maybe Types.OutputLocation)
jpOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE jpOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

-- | The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response. 
--
-- An error occurs if you specify this field for an inventory retrieval job request.
--
-- /Note:/ Consider using 'retrievalByteRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpRetrievalByteRange :: Lens.Lens' JobParameters (Core.Maybe Core.Text)
jpRetrievalByteRange = Lens.field @"retrievalByteRange"
{-# INLINEABLE jpRetrievalByteRange #-}
{-# DEPRECATED retrievalByteRange "Use generic-lens or generic-optics with 'retrievalByteRange' instead"  #-}

-- | The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
--
-- /Note:/ Consider using 'sNSTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpSNSTopic :: Lens.Lens' JobParameters (Core.Maybe Core.Text)
jpSNSTopic = Lens.field @"sNSTopic"
{-# INLINEABLE jpSNSTopic #-}
{-# DEPRECATED sNSTopic "Use generic-lens or generic-optics with 'sNSTopic' instead"  #-}

-- | Contains the parameters that define a job.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpSelectParameters :: Lens.Lens' JobParameters (Core.Maybe Types.SelectParameters)
jpSelectParameters = Lens.field @"selectParameters"
{-# INLINEABLE jpSelectParameters #-}
{-# DEPRECATED selectParameters "Use generic-lens or generic-optics with 'selectParameters' instead"  #-}

-- | The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpTier :: Lens.Lens' JobParameters (Core.Maybe Core.Text)
jpTier = Lens.field @"tier"
{-# INLINEABLE jpTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpType :: Lens.Lens' JobParameters (Core.Maybe Core.Text)
jpType = Lens.field @"type'"
{-# INLINEABLE jpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON JobParameters where
        toJSON JobParameters{..}
          = Core.object
              (Core.catMaybes
                 [("ArchiveId" Core..=) Core.<$> archiveId,
                  ("Description" Core..=) Core.<$> description,
                  ("Format" Core..=) Core.<$> format,
                  ("InventoryRetrievalParameters" Core..=) Core.<$>
                    inventoryRetrievalParameters,
                  ("OutputLocation" Core..=) Core.<$> outputLocation,
                  ("RetrievalByteRange" Core..=) Core.<$> retrievalByteRange,
                  ("SNSTopic" Core..=) Core.<$> sNSTopic,
                  ("SelectParameters" Core..=) Core.<$> selectParameters,
                  ("Tier" Core..=) Core.<$> tier, ("Type" Core..=) Core.<$> type'])
