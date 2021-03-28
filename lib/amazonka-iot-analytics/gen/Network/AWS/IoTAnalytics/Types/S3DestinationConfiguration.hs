{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
  ( S3DestinationConfiguration (..)
  -- * Smart constructor
  , mkS3DestinationConfiguration
  -- * Lenses
  , sdcBucket
  , sdcKey
  , sdcRoleArn
  , sdcGlueConfiguration
  ) where

import qualified Network.AWS.IoTAnalytics.Types.Bucket as Types
import qualified Network.AWS.IoTAnalytics.Types.GlueConfiguration as Types
import qualified Network.AWS.IoTAnalytics.Types.Key as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information for delivery of dataset contents to Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'mkS3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
  { bucket :: Types.Bucket
    -- ^ The name of the S3 bucket to which dataset contents are delivered.
  , key :: Types.Key
    -- ^ The key of the dataset contents object in an S3 bucket. Each object has a key that is a unique identifier. Each object has exactly one key.
--
-- You can create a unique key with the following options:
--
--     * Use @!{iotanalytics:scheduleTime}@ to insert the time of a scheduled SQL query run.
--
--
--     * Use @!{iotanalytics:versionId}@ to insert a unique hash that identifies a dataset content.
--
--
--     * Use @!{iotanalytics:creationTime}@ to insert the creation time of a dataset content.
--
--
-- The following example creates a unique key for a CSV file: @dataset/mydataset/!{iotanalytics:scheduleTime}/!{iotanalytics:versionId}.csv@ 
  , roleArn :: Types.RoleArn
    -- ^ The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 and AWS Glue resources.
  , glueConfiguration :: Core.Maybe Types.GlueConfiguration
    -- ^ Configuration information for coordination with AWS Glue, a fully managed extract, transform and load (ETL) service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3DestinationConfiguration' value with any optional fields omitted.
mkS3DestinationConfiguration
    :: Types.Bucket -- ^ 'bucket'
    -> Types.Key -- ^ 'key'
    -> Types.RoleArn -- ^ 'roleArn'
    -> S3DestinationConfiguration
mkS3DestinationConfiguration bucket key roleArn
  = S3DestinationConfiguration'{bucket, key, roleArn,
                                glueConfiguration = Core.Nothing}

-- | The name of the S3 bucket to which dataset contents are delivered.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcBucket :: Lens.Lens' S3DestinationConfiguration Types.Bucket
sdcBucket = Lens.field @"bucket"
{-# INLINEABLE sdcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The key of the dataset contents object in an S3 bucket. Each object has a key that is a unique identifier. Each object has exactly one key.
--
-- You can create a unique key with the following options:
--
--     * Use @!{iotanalytics:scheduleTime}@ to insert the time of a scheduled SQL query run.
--
--
--     * Use @!{iotanalytics:versionId}@ to insert a unique hash that identifies a dataset content.
--
--
--     * Use @!{iotanalytics:creationTime}@ to insert the creation time of a dataset content.
--
--
-- The following example creates a unique key for a CSV file: @dataset/mydataset/!{iotanalytics:scheduleTime}/!{iotanalytics:versionId}.csv@ 
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcKey :: Lens.Lens' S3DestinationConfiguration Types.Key
sdcKey = Lens.field @"key"
{-# INLINEABLE sdcKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 and AWS Glue resources.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcRoleArn :: Lens.Lens' S3DestinationConfiguration Types.RoleArn
sdcRoleArn = Lens.field @"roleArn"
{-# INLINEABLE sdcRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Configuration information for coordination with AWS Glue, a fully managed extract, transform and load (ETL) service.
--
-- /Note:/ Consider using 'glueConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcGlueConfiguration :: Lens.Lens' S3DestinationConfiguration (Core.Maybe Types.GlueConfiguration)
sdcGlueConfiguration = Lens.field @"glueConfiguration"
{-# INLINEABLE sdcGlueConfiguration #-}
{-# DEPRECATED glueConfiguration "Use generic-lens or generic-optics with 'glueConfiguration' instead"  #-}

instance Core.FromJSON S3DestinationConfiguration where
        toJSON S3DestinationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("bucket" Core..= bucket),
                  Core.Just ("key" Core..= key),
                  Core.Just ("roleArn" Core..= roleArn),
                  ("glueConfiguration" Core..=) Core.<$> glueConfiguration])

instance Core.FromJSON S3DestinationConfiguration where
        parseJSON
          = Core.withObject "S3DestinationConfiguration" Core.$
              \ x ->
                S3DestinationConfiguration' Core.<$>
                  (x Core..: "bucket") Core.<*> x Core..: "key" Core.<*>
                    x Core..: "roleArn"
                    Core.<*> x Core..:? "glueConfiguration"
