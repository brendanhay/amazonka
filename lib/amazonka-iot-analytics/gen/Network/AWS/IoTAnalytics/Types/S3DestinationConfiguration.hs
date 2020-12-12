{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
  ( S3DestinationConfiguration (..),

    -- * Smart constructor
    mkS3DestinationConfiguration,

    -- * Lenses
    sdcGlueConfiguration,
    sdcBucket,
    sdcKey,
    sdcRoleARN,
  )
where

import Network.AWS.IoTAnalytics.Types.GlueConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for delivery of dataset contents to Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'mkS3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
  { glueConfiguration ::
      Lude.Maybe GlueConfiguration,
    bucket :: Lude.Text,
    key :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3DestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the S3 bucket to which dataset contents are delivered.
-- * 'glueConfiguration' - Configuration information for coordination with AWS Glue, a fully managed extract, transform and load (ETL) service.
-- * 'key' - The key of the dataset contents object in an S3 bucket. Each object has a key that is a unique identifier. Each object has exactly one key.
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
-- * 'roleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 and AWS Glue resources.
mkS3DestinationConfiguration ::
  -- | 'bucket'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  S3DestinationConfiguration
mkS3DestinationConfiguration pBucket_ pKey_ pRoleARN_ =
  S3DestinationConfiguration'
    { glueConfiguration = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      roleARN = pRoleARN_
    }

-- | Configuration information for coordination with AWS Glue, a fully managed extract, transform and load (ETL) service.
--
-- /Note:/ Consider using 'glueConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcGlueConfiguration :: Lens.Lens' S3DestinationConfiguration (Lude.Maybe GlueConfiguration)
sdcGlueConfiguration = Lens.lens (glueConfiguration :: S3DestinationConfiguration -> Lude.Maybe GlueConfiguration) (\s a -> s {glueConfiguration = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcGlueConfiguration "Use generic-lens or generic-optics with 'glueConfiguration' instead." #-}

-- | The name of the S3 bucket to which dataset contents are delivered.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcBucket :: Lens.Lens' S3DestinationConfiguration Lude.Text
sdcBucket = Lens.lens (bucket :: S3DestinationConfiguration -> Lude.Text) (\s a -> s {bucket = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

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
sdcKey :: Lens.Lens' S3DestinationConfiguration Lude.Text
sdcKey = Lens.lens (key :: S3DestinationConfiguration -> Lude.Text) (\s a -> s {key = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 and AWS Glue resources.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcRoleARN :: Lens.Lens' S3DestinationConfiguration Lude.Text
sdcRoleARN = Lens.lens (roleARN :: S3DestinationConfiguration -> Lude.Text) (\s a -> s {roleARN = a} :: S3DestinationConfiguration)
{-# DEPRECATED sdcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON S3DestinationConfiguration where
  parseJSON =
    Lude.withObject
      "S3DestinationConfiguration"
      ( \x ->
          S3DestinationConfiguration'
            Lude.<$> (x Lude..:? "glueConfiguration")
            Lude.<*> (x Lude..: "bucket")
            Lude.<*> (x Lude..: "key")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON S3DestinationConfiguration where
  toJSON S3DestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("glueConfiguration" Lude..=) Lude.<$> glueConfiguration,
            Lude.Just ("bucket" Lude..= bucket),
            Lude.Just ("key" Lude..= key),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
