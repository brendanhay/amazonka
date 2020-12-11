-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.LoggingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.LoggingInfo
  ( LoggingInfo (..),

    -- * Smart constructor
    mkLoggingInfo,

    -- * Lenses
    liS3KeyPrefix,
    liS3BucketName,
    liS3Region,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an S3 bucket to write instance-level logs to.
--
-- /See:/ 'mkLoggingInfo' smart constructor.
data LoggingInfo = LoggingInfo'
  { s3KeyPrefix ::
      Lude.Maybe Lude.Text,
    s3BucketName :: Lude.Text,
    s3Region :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingInfo' with the minimum fields required to make a request.
--
-- * 's3BucketName' - The name of an S3 bucket where execution logs are stored .
-- * 's3KeyPrefix' - (Optional) The S3 bucket subfolder.
-- * 's3Region' - The Region where the S3 bucket is located.
mkLoggingInfo ::
  -- | 's3BucketName'
  Lude.Text ->
  -- | 's3Region'
  Lude.Text ->
  LoggingInfo
mkLoggingInfo pS3BucketName_ pS3Region_ =
  LoggingInfo'
    { s3KeyPrefix = Lude.Nothing,
      s3BucketName = pS3BucketName_,
      s3Region = pS3Region_
    }

-- | (Optional) The S3 bucket subfolder.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liS3KeyPrefix :: Lens.Lens' LoggingInfo (Lude.Maybe Lude.Text)
liS3KeyPrefix = Lens.lens (s3KeyPrefix :: LoggingInfo -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: LoggingInfo)
{-# DEPRECATED liS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The name of an S3 bucket where execution logs are stored .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liS3BucketName :: Lens.Lens' LoggingInfo Lude.Text
liS3BucketName = Lens.lens (s3BucketName :: LoggingInfo -> Lude.Text) (\s a -> s {s3BucketName = a} :: LoggingInfo)
{-# DEPRECATED liS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The Region where the S3 bucket is located.
--
-- /Note:/ Consider using 's3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liS3Region :: Lens.Lens' LoggingInfo Lude.Text
liS3Region = Lens.lens (s3Region :: LoggingInfo -> Lude.Text) (\s a -> s {s3Region = a} :: LoggingInfo)
{-# DEPRECATED liS3Region "Use generic-lens or generic-optics with 's3Region' instead." #-}

instance Lude.FromJSON LoggingInfo where
  parseJSON =
    Lude.withObject
      "LoggingInfo"
      ( \x ->
          LoggingInfo'
            Lude.<$> (x Lude..:? "S3KeyPrefix")
            Lude.<*> (x Lude..: "S3BucketName")
            Lude.<*> (x Lude..: "S3Region")
      )

instance Lude.ToJSON LoggingInfo where
  toJSON LoggingInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            Lude.Just ("S3BucketName" Lude..= s3BucketName),
            Lude.Just ("S3Region" Lude..= s3Region)
          ]
      )
