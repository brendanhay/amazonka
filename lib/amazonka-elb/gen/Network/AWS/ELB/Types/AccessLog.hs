{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AccessLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AccessLog
  ( AccessLog (..),

    -- * Smart constructor
    mkAccessLog,

    -- * Lenses
    alEmitInterval,
    alS3BucketPrefix,
    alS3BucketName,
    alEnabled,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the @AccessLog@ attribute.
--
-- /See:/ 'mkAccessLog' smart constructor.
data AccessLog = AccessLog'
  { emitInterval :: Lude.Maybe Lude.Int,
    s3BucketPrefix :: Lude.Maybe Lude.Text,
    s3BucketName :: Lude.Maybe Lude.Text,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessLog' with the minimum fields required to make a request.
--
-- * 'emitInterval' - The interval for publishing the access logs. You can specify an interval of either 5 minutes or 60 minutes.
--
-- Default: 60 minutes
-- * 'enabled' - Specifies whether access logs are enabled for the load balancer.
-- * 's3BucketName' - The name of the Amazon S3 bucket where the access logs are stored.
-- * 's3BucketPrefix' - The logical hierarchy you created for your Amazon S3 bucket, for example @my-bucket-prefix/prod@ . If the prefix is not provided, the log is placed at the root level of the bucket.
mkAccessLog ::
  -- | 'enabled'
  Lude.Bool ->
  AccessLog
mkAccessLog pEnabled_ =
  AccessLog'
    { emitInterval = Lude.Nothing,
      s3BucketPrefix = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      enabled = pEnabled_
    }

-- | The interval for publishing the access logs. You can specify an interval of either 5 minutes or 60 minutes.
--
-- Default: 60 minutes
--
-- /Note:/ Consider using 'emitInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alEmitInterval :: Lens.Lens' AccessLog (Lude.Maybe Lude.Int)
alEmitInterval = Lens.lens (emitInterval :: AccessLog -> Lude.Maybe Lude.Int) (\s a -> s {emitInterval = a} :: AccessLog)
{-# DEPRECATED alEmitInterval "Use generic-lens or generic-optics with 'emitInterval' instead." #-}

-- | The logical hierarchy you created for your Amazon S3 bucket, for example @my-bucket-prefix/prod@ . If the prefix is not provided, the log is placed at the root level of the bucket.
--
-- /Note:/ Consider using 's3BucketPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alS3BucketPrefix :: Lens.Lens' AccessLog (Lude.Maybe Lude.Text)
alS3BucketPrefix = Lens.lens (s3BucketPrefix :: AccessLog -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketPrefix = a} :: AccessLog)
{-# DEPRECATED alS3BucketPrefix "Use generic-lens or generic-optics with 's3BucketPrefix' instead." #-}

-- | The name of the Amazon S3 bucket where the access logs are stored.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alS3BucketName :: Lens.Lens' AccessLog (Lude.Maybe Lude.Text)
alS3BucketName = Lens.lens (s3BucketName :: AccessLog -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: AccessLog)
{-# DEPRECATED alS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies whether access logs are enabled for the load balancer.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alEnabled :: Lens.Lens' AccessLog Lude.Bool
alEnabled = Lens.lens (enabled :: AccessLog -> Lude.Bool) (\s a -> s {enabled = a} :: AccessLog)
{-# DEPRECATED alEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML AccessLog where
  parseXML x =
    AccessLog'
      Lude.<$> (x Lude..@? "EmitInterval")
      Lude.<*> (x Lude..@? "S3BucketPrefix")
      Lude.<*> (x Lude..@? "S3BucketName")
      Lude.<*> (x Lude..@ "Enabled")

instance Lude.ToQuery AccessLog where
  toQuery AccessLog' {..} =
    Lude.mconcat
      [ "EmitInterval" Lude.=: emitInterval,
        "S3BucketPrefix" Lude.=: s3BucketPrefix,
        "S3BucketName" Lude.=: s3BucketName,
        "Enabled" Lude.=: enabled
      ]
