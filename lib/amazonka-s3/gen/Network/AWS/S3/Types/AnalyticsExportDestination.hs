-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsExportDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsExportDestination
  ( AnalyticsExportDestination (..),

    -- * Smart constructor
    mkAnalyticsExportDestination,

    -- * Lenses
    aedS3BucketDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsS3BucketDestination

-- | Where to publish the analytics results.
--
-- /See:/ 'mkAnalyticsExportDestination' smart constructor.
newtype AnalyticsExportDestination = AnalyticsExportDestination'
  { s3BucketDestination ::
      AnalyticsS3BucketDestination
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalyticsExportDestination' with the minimum fields required to make a request.
--
-- * 's3BucketDestination' - A destination signifying output to an S3 bucket.
mkAnalyticsExportDestination ::
  -- | 's3BucketDestination'
  AnalyticsS3BucketDestination ->
  AnalyticsExportDestination
mkAnalyticsExportDestination pS3BucketDestination_ =
  AnalyticsExportDestination'
    { s3BucketDestination =
        pS3BucketDestination_
    }

-- | A destination signifying output to an S3 bucket.
--
-- /Note:/ Consider using 's3BucketDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedS3BucketDestination :: Lens.Lens' AnalyticsExportDestination AnalyticsS3BucketDestination
aedS3BucketDestination = Lens.lens (s3BucketDestination :: AnalyticsExportDestination -> AnalyticsS3BucketDestination) (\s a -> s {s3BucketDestination = a} :: AnalyticsExportDestination)
{-# DEPRECATED aedS3BucketDestination "Use generic-lens or generic-optics with 's3BucketDestination' instead." #-}

instance Lude.FromXML AnalyticsExportDestination where
  parseXML x =
    AnalyticsExportDestination'
      Lude.<$> (x Lude..@ "S3BucketDestination")

instance Lude.ToXML AnalyticsExportDestination where
  toXML AnalyticsExportDestination' {..} =
    Lude.mconcat ["S3BucketDestination" Lude.@= s3BucketDestination]
