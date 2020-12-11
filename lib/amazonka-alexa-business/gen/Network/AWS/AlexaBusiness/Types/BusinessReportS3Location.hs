-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
  ( BusinessReportS3Location (..),

    -- * Smart constructor
    mkBusinessReportS3Location,

    -- * Lenses
    brslPath,
    brslBucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The S3 location of the output reports.
--
-- /See:/ 'mkBusinessReportS3Location' smart constructor.
data BusinessReportS3Location = BusinessReportS3Location'
  { path ::
      Lude.Maybe Lude.Text,
    bucketName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BusinessReportS3Location' with the minimum fields required to make a request.
--
-- * 'bucketName' - The S3 bucket name of the output reports.
-- * 'path' - The path of the business report.
mkBusinessReportS3Location ::
  BusinessReportS3Location
mkBusinessReportS3Location =
  BusinessReportS3Location'
    { path = Lude.Nothing,
      bucketName = Lude.Nothing
    }

-- | The path of the business report.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brslPath :: Lens.Lens' BusinessReportS3Location (Lude.Maybe Lude.Text)
brslPath = Lens.lens (path :: BusinessReportS3Location -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: BusinessReportS3Location)
{-# DEPRECATED brslPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The S3 bucket name of the output reports.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brslBucketName :: Lens.Lens' BusinessReportS3Location (Lude.Maybe Lude.Text)
brslBucketName = Lens.lens (bucketName :: BusinessReportS3Location -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: BusinessReportS3Location)
{-# DEPRECATED brslBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

instance Lude.FromJSON BusinessReportS3Location where
  parseJSON =
    Lude.withObject
      "BusinessReportS3Location"
      ( \x ->
          BusinessReportS3Location'
            Lude.<$> (x Lude..:? "Path") Lude.<*> (x Lude..:? "BucketName")
      )
