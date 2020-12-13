{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskS3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskS3Location
  ( ExportTaskS3Location (..),

    -- * Smart constructor
    mkExportTaskS3Location,

    -- * Lenses
    etslS3Prefix,
    etslS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the destination for an export image task.
--
-- /See:/ 'mkExportTaskS3Location' smart constructor.
data ExportTaskS3Location = ExportTaskS3Location'
  { -- | The prefix (logical hierarchy) in the bucket.
    s3Prefix :: Lude.Maybe Lude.Text,
    -- | The destination Amazon S3 bucket.
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTaskS3Location' with the minimum fields required to make a request.
--
-- * 's3Prefix' - The prefix (logical hierarchy) in the bucket.
-- * 's3Bucket' - The destination Amazon S3 bucket.
mkExportTaskS3Location ::
  ExportTaskS3Location
mkExportTaskS3Location =
  ExportTaskS3Location'
    { s3Prefix = Lude.Nothing,
      s3Bucket = Lude.Nothing
    }

-- | The prefix (logical hierarchy) in the bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslS3Prefix :: Lens.Lens' ExportTaskS3Location (Lude.Maybe Lude.Text)
etslS3Prefix = Lens.lens (s3Prefix :: ExportTaskS3Location -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: ExportTaskS3Location)
{-# DEPRECATED etslS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslS3Bucket :: Lens.Lens' ExportTaskS3Location (Lude.Maybe Lude.Text)
etslS3Bucket = Lens.lens (s3Bucket :: ExportTaskS3Location -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: ExportTaskS3Location)
{-# DEPRECATED etslS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.FromXML ExportTaskS3Location where
  parseXML x =
    ExportTaskS3Location'
      Lude.<$> (x Lude..@? "s3Prefix") Lude.<*> (x Lude..@? "s3Bucket")
