{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskS3LocationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskS3LocationRequest
  ( ExportTaskS3LocationRequest (..),

    -- * Smart constructor
    mkExportTaskS3LocationRequest,

    -- * Lenses
    etslrS3Prefix,
    etslrS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the destination for an export image task.
--
-- /See:/ 'mkExportTaskS3LocationRequest' smart constructor.
data ExportTaskS3LocationRequest = ExportTaskS3LocationRequest'
  { -- | The prefix (logical hierarchy) in the bucket.
    s3Prefix :: Lude.Maybe Lude.Text,
    -- | The destination Amazon S3 bucket.
    s3Bucket :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTaskS3LocationRequest' with the minimum fields required to make a request.
--
-- * 's3Prefix' - The prefix (logical hierarchy) in the bucket.
-- * 's3Bucket' - The destination Amazon S3 bucket.
mkExportTaskS3LocationRequest ::
  -- | 's3Bucket'
  Lude.Text ->
  ExportTaskS3LocationRequest
mkExportTaskS3LocationRequest pS3Bucket_ =
  ExportTaskS3LocationRequest'
    { s3Prefix = Lude.Nothing,
      s3Bucket = pS3Bucket_
    }

-- | The prefix (logical hierarchy) in the bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslrS3Prefix :: Lens.Lens' ExportTaskS3LocationRequest (Lude.Maybe Lude.Text)
etslrS3Prefix = Lens.lens (s3Prefix :: ExportTaskS3LocationRequest -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: ExportTaskS3LocationRequest)
{-# DEPRECATED etslrS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslrS3Bucket :: Lens.Lens' ExportTaskS3LocationRequest Lude.Text
etslrS3Bucket = Lens.lens (s3Bucket :: ExportTaskS3LocationRequest -> Lude.Text) (\s a -> s {s3Bucket = a} :: ExportTaskS3LocationRequest)
{-# DEPRECATED etslrS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.ToQuery ExportTaskS3LocationRequest where
  toQuery ExportTaskS3LocationRequest' {..} =
    Lude.mconcat
      ["S3Prefix" Lude.=: s3Prefix, "S3Bucket" Lude.=: s3Bucket]
