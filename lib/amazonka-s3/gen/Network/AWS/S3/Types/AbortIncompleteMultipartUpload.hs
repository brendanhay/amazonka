{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AbortIncompleteMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AbortIncompleteMultipartUpload
  ( AbortIncompleteMultipartUpload (..),

    -- * Smart constructor
    mkAbortIncompleteMultipartUpload,

    -- * Lenses
    aimuDaysAfterInitiation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies the days since the initiation of an incomplete multipart upload that Amazon S3 will wait before permanently removing all parts of the upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkAbortIncompleteMultipartUpload' smart constructor.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload'
  { -- | Specifies the number of days after which Amazon S3 aborts an incomplete multipart upload.
    daysAfterInitiation :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortIncompleteMultipartUpload' with the minimum fields required to make a request.
--
-- * 'daysAfterInitiation' - Specifies the number of days after which Amazon S3 aborts an incomplete multipart upload.
mkAbortIncompleteMultipartUpload ::
  AbortIncompleteMultipartUpload
mkAbortIncompleteMultipartUpload =
  AbortIncompleteMultipartUpload'
    { daysAfterInitiation =
        Lude.Nothing
    }

-- | Specifies the number of days after which Amazon S3 aborts an incomplete multipart upload.
--
-- /Note:/ Consider using 'daysAfterInitiation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aimuDaysAfterInitiation :: Lens.Lens' AbortIncompleteMultipartUpload (Lude.Maybe Lude.Int)
aimuDaysAfterInitiation = Lens.lens (daysAfterInitiation :: AbortIncompleteMultipartUpload -> Lude.Maybe Lude.Int) (\s a -> s {daysAfterInitiation = a} :: AbortIncompleteMultipartUpload)
{-# DEPRECATED aimuDaysAfterInitiation "Use generic-lens or generic-optics with 'daysAfterInitiation' instead." #-}

instance Lude.FromXML AbortIncompleteMultipartUpload where
  parseXML x =
    AbortIncompleteMultipartUpload'
      Lude.<$> (x Lude..@? "DaysAfterInitiation")

instance Lude.ToXML AbortIncompleteMultipartUpload where
  toXML AbortIncompleteMultipartUpload' {..} =
    Lude.mconcat ["DaysAfterInitiation" Lude.@= daysAfterInitiation]
