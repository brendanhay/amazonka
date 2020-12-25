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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Specifies the days since the initiation of an incomplete multipart upload that Amazon S3 will wait before permanently removing all parts of the upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkAbortIncompleteMultipartUpload' smart constructor.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload'
  { -- | Specifies the number of days after which Amazon S3 aborts an incomplete multipart upload.
    daysAfterInitiation :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AbortIncompleteMultipartUpload' value with any optional fields omitted.
mkAbortIncompleteMultipartUpload ::
  AbortIncompleteMultipartUpload
mkAbortIncompleteMultipartUpload =
  AbortIncompleteMultipartUpload'
    { daysAfterInitiation =
        Core.Nothing
    }

-- | Specifies the number of days after which Amazon S3 aborts an incomplete multipart upload.
--
-- /Note:/ Consider using 'daysAfterInitiation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aimuDaysAfterInitiation :: Lens.Lens' AbortIncompleteMultipartUpload (Core.Maybe Core.Int)
aimuDaysAfterInitiation = Lens.field @"daysAfterInitiation"
{-# DEPRECATED aimuDaysAfterInitiation "Use generic-lens or generic-optics with 'daysAfterInitiation' instead." #-}

instance Core.ToXML AbortIncompleteMultipartUpload where
  toXML AbortIncompleteMultipartUpload {..} =
    Core.toXMLNode "DaysAfterInitiation" Core.<$> daysAfterInitiation

instance Core.FromXML AbortIncompleteMultipartUpload where
  parseXML x =
    AbortIncompleteMultipartUpload'
      Core.<$> (x Core..@? "DaysAfterInitiation")
