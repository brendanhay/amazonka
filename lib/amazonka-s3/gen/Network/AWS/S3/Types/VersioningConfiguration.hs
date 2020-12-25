{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.VersioningConfiguration
  ( VersioningConfiguration (..),

    -- * Smart constructor
    mkVersioningConfiguration,

    -- * Lenses
    vcMFADelete,
    vcStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.BucketVersioningStatus as Types
import qualified Network.AWS.S3.Types.MFADelete as Types

-- | Describes the versioning state of an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTVersioningStatus.html PUT Bucket versioning> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkVersioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { -- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
    mFADelete :: Core.Maybe Types.MFADelete,
    -- | The versioning state of the bucket.
    status :: Core.Maybe Types.BucketVersioningStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VersioningConfiguration' value with any optional fields omitted.
mkVersioningConfiguration ::
  VersioningConfiguration
mkVersioningConfiguration =
  VersioningConfiguration'
    { mFADelete = Core.Nothing,
      status = Core.Nothing
    }

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
--
-- /Note:/ Consider using 'mFADelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcMFADelete :: Lens.Lens' VersioningConfiguration (Core.Maybe Types.MFADelete)
vcMFADelete = Lens.field @"mFADelete"
{-# DEPRECATED vcMFADelete "Use generic-lens or generic-optics with 'mFADelete' instead." #-}

-- | The versioning state of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcStatus :: Lens.Lens' VersioningConfiguration (Core.Maybe Types.BucketVersioningStatus)
vcStatus = Lens.field @"status"
{-# DEPRECATED vcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.ToXML VersioningConfiguration where
  toXML VersioningConfiguration {..} =
    Core.toXMLNode "MfaDelete" Core.<$> mFADelete
      Core.<> Core.toXMLNode "Status" Core.<$> status
