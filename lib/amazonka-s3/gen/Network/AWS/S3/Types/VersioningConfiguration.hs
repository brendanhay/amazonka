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
    vcStatus,
    vcMFADelete,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.BucketVersioningStatus
import Network.AWS.S3.Types.MFADelete

-- | Describes the versioning state of an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTVersioningStatus.html PUT Bucket versioning> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkVersioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { status ::
      Lude.Maybe BucketVersioningStatus,
    mfaDelete :: Lude.Maybe MFADelete
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VersioningConfiguration' with the minimum fields required to make a request.
--
-- * 'mfaDelete' - Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
-- * 'status' - The versioning state of the bucket.
mkVersioningConfiguration ::
  VersioningConfiguration
mkVersioningConfiguration =
  VersioningConfiguration'
    { status = Lude.Nothing,
      mfaDelete = Lude.Nothing
    }

-- | The versioning state of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcStatus :: Lens.Lens' VersioningConfiguration (Lude.Maybe BucketVersioningStatus)
vcStatus = Lens.lens (status :: VersioningConfiguration -> Lude.Maybe BucketVersioningStatus) (\s a -> s {status = a} :: VersioningConfiguration)
{-# DEPRECATED vcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
--
-- /Note:/ Consider using 'mfaDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcMFADelete :: Lens.Lens' VersioningConfiguration (Lude.Maybe MFADelete)
vcMFADelete = Lens.lens (mfaDelete :: VersioningConfiguration -> Lude.Maybe MFADelete) (\s a -> s {mfaDelete = a} :: VersioningConfiguration)
{-# DEPRECATED vcMFADelete "Use generic-lens or generic-optics with 'mfaDelete' instead." #-}

instance Lude.ToXML VersioningConfiguration where
  toXML VersioningConfiguration' {..} =
    Lude.mconcat
      ["Status" Lude.@= status, "MfaDelete" Lude.@= mfaDelete]
