-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rResourceType,
    rS3BucketDetails,
    rInstanceDetails,
    rAccessKeyDetails,
  )
where

import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.S3BucketDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the AWS resource associated with the activity that prompted GuardDuty to generate a finding.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { resourceType :: Lude.Maybe Lude.Text,
    s3BucketDetails :: Lude.Maybe [S3BucketDetail],
    instanceDetails :: Lude.Maybe InstanceDetails,
    accessKeyDetails :: Lude.Maybe AccessKeyDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- * 'accessKeyDetails' - The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
-- * 'instanceDetails' - The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
-- * 'resourceType' - The type of AWS resource.
-- * 's3BucketDetails' - Contains information on the S3 bucket.
mkResource ::
  Resource
mkResource =
  Resource'
    { resourceType = Lude.Nothing,
      s3BucketDetails = Lude.Nothing,
      instanceDetails = Lude.Nothing,
      accessKeyDetails = Lude.Nothing
    }

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rResourceType = Lens.lens (resourceType :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: Resource)
{-# DEPRECATED rResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Contains information on the S3 bucket.
--
-- /Note:/ Consider using 's3BucketDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rS3BucketDetails :: Lens.Lens' Resource (Lude.Maybe [S3BucketDetail])
rS3BucketDetails = Lens.lens (s3BucketDetails :: Resource -> Lude.Maybe [S3BucketDetail]) (\s a -> s {s3BucketDetails = a} :: Resource)
{-# DEPRECATED rS3BucketDetails "Use generic-lens or generic-optics with 's3BucketDetails' instead." #-}

-- | The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
--
-- /Note:/ Consider using 'instanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceDetails :: Lens.Lens' Resource (Lude.Maybe InstanceDetails)
rInstanceDetails = Lens.lens (instanceDetails :: Resource -> Lude.Maybe InstanceDetails) (\s a -> s {instanceDetails = a} :: Resource)
{-# DEPRECATED rInstanceDetails "Use generic-lens or generic-optics with 'instanceDetails' instead." #-}

-- | The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
--
-- /Note:/ Consider using 'accessKeyDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAccessKeyDetails :: Lens.Lens' Resource (Lude.Maybe AccessKeyDetails)
rAccessKeyDetails = Lens.lens (accessKeyDetails :: Resource -> Lude.Maybe AccessKeyDetails) (\s a -> s {accessKeyDetails = a} :: Resource)
{-# DEPRECATED rAccessKeyDetails "Use generic-lens or generic-optics with 'accessKeyDetails' instead." #-}

instance Lude.FromJSON Resource where
  parseJSON =
    Lude.withObject
      "Resource"
      ( \x ->
          Resource'
            Lude.<$> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "s3BucketDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "instanceDetails")
            Lude.<*> (x Lude..:? "accessKeyDetails")
      )
