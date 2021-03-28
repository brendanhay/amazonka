{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Resource
  ( Resource (..)
  -- * Smart constructor
  , mkResource
  -- * Lenses
  , rAccessKeyDetails
  , rInstanceDetails
  , rResourceType
  , rS3BucketDetails
  ) where

import qualified Network.AWS.GuardDuty.Types.AccessKeyDetails as Types
import qualified Network.AWS.GuardDuty.Types.InstanceDetails as Types
import qualified Network.AWS.GuardDuty.Types.S3BucketDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the AWS resource associated with the activity that prompted GuardDuty to generate a finding.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { accessKeyDetails :: Core.Maybe Types.AccessKeyDetails
    -- ^ The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
  , instanceDetails :: Core.Maybe Types.InstanceDetails
    -- ^ The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
  , resourceType :: Core.Maybe Core.Text
    -- ^ The type of AWS resource.
  , s3BucketDetails :: Core.Maybe [Types.S3BucketDetail]
    -- ^ Contains information on the S3 bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Resource' value with any optional fields omitted.
mkResource
    :: Resource
mkResource
  = Resource'{accessKeyDetails = Core.Nothing,
              instanceDetails = Core.Nothing, resourceType = Core.Nothing,
              s3BucketDetails = Core.Nothing}

-- | The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
--
-- /Note:/ Consider using 'accessKeyDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAccessKeyDetails :: Lens.Lens' Resource (Core.Maybe Types.AccessKeyDetails)
rAccessKeyDetails = Lens.field @"accessKeyDetails"
{-# INLINEABLE rAccessKeyDetails #-}
{-# DEPRECATED accessKeyDetails "Use generic-lens or generic-optics with 'accessKeyDetails' instead"  #-}

-- | The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
--
-- /Note:/ Consider using 'instanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceDetails :: Lens.Lens' Resource (Core.Maybe Types.InstanceDetails)
rInstanceDetails = Lens.field @"instanceDetails"
{-# INLINEABLE rInstanceDetails #-}
{-# DEPRECATED instanceDetails "Use generic-lens or generic-optics with 'instanceDetails' instead"  #-}

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' Resource (Core.Maybe Core.Text)
rResourceType = Lens.field @"resourceType"
{-# INLINEABLE rResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Contains information on the S3 bucket.
--
-- /Note:/ Consider using 's3BucketDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rS3BucketDetails :: Lens.Lens' Resource (Core.Maybe [Types.S3BucketDetail])
rS3BucketDetails = Lens.field @"s3BucketDetails"
{-# INLINEABLE rS3BucketDetails #-}
{-# DEPRECATED s3BucketDetails "Use generic-lens or generic-optics with 's3BucketDetails' instead"  #-}

instance Core.FromJSON Resource where
        parseJSON
          = Core.withObject "Resource" Core.$
              \ x ->
                Resource' Core.<$>
                  (x Core..:? "accessKeyDetails") Core.<*>
                    x Core..:? "instanceDetails"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "s3BucketDetails"
