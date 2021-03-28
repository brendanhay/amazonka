{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.JobResource
  ( JobResource (..)
  -- * Smart constructor
  , mkJobResource
  -- * Lenses
  , jrEc2AmiResources
  , jrLambdaResources
  , jrS3Resources
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.Ec2AmiResource as Types
import qualified Network.AWS.Snowball.Types.LambdaResource as Types
import qualified Network.AWS.Snowball.Types.S3Resource as Types

-- | Contains an array of AWS resource objects. Each object represents an Amazon S3 bucket, an AWS Lambda function, or an Amazon Machine Image (AMI) based on Amazon EC2 that is associated with a particular job.
--
-- /See:/ 'mkJobResource' smart constructor.
data JobResource = JobResource'
  { ec2AmiResources :: Core.Maybe [Types.Ec2AmiResource]
    -- ^ The Amazon Machine Images (AMIs) associated with this job.
  , lambdaResources :: Core.Maybe [Types.LambdaResource]
    -- ^ The Python-language Lambda functions for this job.
  , s3Resources :: Core.Maybe [Types.S3Resource]
    -- ^ An array of @S3Resource@ objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobResource' value with any optional fields omitted.
mkJobResource
    :: JobResource
mkJobResource
  = JobResource'{ec2AmiResources = Core.Nothing,
                 lambdaResources = Core.Nothing, s3Resources = Core.Nothing}

-- | The Amazon Machine Images (AMIs) associated with this job.
--
-- /Note:/ Consider using 'ec2AmiResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrEc2AmiResources :: Lens.Lens' JobResource (Core.Maybe [Types.Ec2AmiResource])
jrEc2AmiResources = Lens.field @"ec2AmiResources"
{-# INLINEABLE jrEc2AmiResources #-}
{-# DEPRECATED ec2AmiResources "Use generic-lens or generic-optics with 'ec2AmiResources' instead"  #-}

-- | The Python-language Lambda functions for this job.
--
-- /Note:/ Consider using 'lambdaResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLambdaResources :: Lens.Lens' JobResource (Core.Maybe [Types.LambdaResource])
jrLambdaResources = Lens.field @"lambdaResources"
{-# INLINEABLE jrLambdaResources #-}
{-# DEPRECATED lambdaResources "Use generic-lens or generic-optics with 'lambdaResources' instead"  #-}

-- | An array of @S3Resource@ objects.
--
-- /Note:/ Consider using 's3Resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrS3Resources :: Lens.Lens' JobResource (Core.Maybe [Types.S3Resource])
jrS3Resources = Lens.field @"s3Resources"
{-# INLINEABLE jrS3Resources #-}
{-# DEPRECATED s3Resources "Use generic-lens or generic-optics with 's3Resources' instead"  #-}

instance Core.FromJSON JobResource where
        toJSON JobResource{..}
          = Core.object
              (Core.catMaybes
                 [("Ec2AmiResources" Core..=) Core.<$> ec2AmiResources,
                  ("LambdaResources" Core..=) Core.<$> lambdaResources,
                  ("S3Resources" Core..=) Core.<$> s3Resources])

instance Core.FromJSON JobResource where
        parseJSON
          = Core.withObject "JobResource" Core.$
              \ x ->
                JobResource' Core.<$>
                  (x Core..:? "Ec2AmiResources") Core.<*>
                    x Core..:? "LambdaResources"
                    Core.<*> x Core..:? "S3Resources"
