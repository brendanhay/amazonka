{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobResource
  ( JobResource (..),

    -- * Smart constructor
    mkJobResource,

    -- * Lenses
    jrEC2AMIResources,
    jrLambdaResources,
    jrS3Resources,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.EC2AMIResource
import Network.AWS.Snowball.Types.LambdaResource
import Network.AWS.Snowball.Types.S3Resource

-- | Contains an array of AWS resource objects. Each object represents an Amazon S3 bucket, an AWS Lambda function, or an Amazon Machine Image (AMI) based on Amazon EC2 that is associated with a particular job.
--
-- /See:/ 'mkJobResource' smart constructor.
data JobResource = JobResource'
  { ec2AMIResources ::
      Lude.Maybe [EC2AMIResource],
    lambdaResources :: Lude.Maybe [LambdaResource],
    s3Resources :: Lude.Maybe [S3Resource]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobResource' with the minimum fields required to make a request.
--
-- * 'ec2AMIResources' - The Amazon Machine Images (AMIs) associated with this job.
-- * 'lambdaResources' - The Python-language Lambda functions for this job.
-- * 's3Resources' - An array of @S3Resource@ objects.
mkJobResource ::
  JobResource
mkJobResource =
  JobResource'
    { ec2AMIResources = Lude.Nothing,
      lambdaResources = Lude.Nothing,
      s3Resources = Lude.Nothing
    }

-- | The Amazon Machine Images (AMIs) associated with this job.
--
-- /Note:/ Consider using 'ec2AMIResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrEC2AMIResources :: Lens.Lens' JobResource (Lude.Maybe [EC2AMIResource])
jrEC2AMIResources = Lens.lens (ec2AMIResources :: JobResource -> Lude.Maybe [EC2AMIResource]) (\s a -> s {ec2AMIResources = a} :: JobResource)
{-# DEPRECATED jrEC2AMIResources "Use generic-lens or generic-optics with 'ec2AMIResources' instead." #-}

-- | The Python-language Lambda functions for this job.
--
-- /Note:/ Consider using 'lambdaResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLambdaResources :: Lens.Lens' JobResource (Lude.Maybe [LambdaResource])
jrLambdaResources = Lens.lens (lambdaResources :: JobResource -> Lude.Maybe [LambdaResource]) (\s a -> s {lambdaResources = a} :: JobResource)
{-# DEPRECATED jrLambdaResources "Use generic-lens or generic-optics with 'lambdaResources' instead." #-}

-- | An array of @S3Resource@ objects.
--
-- /Note:/ Consider using 's3Resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrS3Resources :: Lens.Lens' JobResource (Lude.Maybe [S3Resource])
jrS3Resources = Lens.lens (s3Resources :: JobResource -> Lude.Maybe [S3Resource]) (\s a -> s {s3Resources = a} :: JobResource)
{-# DEPRECATED jrS3Resources "Use generic-lens or generic-optics with 's3Resources' instead." #-}

instance Lude.FromJSON JobResource where
  parseJSON =
    Lude.withObject
      "JobResource"
      ( \x ->
          JobResource'
            Lude.<$> (x Lude..:? "Ec2AmiResources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LambdaResources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "S3Resources" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON JobResource where
  toJSON JobResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Ec2AmiResources" Lude..=) Lude.<$> ec2AMIResources,
            ("LambdaResources" Lude..=) Lude.<$> lambdaResources,
            ("S3Resources" Lude..=) Lude.<$> s3Resources
          ]
      )
