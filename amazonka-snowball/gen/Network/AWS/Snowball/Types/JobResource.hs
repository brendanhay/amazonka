{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobResource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Snowball.Types.Ec2AmiResource
import Network.AWS.Snowball.Types.LambdaResource
import Network.AWS.Snowball.Types.S3Resource

-- | Contains an array of AWS resource objects. Each object represents an
-- Amazon S3 bucket, an AWS Lambda function, or an Amazon Machine Image
-- (AMI) based on Amazon EC2 that is associated with a particular job.
--
-- /See:/ 'newJobResource' smart constructor.
data JobResource = JobResource'
  { -- | An array of @S3Resource@ objects.
    s3Resources :: Core.Maybe [S3Resource],
    -- | The Amazon Machine Images (AMIs) associated with this job.
    ec2AmiResources :: Core.Maybe [Ec2AmiResource],
    -- | The Python-language Lambda functions for this job.
    lambdaResources :: Core.Maybe [LambdaResource]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Resources', 'jobResource_s3Resources' - An array of @S3Resource@ objects.
--
-- 'ec2AmiResources', 'jobResource_ec2AmiResources' - The Amazon Machine Images (AMIs) associated with this job.
--
-- 'lambdaResources', 'jobResource_lambdaResources' - The Python-language Lambda functions for this job.
newJobResource ::
  JobResource
newJobResource =
  JobResource'
    { s3Resources = Core.Nothing,
      ec2AmiResources = Core.Nothing,
      lambdaResources = Core.Nothing
    }

-- | An array of @S3Resource@ objects.
jobResource_s3Resources :: Lens.Lens' JobResource (Core.Maybe [S3Resource])
jobResource_s3Resources = Lens.lens (\JobResource' {s3Resources} -> s3Resources) (\s@JobResource' {} a -> s {s3Resources = a} :: JobResource) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Machine Images (AMIs) associated with this job.
jobResource_ec2AmiResources :: Lens.Lens' JobResource (Core.Maybe [Ec2AmiResource])
jobResource_ec2AmiResources = Lens.lens (\JobResource' {ec2AmiResources} -> ec2AmiResources) (\s@JobResource' {} a -> s {ec2AmiResources = a} :: JobResource) Core.. Lens.mapping Lens._Coerce

-- | The Python-language Lambda functions for this job.
jobResource_lambdaResources :: Lens.Lens' JobResource (Core.Maybe [LambdaResource])
jobResource_lambdaResources = Lens.lens (\JobResource' {lambdaResources} -> lambdaResources) (\s@JobResource' {} a -> s {lambdaResources = a} :: JobResource) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON JobResource where
  parseJSON =
    Core.withObject
      "JobResource"
      ( \x ->
          JobResource'
            Core.<$> (x Core..:? "S3Resources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Ec2AmiResources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LambdaResources" Core..!= Core.mempty)
      )

instance Core.Hashable JobResource

instance Core.NFData JobResource

instance Core.ToJSON JobResource where
  toJSON JobResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3Resources" Core..=) Core.<$> s3Resources,
            ("Ec2AmiResources" Core..=) Core.<$> ec2AmiResources,
            ("LambdaResources" Core..=)
              Core.<$> lambdaResources
          ]
      )
