{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    s3Resources :: Prelude.Maybe [S3Resource],
    -- | The Amazon Machine Images (AMIs) associated with this job.
    ec2AmiResources :: Prelude.Maybe [Ec2AmiResource],
    -- | The Python-language Lambda functions for this job.
    lambdaResources :: Prelude.Maybe [LambdaResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { s3Resources = Prelude.Nothing,
      ec2AmiResources = Prelude.Nothing,
      lambdaResources = Prelude.Nothing
    }

-- | An array of @S3Resource@ objects.
jobResource_s3Resources :: Lens.Lens' JobResource (Prelude.Maybe [S3Resource])
jobResource_s3Resources = Lens.lens (\JobResource' {s3Resources} -> s3Resources) (\s@JobResource' {} a -> s {s3Resources = a} :: JobResource) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Machine Images (AMIs) associated with this job.
jobResource_ec2AmiResources :: Lens.Lens' JobResource (Prelude.Maybe [Ec2AmiResource])
jobResource_ec2AmiResources = Lens.lens (\JobResource' {ec2AmiResources} -> ec2AmiResources) (\s@JobResource' {} a -> s {ec2AmiResources = a} :: JobResource) Prelude.. Lens.mapping Prelude._Coerce

-- | The Python-language Lambda functions for this job.
jobResource_lambdaResources :: Lens.Lens' JobResource (Prelude.Maybe [LambdaResource])
jobResource_lambdaResources = Lens.lens (\JobResource' {lambdaResources} -> lambdaResources) (\s@JobResource' {} a -> s {lambdaResources = a} :: JobResource) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON JobResource where
  parseJSON =
    Prelude.withObject
      "JobResource"
      ( \x ->
          JobResource'
            Prelude.<$> ( x Prelude..:? "S3Resources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "Ec2AmiResources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "LambdaResources"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable JobResource

instance Prelude.NFData JobResource

instance Prelude.ToJSON JobResource where
  toJSON JobResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3Resources" Prelude..=) Prelude.<$> s3Resources,
            ("Ec2AmiResources" Prelude..=)
              Prelude.<$> ec2AmiResources,
            ("LambdaResources" Prelude..=)
              Prelude.<$> lambdaResources
          ]
      )
