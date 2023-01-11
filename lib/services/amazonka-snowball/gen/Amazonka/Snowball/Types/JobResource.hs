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
-- Module      : Amazonka.Snowball.Types.JobResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.JobResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.Ec2AmiResource
import Amazonka.Snowball.Types.LambdaResource
import Amazonka.Snowball.Types.S3Resource

-- | Contains an array of Amazon Web Services resource objects. Each object
-- represents an Amazon S3 bucket, an Lambda function, or an Amazon Machine
-- Image (AMI) based on Amazon EC2 that is associated with a particular
-- job.
--
-- /See:/ 'newJobResource' smart constructor.
data JobResource = JobResource'
  { -- | The Amazon Machine Images (AMIs) associated with this job.
    ec2AmiResources :: Prelude.Maybe [Ec2AmiResource],
    -- | The Python-language Lambda functions for this job.
    lambdaResources :: Prelude.Maybe [LambdaResource],
    -- | An array of @S3Resource@ objects.
    s3Resources :: Prelude.Maybe [S3Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2AmiResources', 'jobResource_ec2AmiResources' - The Amazon Machine Images (AMIs) associated with this job.
--
-- 'lambdaResources', 'jobResource_lambdaResources' - The Python-language Lambda functions for this job.
--
-- 's3Resources', 'jobResource_s3Resources' - An array of @S3Resource@ objects.
newJobResource ::
  JobResource
newJobResource =
  JobResource'
    { ec2AmiResources = Prelude.Nothing,
      lambdaResources = Prelude.Nothing,
      s3Resources = Prelude.Nothing
    }

-- | The Amazon Machine Images (AMIs) associated with this job.
jobResource_ec2AmiResources :: Lens.Lens' JobResource (Prelude.Maybe [Ec2AmiResource])
jobResource_ec2AmiResources = Lens.lens (\JobResource' {ec2AmiResources} -> ec2AmiResources) (\s@JobResource' {} a -> s {ec2AmiResources = a} :: JobResource) Prelude.. Lens.mapping Lens.coerced

-- | The Python-language Lambda functions for this job.
jobResource_lambdaResources :: Lens.Lens' JobResource (Prelude.Maybe [LambdaResource])
jobResource_lambdaResources = Lens.lens (\JobResource' {lambdaResources} -> lambdaResources) (\s@JobResource' {} a -> s {lambdaResources = a} :: JobResource) Prelude.. Lens.mapping Lens.coerced

-- | An array of @S3Resource@ objects.
jobResource_s3Resources :: Lens.Lens' JobResource (Prelude.Maybe [S3Resource])
jobResource_s3Resources = Lens.lens (\JobResource' {s3Resources} -> s3Resources) (\s@JobResource' {} a -> s {s3Resources = a} :: JobResource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JobResource where
  parseJSON =
    Data.withObject
      "JobResource"
      ( \x ->
          JobResource'
            Prelude.<$> ( x Data..:? "Ec2AmiResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "LambdaResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "S3Resources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JobResource where
  hashWithSalt _salt JobResource' {..} =
    _salt `Prelude.hashWithSalt` ec2AmiResources
      `Prelude.hashWithSalt` lambdaResources
      `Prelude.hashWithSalt` s3Resources

instance Prelude.NFData JobResource where
  rnf JobResource' {..} =
    Prelude.rnf ec2AmiResources
      `Prelude.seq` Prelude.rnf lambdaResources
      `Prelude.seq` Prelude.rnf s3Resources

instance Data.ToJSON JobResource where
  toJSON JobResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ec2AmiResources" Data..=)
              Prelude.<$> ec2AmiResources,
            ("LambdaResources" Data..=)
              Prelude.<$> lambdaResources,
            ("S3Resources" Data..=) Prelude.<$> s3Resources
          ]
      )
