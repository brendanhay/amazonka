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
-- Module      : Amazonka.Inspector2.Types.ResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | Details the status of Amazon Inspector for each resource type Amazon
-- Inspector scans.
--
-- /See:/ 'newResourceStatus' smart constructor.
data ResourceStatus = ResourceStatus'
  { -- | The status of Amazon Inspector scanning for AWS Lambda function
    -- resources.
    lambda :: Prelude.Maybe Status,
    -- | The status of Amazon Inspector scanning for Amazon EC2 resources.
    ec2 :: Status,
    -- | The status of Amazon Inspector scanning for Amazon ECR resources.
    ecr :: Status
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambda', 'resourceStatus_lambda' - The status of Amazon Inspector scanning for AWS Lambda function
-- resources.
--
-- 'ec2', 'resourceStatus_ec2' - The status of Amazon Inspector scanning for Amazon EC2 resources.
--
-- 'ecr', 'resourceStatus_ecr' - The status of Amazon Inspector scanning for Amazon ECR resources.
newResourceStatus ::
  -- | 'ec2'
  Status ->
  -- | 'ecr'
  Status ->
  ResourceStatus
newResourceStatus pEc2_ pEcr_ =
  ResourceStatus'
    { lambda = Prelude.Nothing,
      ec2 = pEc2_,
      ecr = pEcr_
    }

-- | The status of Amazon Inspector scanning for AWS Lambda function
-- resources.
resourceStatus_lambda :: Lens.Lens' ResourceStatus (Prelude.Maybe Status)
resourceStatus_lambda = Lens.lens (\ResourceStatus' {lambda} -> lambda) (\s@ResourceStatus' {} a -> s {lambda = a} :: ResourceStatus)

-- | The status of Amazon Inspector scanning for Amazon EC2 resources.
resourceStatus_ec2 :: Lens.Lens' ResourceStatus Status
resourceStatus_ec2 = Lens.lens (\ResourceStatus' {ec2} -> ec2) (\s@ResourceStatus' {} a -> s {ec2 = a} :: ResourceStatus)

-- | The status of Amazon Inspector scanning for Amazon ECR resources.
resourceStatus_ecr :: Lens.Lens' ResourceStatus Status
resourceStatus_ecr = Lens.lens (\ResourceStatus' {ecr} -> ecr) (\s@ResourceStatus' {} a -> s {ecr = a} :: ResourceStatus)

instance Data.FromJSON ResourceStatus where
  parseJSON =
    Data.withObject
      "ResourceStatus"
      ( \x ->
          ResourceStatus'
            Prelude.<$> (x Data..:? "lambda")
            Prelude.<*> (x Data..: "ec2")
            Prelude.<*> (x Data..: "ecr")
      )

instance Prelude.Hashable ResourceStatus where
  hashWithSalt _salt ResourceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` ec2
      `Prelude.hashWithSalt` ecr

instance Prelude.NFData ResourceStatus where
  rnf ResourceStatus' {..} =
    Prelude.rnf lambda `Prelude.seq`
      Prelude.rnf ec2 `Prelude.seq`
        Prelude.rnf ecr
