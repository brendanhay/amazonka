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
-- Module      : Amazonka.DevOpsGuru.Types.ResourceCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ResourceCollection where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.CloudFormationCollection
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A collection of AWS resources supported by DevOps Guru. The one type of
-- AWS resource collection supported is AWS CloudFormation stacks. DevOps
-- Guru can be configured to analyze only the AWS resources that are
-- defined in the stacks. You can specify up to 500 AWS CloudFormation
-- stacks.
--
-- /See:/ 'newResourceCollection' smart constructor.
data ResourceCollection = ResourceCollection'
  { -- | An array of the names of AWS CloudFormation stacks. The stacks define
    -- AWS resources that DevOps Guru analyzes. You can specify up to 500 AWS
    -- CloudFormation stacks.
    cloudFormation :: Prelude.Maybe CloudFormationCollection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormation', 'resourceCollection_cloudFormation' - An array of the names of AWS CloudFormation stacks. The stacks define
-- AWS resources that DevOps Guru analyzes. You can specify up to 500 AWS
-- CloudFormation stacks.
newResourceCollection ::
  ResourceCollection
newResourceCollection =
  ResourceCollection'
    { cloudFormation =
        Prelude.Nothing
    }

-- | An array of the names of AWS CloudFormation stacks. The stacks define
-- AWS resources that DevOps Guru analyzes. You can specify up to 500 AWS
-- CloudFormation stacks.
resourceCollection_cloudFormation :: Lens.Lens' ResourceCollection (Prelude.Maybe CloudFormationCollection)
resourceCollection_cloudFormation = Lens.lens (\ResourceCollection' {cloudFormation} -> cloudFormation) (\s@ResourceCollection' {} a -> s {cloudFormation = a} :: ResourceCollection)

instance Core.FromJSON ResourceCollection where
  parseJSON =
    Core.withObject
      "ResourceCollection"
      ( \x ->
          ResourceCollection'
            Prelude.<$> (x Core..:? "CloudFormation")
      )

instance Prelude.Hashable ResourceCollection where
  hashWithSalt _salt ResourceCollection' {..} =
    _salt `Prelude.hashWithSalt` cloudFormation

instance Prelude.NFData ResourceCollection where
  rnf ResourceCollection' {..} =
    Prelude.rnf cloudFormation

instance Core.ToJSON ResourceCollection where
  toJSON ResourceCollection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CloudFormation" Core..=)
              Prelude.<$> cloudFormation
          ]
      )
