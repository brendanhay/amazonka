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
-- Module      : Amazonka.ComputeOptimizer.Types.JobFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.JobFilter where

import Amazonka.ComputeOptimizer.Types.JobFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter that returns a more specific list of recommendation
-- export jobs. Use this filter with the DescribeRecommendationExportJobs
-- action.
--
-- You can use @EBSFilter@ with the GetEBSVolumeRecommendations action,
-- @LambdaFunctionRecommendationFilter@ with the
-- GetLambdaFunctionRecommendations action, and @Filter@ with the
-- GetAutoScalingGroupRecommendations and GetEC2InstanceRecommendations
-- actions.
--
-- /See:/ 'newJobFilter' smart constructor.
data JobFilter = JobFilter'
  { -- | The name of the filter.
    --
    -- Specify @ResourceType@ to return export jobs of a specific resource type
    -- (for example, @Ec2Instance@).
    --
    -- Specify @JobStatus@ to return export jobs with a specific status (e.g,
    -- @Complete@).
    name :: Prelude.Maybe JobFilterName,
    -- | The value of the filter.
    --
    -- The valid values for this parameter are as follows, depending on what
    -- you specify for the @name@ parameter:
    --
    -- -   Specify @Ec2Instance@ or @AutoScalingGroup@ if you specify the
    --     @name@ parameter as @ResourceType@. There is no filter for EBS
    --     volumes because volume recommendations cannot be exported at this
    --     time.
    --
    -- -   Specify @Queued@, @InProgress@, @Complete@, or @Failed@ if you
    --     specify the @name@ parameter as @JobStatus@.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'jobFilter_name' - The name of the filter.
--
-- Specify @ResourceType@ to return export jobs of a specific resource type
-- (for example, @Ec2Instance@).
--
-- Specify @JobStatus@ to return export jobs with a specific status (e.g,
-- @Complete@).
--
-- 'values', 'jobFilter_values' - The value of the filter.
--
-- The valid values for this parameter are as follows, depending on what
-- you specify for the @name@ parameter:
--
-- -   Specify @Ec2Instance@ or @AutoScalingGroup@ if you specify the
--     @name@ parameter as @ResourceType@. There is no filter for EBS
--     volumes because volume recommendations cannot be exported at this
--     time.
--
-- -   Specify @Queued@, @InProgress@, @Complete@, or @Failed@ if you
--     specify the @name@ parameter as @JobStatus@.
newJobFilter ::
  JobFilter
newJobFilter =
  JobFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
--
-- Specify @ResourceType@ to return export jobs of a specific resource type
-- (for example, @Ec2Instance@).
--
-- Specify @JobStatus@ to return export jobs with a specific status (e.g,
-- @Complete@).
jobFilter_name :: Lens.Lens' JobFilter (Prelude.Maybe JobFilterName)
jobFilter_name = Lens.lens (\JobFilter' {name} -> name) (\s@JobFilter' {} a -> s {name = a} :: JobFilter)

-- | The value of the filter.
--
-- The valid values for this parameter are as follows, depending on what
-- you specify for the @name@ parameter:
--
-- -   Specify @Ec2Instance@ or @AutoScalingGroup@ if you specify the
--     @name@ parameter as @ResourceType@. There is no filter for EBS
--     volumes because volume recommendations cannot be exported at this
--     time.
--
-- -   Specify @Queued@, @InProgress@, @Complete@, or @Failed@ if you
--     specify the @name@ parameter as @JobStatus@.
jobFilter_values :: Lens.Lens' JobFilter (Prelude.Maybe [Prelude.Text])
jobFilter_values = Lens.lens (\JobFilter' {values} -> values) (\s@JobFilter' {} a -> s {values = a} :: JobFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable JobFilter where
  hashWithSalt _salt JobFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData JobFilter where
  rnf JobFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON JobFilter where
  toJSON JobFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
