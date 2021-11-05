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
-- Module      : Network.AWS.ComputeOptimizer.Types.JobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComputeOptimizer.Types.JobFilter where

import Network.AWS.ComputeOptimizer.Types.JobFilterName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The value of the filter.
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
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter.
    --
    -- Specify @ResourceType@ to return export jobs of a specific resource type
    -- (for example, @Ec2Instance@).
    --
    -- Specify @JobStatus@ to return export jobs with a specific status (e.g,
    -- @Complete@).
    name :: Prelude.Maybe JobFilterName
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
--
-- 'name', 'jobFilter_name' - The name of the filter.
--
-- Specify @ResourceType@ to return export jobs of a specific resource type
-- (for example, @Ec2Instance@).
--
-- Specify @JobStatus@ to return export jobs with a specific status (e.g,
-- @Complete@).
newJobFilter ::
  JobFilter
newJobFilter =
  JobFilter'
    { values = Prelude.Nothing,
      name = Prelude.Nothing
    }

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

-- | The name of the filter.
--
-- Specify @ResourceType@ to return export jobs of a specific resource type
-- (for example, @Ec2Instance@).
--
-- Specify @JobStatus@ to return export jobs with a specific status (e.g,
-- @Complete@).
jobFilter_name :: Lens.Lens' JobFilter (Prelude.Maybe JobFilterName)
jobFilter_name = Lens.lens (\JobFilter' {name} -> name) (\s@JobFilter' {} a -> s {name = a} :: JobFilter)

instance Prelude.Hashable JobFilter

instance Prelude.NFData JobFilter

instance Core.ToJSON JobFilter where
  toJSON JobFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
