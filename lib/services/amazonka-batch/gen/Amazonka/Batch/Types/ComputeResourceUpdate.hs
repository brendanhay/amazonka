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
-- Module      : Amazonka.Batch.Types.ComputeResourceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ComputeResourceUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the attributes of a compute environment that can
-- be updated. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /Batch User Guide/.
--
-- /See:/ 'newComputeResourceUpdate' smart constructor.
data ComputeResourceUpdate = ComputeResourceUpdate'
  { -- | The minimum number of Amazon EC2 vCPUs that an environment should
    -- maintain.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources, and shouldn\'t be specified.
    minvCpus :: Prelude.Maybe Prelude.Int,
    -- | The Amazon EC2 security groups associated with instances launched in the
    -- compute environment. This parameter is required for Fargate compute
    -- resources, where it can contain up to 5 security groups. This can\'t be
    -- specified for EC2 compute resources. Providing an empty list is handled
    -- as if this parameter wasn\'t specified and no change is made.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The desired number of Amazon EC2 vCPUS in the compute environment.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources, and shouldn\'t be specified.
    desiredvCpus :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
    --
    -- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
    -- allocation strategies, Batch might need to exceed @maxvCpus@ to meet
    -- your capacity requirements. In this event, Batch never exceeds
    -- @maxvCpus@ by more than a single instance. That is, no more than a
    -- single instance from among those specified in your compute environment.
    maxvCpus :: Prelude.Maybe Prelude.Int,
    -- | The VPC subnets where the compute resources are launched. Fargate
    -- compute resources can contain up to 16 subnets. Providing an empty list
    -- will be handled as if this parameter wasn\'t specified and no change is
    -- made. This can\'t be specified for EC2 compute resources. For more
    -- information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
    -- in the /Amazon VPC User Guide/.
    subnets :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeResourceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minvCpus', 'computeResourceUpdate_minvCpus' - The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources, and shouldn\'t be specified.
--
-- 'securityGroupIds', 'computeResourceUpdate_securityGroupIds' - The Amazon EC2 security groups associated with instances launched in the
-- compute environment. This parameter is required for Fargate compute
-- resources, where it can contain up to 5 security groups. This can\'t be
-- specified for EC2 compute resources. Providing an empty list is handled
-- as if this parameter wasn\'t specified and no change is made.
--
-- 'desiredvCpus', 'computeResourceUpdate_desiredvCpus' - The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources, and shouldn\'t be specified.
--
-- 'maxvCpus', 'computeResourceUpdate_maxvCpus' - The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies, Batch might need to exceed @maxvCpus@ to meet
-- your capacity requirements. In this event, Batch never exceeds
-- @maxvCpus@ by more than a single instance. That is, no more than a
-- single instance from among those specified in your compute environment.
--
-- 'subnets', 'computeResourceUpdate_subnets' - The VPC subnets where the compute resources are launched. Fargate
-- compute resources can contain up to 16 subnets. Providing an empty list
-- will be handled as if this parameter wasn\'t specified and no change is
-- made. This can\'t be specified for EC2 compute resources. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
-- in the /Amazon VPC User Guide/.
newComputeResourceUpdate ::
  ComputeResourceUpdate
newComputeResourceUpdate =
  ComputeResourceUpdate'
    { minvCpus = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      desiredvCpus = Prelude.Nothing,
      maxvCpus = Prelude.Nothing,
      subnets = Prelude.Nothing
    }

-- | The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources, and shouldn\'t be specified.
computeResourceUpdate_minvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_minvCpus = Lens.lens (\ComputeResourceUpdate' {minvCpus} -> minvCpus) (\s@ComputeResourceUpdate' {} a -> s {minvCpus = a} :: ComputeResourceUpdate)

-- | The Amazon EC2 security groups associated with instances launched in the
-- compute environment. This parameter is required for Fargate compute
-- resources, where it can contain up to 5 security groups. This can\'t be
-- specified for EC2 compute resources. Providing an empty list is handled
-- as if this parameter wasn\'t specified and no change is made.
computeResourceUpdate_securityGroupIds :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Prelude.Text])
computeResourceUpdate_securityGroupIds = Lens.lens (\ComputeResourceUpdate' {securityGroupIds} -> securityGroupIds) (\s@ComputeResourceUpdate' {} a -> s {securityGroupIds = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources, and shouldn\'t be specified.
computeResourceUpdate_desiredvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_desiredvCpus = Lens.lens (\ComputeResourceUpdate' {desiredvCpus} -> desiredvCpus) (\s@ComputeResourceUpdate' {} a -> s {desiredvCpus = a} :: ComputeResourceUpdate)

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies, Batch might need to exceed @maxvCpus@ to meet
-- your capacity requirements. In this event, Batch never exceeds
-- @maxvCpus@ by more than a single instance. That is, no more than a
-- single instance from among those specified in your compute environment.
computeResourceUpdate_maxvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_maxvCpus = Lens.lens (\ComputeResourceUpdate' {maxvCpus} -> maxvCpus) (\s@ComputeResourceUpdate' {} a -> s {maxvCpus = a} :: ComputeResourceUpdate)

-- | The VPC subnets where the compute resources are launched. Fargate
-- compute resources can contain up to 16 subnets. Providing an empty list
-- will be handled as if this parameter wasn\'t specified and no change is
-- made. This can\'t be specified for EC2 compute resources. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
-- in the /Amazon VPC User Guide/.
computeResourceUpdate_subnets :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Prelude.Text])
computeResourceUpdate_subnets = Lens.lens (\ComputeResourceUpdate' {subnets} -> subnets) (\s@ComputeResourceUpdate' {} a -> s {subnets = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ComputeResourceUpdate where
  hashWithSalt _salt ComputeResourceUpdate' {..} =
    _salt `Prelude.hashWithSalt` minvCpus
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` desiredvCpus
      `Prelude.hashWithSalt` maxvCpus
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData ComputeResourceUpdate where
  rnf ComputeResourceUpdate' {..} =
    Prelude.rnf minvCpus
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf desiredvCpus
      `Prelude.seq` Prelude.rnf maxvCpus
      `Prelude.seq` Prelude.rnf subnets

instance Core.ToJSON ComputeResourceUpdate where
  toJSON ComputeResourceUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("minvCpus" Core..=) Prelude.<$> minvCpus,
            ("securityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("desiredvCpus" Core..=) Prelude.<$> desiredvCpus,
            ("maxvCpus" Core..=) Prelude.<$> maxvCpus,
            ("subnets" Core..=) Prelude.<$> subnets
          ]
      )
