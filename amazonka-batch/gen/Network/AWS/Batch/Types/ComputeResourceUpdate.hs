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
-- Module      : Network.AWS.Batch.Types.ComputeResourceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeResourceUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the attributes of a compute environment that can
-- be updated. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/compute_environments.html Compute Environments>
-- in the /AWS Batch User Guide/.
--
-- /See:/ 'newComputeResourceUpdate' smart constructor.
data ComputeResourceUpdate = ComputeResourceUpdate'
  { -- | The Amazon EC2 security groups associated with instances launched in the
    -- compute environment. This parameter is required for Fargate compute
    -- resources, where it can contain up to 5 security groups. This can\'t be
    -- specified for EC2 compute resources. Providing an empty list is handled
    -- as if this parameter wasn\'t specified and no change is made.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The minimum number of Amazon EC2 vCPUs that an environment should
    -- maintain.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    minvCpus :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
    --
    -- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
    -- allocation strategies, AWS Batch might need to go above @maxvCpus@ to
    -- meet your capacity requirements. In this event, AWS Batch will never go
    -- above @maxvCpus@ by more than a single instance (e.g., no more than a
    -- single instance from among those specified in your compute environment).
    maxvCpus :: Prelude.Maybe Prelude.Int,
    -- | The desired number of Amazon EC2 vCPUS in the compute environment.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources,
    -- and shouldn\'t be specified.
    desiredvCpus :: Prelude.Maybe Prelude.Int,
    -- | The VPC subnets that the compute resources are launched into. This
    -- parameter is required for jobs running on Fargate compute resources,
    -- where it can contain up to 16 subnets. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
    -- in the /Amazon VPC User Guide/. This can\'t be specified for EC2 compute
    -- resources. Providing an empty list will be handled as if this parameter
    -- wasn\'t specified and no change is made.
    subnets :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComputeResourceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'computeResourceUpdate_securityGroupIds' - The Amazon EC2 security groups associated with instances launched in the
-- compute environment. This parameter is required for Fargate compute
-- resources, where it can contain up to 5 security groups. This can\'t be
-- specified for EC2 compute resources. Providing an empty list is handled
-- as if this parameter wasn\'t specified and no change is made.
--
-- 'minvCpus', 'computeResourceUpdate_minvCpus' - The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'maxvCpus', 'computeResourceUpdate_maxvCpus' - The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies, AWS Batch might need to go above @maxvCpus@ to
-- meet your capacity requirements. In this event, AWS Batch will never go
-- above @maxvCpus@ by more than a single instance (e.g., no more than a
-- single instance from among those specified in your compute environment).
--
-- 'desiredvCpus', 'computeResourceUpdate_desiredvCpus' - The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
--
-- 'subnets', 'computeResourceUpdate_subnets' - The VPC subnets that the compute resources are launched into. This
-- parameter is required for jobs running on Fargate compute resources,
-- where it can contain up to 16 subnets. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
-- in the /Amazon VPC User Guide/. This can\'t be specified for EC2 compute
-- resources. Providing an empty list will be handled as if this parameter
-- wasn\'t specified and no change is made.
newComputeResourceUpdate ::
  ComputeResourceUpdate
newComputeResourceUpdate =
  ComputeResourceUpdate'
    { securityGroupIds =
        Prelude.Nothing,
      minvCpus = Prelude.Nothing,
      maxvCpus = Prelude.Nothing,
      desiredvCpus = Prelude.Nothing,
      subnets = Prelude.Nothing
    }

-- | The Amazon EC2 security groups associated with instances launched in the
-- compute environment. This parameter is required for Fargate compute
-- resources, where it can contain up to 5 security groups. This can\'t be
-- specified for EC2 compute resources. Providing an empty list is handled
-- as if this parameter wasn\'t specified and no change is made.
computeResourceUpdate_securityGroupIds :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Prelude.Text])
computeResourceUpdate_securityGroupIds = Lens.lens (\ComputeResourceUpdate' {securityGroupIds} -> securityGroupIds) (\s@ComputeResourceUpdate' {} a -> s {securityGroupIds = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Prelude._Coerce

-- | The minimum number of Amazon EC2 vCPUs that an environment should
-- maintain.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResourceUpdate_minvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_minvCpus = Lens.lens (\ComputeResourceUpdate' {minvCpus} -> minvCpus) (\s@ComputeResourceUpdate' {} a -> s {minvCpus = a} :: ComputeResourceUpdate)

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- With both @BEST_FIT_PROGRESSIVE@ and @SPOT_CAPACITY_OPTIMIZED@
-- allocation strategies, AWS Batch might need to go above @maxvCpus@ to
-- meet your capacity requirements. In this event, AWS Batch will never go
-- above @maxvCpus@ by more than a single instance (e.g., no more than a
-- single instance from among those specified in your compute environment).
computeResourceUpdate_maxvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_maxvCpus = Lens.lens (\ComputeResourceUpdate' {maxvCpus} -> maxvCpus) (\s@ComputeResourceUpdate' {} a -> s {maxvCpus = a} :: ComputeResourceUpdate)

-- | The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources,
-- and shouldn\'t be specified.
computeResourceUpdate_desiredvCpus :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe Prelude.Int)
computeResourceUpdate_desiredvCpus = Lens.lens (\ComputeResourceUpdate' {desiredvCpus} -> desiredvCpus) (\s@ComputeResourceUpdate' {} a -> s {desiredvCpus = a} :: ComputeResourceUpdate)

-- | The VPC subnets that the compute resources are launched into. This
-- parameter is required for jobs running on Fargate compute resources,
-- where it can contain up to 16 subnets. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>
-- in the /Amazon VPC User Guide/. This can\'t be specified for EC2 compute
-- resources. Providing an empty list will be handled as if this parameter
-- wasn\'t specified and no change is made.
computeResourceUpdate_subnets :: Lens.Lens' ComputeResourceUpdate (Prelude.Maybe [Prelude.Text])
computeResourceUpdate_subnets = Lens.lens (\ComputeResourceUpdate' {subnets} -> subnets) (\s@ComputeResourceUpdate' {} a -> s {subnets = a} :: ComputeResourceUpdate) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable ComputeResourceUpdate

instance Prelude.NFData ComputeResourceUpdate

instance Prelude.ToJSON ComputeResourceUpdate where
  toJSON ComputeResourceUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Prelude..=)
              Prelude.<$> securityGroupIds,
            ("minvCpus" Prelude..=) Prelude.<$> minvCpus,
            ("maxvCpus" Prelude..=) Prelude.<$> maxvCpus,
            ("desiredvCpus" Prelude..=) Prelude.<$> desiredvCpus,
            ("subnets" Prelude..=) Prelude.<$> subnets
          ]
      )
