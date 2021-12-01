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
-- Module      : Amazonka.DevOpsGuru.Types.CostEstimationResourceCollectionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CostEstimationResourceCollectionFilter where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.CloudFormationCostEstimationResourceCollectionFilter
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a filter used to specify which AWS resources are
-- analyzed to create a monthly DevOps Guru cost estimate. For more
-- information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/cost-estimate.html Estimate your Amazon DevOps Guru costs>
-- and
-- <http://aws.amazon.com/devops-guru/pricing/ Amazon DevOps Guru pricing>.
--
-- /See:/ 'newCostEstimationResourceCollectionFilter' smart constructor.
data CostEstimationResourceCollectionFilter = CostEstimationResourceCollectionFilter'
  { -- | An object that specifies the CloudFormation stack that defines the AWS
    -- resources used to create a monthly estimate for DevOps Guru.
    cloudFormation :: Prelude.Maybe CloudFormationCostEstimationResourceCollectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostEstimationResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormation', 'costEstimationResourceCollectionFilter_cloudFormation' - An object that specifies the CloudFormation stack that defines the AWS
-- resources used to create a monthly estimate for DevOps Guru.
newCostEstimationResourceCollectionFilter ::
  CostEstimationResourceCollectionFilter
newCostEstimationResourceCollectionFilter =
  CostEstimationResourceCollectionFilter'
    { cloudFormation =
        Prelude.Nothing
    }

-- | An object that specifies the CloudFormation stack that defines the AWS
-- resources used to create a monthly estimate for DevOps Guru.
costEstimationResourceCollectionFilter_cloudFormation :: Lens.Lens' CostEstimationResourceCollectionFilter (Prelude.Maybe CloudFormationCostEstimationResourceCollectionFilter)
costEstimationResourceCollectionFilter_cloudFormation = Lens.lens (\CostEstimationResourceCollectionFilter' {cloudFormation} -> cloudFormation) (\s@CostEstimationResourceCollectionFilter' {} a -> s {cloudFormation = a} :: CostEstimationResourceCollectionFilter)

instance
  Core.FromJSON
    CostEstimationResourceCollectionFilter
  where
  parseJSON =
    Core.withObject
      "CostEstimationResourceCollectionFilter"
      ( \x ->
          CostEstimationResourceCollectionFilter'
            Prelude.<$> (x Core..:? "CloudFormation")
      )

instance
  Prelude.Hashable
    CostEstimationResourceCollectionFilter
  where
  hashWithSalt
    salt'
    CostEstimationResourceCollectionFilter' {..} =
      salt' `Prelude.hashWithSalt` cloudFormation

instance
  Prelude.NFData
    CostEstimationResourceCollectionFilter
  where
  rnf CostEstimationResourceCollectionFilter' {..} =
    Prelude.rnf cloudFormation

instance
  Core.ToJSON
    CostEstimationResourceCollectionFilter
  where
  toJSON CostEstimationResourceCollectionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CloudFormation" Core..=)
              Prelude.<$> cloudFormation
          ]
      )
