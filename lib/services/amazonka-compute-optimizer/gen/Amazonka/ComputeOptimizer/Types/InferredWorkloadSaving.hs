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
-- Module      : Amazonka.ComputeOptimizer.Types.InferredWorkloadSaving
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.InferredWorkloadSaving where

import Amazonka.ComputeOptimizer.Types.EstimatedMonthlySavings
import Amazonka.ComputeOptimizer.Types.InferredWorkloadType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The estimated monthly savings after you adjust the configurations of
-- your instances running on the inferred workload types to the recommended
-- configurations. If the @inferredWorkloadTypes@ list contains multiple
-- entries, then the savings are the sum of the monthly savings from
-- instances that run the exact combination of the inferred workload types.
--
-- /See:/ 'newInferredWorkloadSaving' smart constructor.
data InferredWorkloadSaving = InferredWorkloadSaving'
  { -- | An object that describes the estimated monthly savings amount possible
    -- by adopting Compute Optimizer recommendations for a given resource. This
    -- is based on the On-Demand instance pricing.
    estimatedMonthlySavings :: Prelude.Maybe EstimatedMonthlySavings,
    -- | The applications that might be running on the instance as inferred by
    -- Compute Optimizer.
    --
    -- Compute Optimizer can infer if one of the following applications might
    -- be running on the instance:
    --
    -- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
    --     instance.
    --
    -- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
    --     the instance.
    --
    -- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
    --     instance.
    --
    -- -   @Memcached@ - Infers that Memcached might be running on the
    --     instance.
    --
    -- -   @NGINX@ - Infers that NGINX might be running on the instance.
    --
    -- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
    --     instance.
    --
    -- -   @Redis@ - Infers that Redis might be running on the instance.
    --
    -- -   @Kafka@ - Infers that Kafka might be running on the instance.
    --
    -- -   @SQLServer@ - Infers that SQLServer might be running on the
    --     instance.
    inferredWorkloadTypes :: Prelude.Maybe [InferredWorkloadType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferredWorkloadSaving' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlySavings', 'inferredWorkloadSaving_estimatedMonthlySavings' - An object that describes the estimated monthly savings amount possible
-- by adopting Compute Optimizer recommendations for a given resource. This
-- is based on the On-Demand instance pricing.
--
-- 'inferredWorkloadTypes', 'inferredWorkloadSaving_inferredWorkloadTypes' - The applications that might be running on the instance as inferred by
-- Compute Optimizer.
--
-- Compute Optimizer can infer if one of the following applications might
-- be running on the instance:
--
-- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
--     instance.
--
-- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
--     the instance.
--
-- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
--     instance.
--
-- -   @Memcached@ - Infers that Memcached might be running on the
--     instance.
--
-- -   @NGINX@ - Infers that NGINX might be running on the instance.
--
-- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
--     instance.
--
-- -   @Redis@ - Infers that Redis might be running on the instance.
--
-- -   @Kafka@ - Infers that Kafka might be running on the instance.
--
-- -   @SQLServer@ - Infers that SQLServer might be running on the
--     instance.
newInferredWorkloadSaving ::
  InferredWorkloadSaving
newInferredWorkloadSaving =
  InferredWorkloadSaving'
    { estimatedMonthlySavings =
        Prelude.Nothing,
      inferredWorkloadTypes = Prelude.Nothing
    }

-- | An object that describes the estimated monthly savings amount possible
-- by adopting Compute Optimizer recommendations for a given resource. This
-- is based on the On-Demand instance pricing.
inferredWorkloadSaving_estimatedMonthlySavings :: Lens.Lens' InferredWorkloadSaving (Prelude.Maybe EstimatedMonthlySavings)
inferredWorkloadSaving_estimatedMonthlySavings = Lens.lens (\InferredWorkloadSaving' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@InferredWorkloadSaving' {} a -> s {estimatedMonthlySavings = a} :: InferredWorkloadSaving)

-- | The applications that might be running on the instance as inferred by
-- Compute Optimizer.
--
-- Compute Optimizer can infer if one of the following applications might
-- be running on the instance:
--
-- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
--     instance.
--
-- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
--     the instance.
--
-- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
--     instance.
--
-- -   @Memcached@ - Infers that Memcached might be running on the
--     instance.
--
-- -   @NGINX@ - Infers that NGINX might be running on the instance.
--
-- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
--     instance.
--
-- -   @Redis@ - Infers that Redis might be running on the instance.
--
-- -   @Kafka@ - Infers that Kafka might be running on the instance.
--
-- -   @SQLServer@ - Infers that SQLServer might be running on the
--     instance.
inferredWorkloadSaving_inferredWorkloadTypes :: Lens.Lens' InferredWorkloadSaving (Prelude.Maybe [InferredWorkloadType])
inferredWorkloadSaving_inferredWorkloadTypes = Lens.lens (\InferredWorkloadSaving' {inferredWorkloadTypes} -> inferredWorkloadTypes) (\s@InferredWorkloadSaving' {} a -> s {inferredWorkloadTypes = a} :: InferredWorkloadSaving) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InferredWorkloadSaving where
  parseJSON =
    Data.withObject
      "InferredWorkloadSaving"
      ( \x ->
          InferredWorkloadSaving'
            Prelude.<$> (x Data..:? "estimatedMonthlySavings")
            Prelude.<*> ( x
                            Data..:? "inferredWorkloadTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InferredWorkloadSaving where
  hashWithSalt _salt InferredWorkloadSaving' {..} =
    _salt
      `Prelude.hashWithSalt` estimatedMonthlySavings
      `Prelude.hashWithSalt` inferredWorkloadTypes

instance Prelude.NFData InferredWorkloadSaving where
  rnf InferredWorkloadSaving' {..} =
    Prelude.rnf estimatedMonthlySavings
      `Prelude.seq` Prelude.rnf inferredWorkloadTypes
