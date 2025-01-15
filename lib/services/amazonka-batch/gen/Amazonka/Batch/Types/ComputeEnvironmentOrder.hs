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
-- Module      : Amazonka.Batch.Types.ComputeEnvironmentOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ComputeEnvironmentOrder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The order that compute environments are tried in for job placement
-- within a queue. Compute environments are tried in ascending order. For
-- example, if two compute environments are associated with a job queue,
-- the compute environment with a lower order integer value is tried for
-- job placement first. Compute environments must be in the @VALID@ state
-- before you can associate them with a job queue. All of the compute
-- environments must be either EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@
-- or @FARGATE_SPOT@); EC2 and Fargate compute environments can\'t be
-- mixed.
--
-- All compute environments that are associated with a job queue must share
-- the same architecture. Batch doesn\'t support mixing compute environment
-- architecture types in a single job queue.
--
-- /See:/ 'newComputeEnvironmentOrder' smart constructor.
data ComputeEnvironmentOrder = ComputeEnvironmentOrder'
  { -- | The order of the compute environment. Compute environments are tried in
    -- ascending order. For example, if two compute environments are associated
    -- with a job queue, the compute environment with a lower @order@ integer
    -- value is tried for job placement first.
    order :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeEnvironmentOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'order', 'computeEnvironmentOrder_order' - The order of the compute environment. Compute environments are tried in
-- ascending order. For example, if two compute environments are associated
-- with a job queue, the compute environment with a lower @order@ integer
-- value is tried for job placement first.
--
-- 'computeEnvironment', 'computeEnvironmentOrder_computeEnvironment' - The Amazon Resource Name (ARN) of the compute environment.
newComputeEnvironmentOrder ::
  -- | 'order'
  Prelude.Int ->
  -- | 'computeEnvironment'
  Prelude.Text ->
  ComputeEnvironmentOrder
newComputeEnvironmentOrder
  pOrder_
  pComputeEnvironment_ =
    ComputeEnvironmentOrder'
      { order = pOrder_,
        computeEnvironment = pComputeEnvironment_
      }

-- | The order of the compute environment. Compute environments are tried in
-- ascending order. For example, if two compute environments are associated
-- with a job queue, the compute environment with a lower @order@ integer
-- value is tried for job placement first.
computeEnvironmentOrder_order :: Lens.Lens' ComputeEnvironmentOrder Prelude.Int
computeEnvironmentOrder_order = Lens.lens (\ComputeEnvironmentOrder' {order} -> order) (\s@ComputeEnvironmentOrder' {} a -> s {order = a} :: ComputeEnvironmentOrder)

-- | The Amazon Resource Name (ARN) of the compute environment.
computeEnvironmentOrder_computeEnvironment :: Lens.Lens' ComputeEnvironmentOrder Prelude.Text
computeEnvironmentOrder_computeEnvironment = Lens.lens (\ComputeEnvironmentOrder' {computeEnvironment} -> computeEnvironment) (\s@ComputeEnvironmentOrder' {} a -> s {computeEnvironment = a} :: ComputeEnvironmentOrder)

instance Data.FromJSON ComputeEnvironmentOrder where
  parseJSON =
    Data.withObject
      "ComputeEnvironmentOrder"
      ( \x ->
          ComputeEnvironmentOrder'
            Prelude.<$> (x Data..: "order")
            Prelude.<*> (x Data..: "computeEnvironment")
      )

instance Prelude.Hashable ComputeEnvironmentOrder where
  hashWithSalt _salt ComputeEnvironmentOrder' {..} =
    _salt
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` computeEnvironment

instance Prelude.NFData ComputeEnvironmentOrder where
  rnf ComputeEnvironmentOrder' {..} =
    Prelude.rnf order `Prelude.seq`
      Prelude.rnf computeEnvironment

instance Data.ToJSON ComputeEnvironmentOrder where
  toJSON ComputeEnvironmentOrder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("order" Data..= order),
            Prelude.Just
              ("computeEnvironment" Data..= computeEnvironment)
          ]
      )
