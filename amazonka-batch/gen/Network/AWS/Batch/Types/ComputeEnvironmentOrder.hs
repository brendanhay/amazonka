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
-- Module      : Network.AWS.Batch.Types.ComputeEnvironmentOrder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeEnvironmentOrder where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The order in which compute environments are tried for job placement
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
-- the same architecture. AWS Batch doesn\'t support mixing compute
-- environment architecture types in a single job queue.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ComputeEnvironmentOrder where
  parseJSON =
    Prelude.withObject
      "ComputeEnvironmentOrder"
      ( \x ->
          ComputeEnvironmentOrder'
            Prelude.<$> (x Prelude..: "order")
            Prelude.<*> (x Prelude..: "computeEnvironment")
      )

instance Prelude.Hashable ComputeEnvironmentOrder

instance Prelude.NFData ComputeEnvironmentOrder

instance Prelude.ToJSON ComputeEnvironmentOrder where
  toJSON ComputeEnvironmentOrder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("order" Prelude..= order),
            Prelude.Just
              ( "computeEnvironment"
                  Prelude..= computeEnvironment
              )
          ]
      )
