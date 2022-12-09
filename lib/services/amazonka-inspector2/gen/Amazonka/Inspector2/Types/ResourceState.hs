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
-- Module      : Amazonka.Inspector2.Types.ResourceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.State
import qualified Amazonka.Prelude as Prelude

-- | Details the state of Amazon Inspector for each resource type Amazon
-- Inspector scans.
--
-- /See:/ 'newResourceState' smart constructor.
data ResourceState = ResourceState'
  { lambda :: Prelude.Maybe State,
    -- | An object detailing the state of Amazon Inspector scanning for Amazon
    -- EC2 resources.
    ec2 :: State,
    -- | An object detailing the state of Amazon Inspector scanning for Amazon
    -- ECR resources.
    ecr :: State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambda', 'resourceState_lambda' - Undocumented member.
--
-- 'ec2', 'resourceState_ec2' - An object detailing the state of Amazon Inspector scanning for Amazon
-- EC2 resources.
--
-- 'ecr', 'resourceState_ecr' - An object detailing the state of Amazon Inspector scanning for Amazon
-- ECR resources.
newResourceState ::
  -- | 'ec2'
  State ->
  -- | 'ecr'
  State ->
  ResourceState
newResourceState pEc2_ pEcr_ =
  ResourceState'
    { lambda = Prelude.Nothing,
      ec2 = pEc2_,
      ecr = pEcr_
    }

-- | Undocumented member.
resourceState_lambda :: Lens.Lens' ResourceState (Prelude.Maybe State)
resourceState_lambda = Lens.lens (\ResourceState' {lambda} -> lambda) (\s@ResourceState' {} a -> s {lambda = a} :: ResourceState)

-- | An object detailing the state of Amazon Inspector scanning for Amazon
-- EC2 resources.
resourceState_ec2 :: Lens.Lens' ResourceState State
resourceState_ec2 = Lens.lens (\ResourceState' {ec2} -> ec2) (\s@ResourceState' {} a -> s {ec2 = a} :: ResourceState)

-- | An object detailing the state of Amazon Inspector scanning for Amazon
-- ECR resources.
resourceState_ecr :: Lens.Lens' ResourceState State
resourceState_ecr = Lens.lens (\ResourceState' {ecr} -> ecr) (\s@ResourceState' {} a -> s {ecr = a} :: ResourceState)

instance Data.FromJSON ResourceState where
  parseJSON =
    Data.withObject
      "ResourceState"
      ( \x ->
          ResourceState'
            Prelude.<$> (x Data..:? "lambda")
            Prelude.<*> (x Data..: "ec2")
            Prelude.<*> (x Data..: "ecr")
      )

instance Prelude.Hashable ResourceState where
  hashWithSalt _salt ResourceState' {..} =
    _salt `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` ec2
      `Prelude.hashWithSalt` ecr

instance Prelude.NFData ResourceState where
  rnf ResourceState' {..} =
    Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf ec2
      `Prelude.seq` Prelude.rnf ecr
