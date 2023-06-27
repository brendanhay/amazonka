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
-- Module      : Amazonka.VPCLattice.Types.ForwardAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ForwardAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.WeightedTargetGroup

-- | Describes a forward action. You can use forward actions to route
-- requests to one or more target groups.
--
-- /See:/ 'newForwardAction' smart constructor.
data ForwardAction = ForwardAction'
  { -- | The target groups. Traffic matching the rule is forwarded to the
    -- specified target groups. With forward actions, you can assign a weight
    -- that controls the prioritization and selection of each target group.
    -- This means that requests are distributed to individual target groups
    -- based on their weights. For example, if two target groups have the same
    -- weight, each target group receives half of the traffic.
    --
    -- The default value is 1. This means that if only one target group is
    -- provided, there is no need to set the weight; 100% of traffic will go to
    -- that target group.
    targetGroups :: Prelude.NonEmpty WeightedTargetGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForwardAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'forwardAction_targetGroups' - The target groups. Traffic matching the rule is forwarded to the
-- specified target groups. With forward actions, you can assign a weight
-- that controls the prioritization and selection of each target group.
-- This means that requests are distributed to individual target groups
-- based on their weights. For example, if two target groups have the same
-- weight, each target group receives half of the traffic.
--
-- The default value is 1. This means that if only one target group is
-- provided, there is no need to set the weight; 100% of traffic will go to
-- that target group.
newForwardAction ::
  -- | 'targetGroups'
  Prelude.NonEmpty WeightedTargetGroup ->
  ForwardAction
newForwardAction pTargetGroups_ =
  ForwardAction'
    { targetGroups =
        Lens.coerced Lens.# pTargetGroups_
    }

-- | The target groups. Traffic matching the rule is forwarded to the
-- specified target groups. With forward actions, you can assign a weight
-- that controls the prioritization and selection of each target group.
-- This means that requests are distributed to individual target groups
-- based on their weights. For example, if two target groups have the same
-- weight, each target group receives half of the traffic.
--
-- The default value is 1. This means that if only one target group is
-- provided, there is no need to set the weight; 100% of traffic will go to
-- that target group.
forwardAction_targetGroups :: Lens.Lens' ForwardAction (Prelude.NonEmpty WeightedTargetGroup)
forwardAction_targetGroups = Lens.lens (\ForwardAction' {targetGroups} -> targetGroups) (\s@ForwardAction' {} a -> s {targetGroups = a} :: ForwardAction) Prelude.. Lens.coerced

instance Data.FromJSON ForwardAction where
  parseJSON =
    Data.withObject
      "ForwardAction"
      ( \x ->
          ForwardAction'
            Prelude.<$> (x Data..: "targetGroups")
      )

instance Prelude.Hashable ForwardAction where
  hashWithSalt _salt ForwardAction' {..} =
    _salt `Prelude.hashWithSalt` targetGroups

instance Prelude.NFData ForwardAction where
  rnf ForwardAction' {..} = Prelude.rnf targetGroups

instance Data.ToJSON ForwardAction where
  toJSON ForwardAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("targetGroups" Data..= targetGroups)]
      )
