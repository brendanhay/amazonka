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
-- Module      : Amazonka.VPCLattice.Types.WeightedTargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.WeightedTargetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the weight of a target group.
--
-- /See:/ 'newWeightedTargetGroup' smart constructor.
data WeightedTargetGroup = WeightedTargetGroup'
  { -- | Only required if you specify multiple target groups for a forward
    -- action. The \"weight\" determines how requests are distributed to the
    -- target group. For example, if you specify two target groups, each with a
    -- weight of 10, each target group receives half the requests. If you
    -- specify two target groups, one with a weight of 10 and the other with a
    -- weight of 20, the target group with a weight of 20 receives twice as
    -- many requests as the other target group. If there\'s only one target
    -- group specified, then the default value is 100.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | The ID or Amazon Resource Name (ARN) of the target group.
    targetGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WeightedTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weight', 'weightedTargetGroup_weight' - Only required if you specify multiple target groups for a forward
-- action. The \"weight\" determines how requests are distributed to the
-- target group. For example, if you specify two target groups, each with a
-- weight of 10, each target group receives half the requests. If you
-- specify two target groups, one with a weight of 10 and the other with a
-- weight of 20, the target group with a weight of 20 receives twice as
-- many requests as the other target group. If there\'s only one target
-- group specified, then the default value is 100.
--
-- 'targetGroupIdentifier', 'weightedTargetGroup_targetGroupIdentifier' - The ID or Amazon Resource Name (ARN) of the target group.
newWeightedTargetGroup ::
  -- | 'targetGroupIdentifier'
  Prelude.Text ->
  WeightedTargetGroup
newWeightedTargetGroup pTargetGroupIdentifier_ =
  WeightedTargetGroup'
    { weight = Prelude.Nothing,
      targetGroupIdentifier = pTargetGroupIdentifier_
    }

-- | Only required if you specify multiple target groups for a forward
-- action. The \"weight\" determines how requests are distributed to the
-- target group. For example, if you specify two target groups, each with a
-- weight of 10, each target group receives half the requests. If you
-- specify two target groups, one with a weight of 10 and the other with a
-- weight of 20, the target group with a weight of 20 receives twice as
-- many requests as the other target group. If there\'s only one target
-- group specified, then the default value is 100.
weightedTargetGroup_weight :: Lens.Lens' WeightedTargetGroup (Prelude.Maybe Prelude.Natural)
weightedTargetGroup_weight = Lens.lens (\WeightedTargetGroup' {weight} -> weight) (\s@WeightedTargetGroup' {} a -> s {weight = a} :: WeightedTargetGroup)

-- | The ID or Amazon Resource Name (ARN) of the target group.
weightedTargetGroup_targetGroupIdentifier :: Lens.Lens' WeightedTargetGroup Prelude.Text
weightedTargetGroup_targetGroupIdentifier = Lens.lens (\WeightedTargetGroup' {targetGroupIdentifier} -> targetGroupIdentifier) (\s@WeightedTargetGroup' {} a -> s {targetGroupIdentifier = a} :: WeightedTargetGroup)

instance Data.FromJSON WeightedTargetGroup where
  parseJSON =
    Data.withObject
      "WeightedTargetGroup"
      ( \x ->
          WeightedTargetGroup'
            Prelude.<$> (x Data..:? "weight")
            Prelude.<*> (x Data..: "targetGroupIdentifier")
      )

instance Prelude.Hashable WeightedTargetGroup where
  hashWithSalt _salt WeightedTargetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` targetGroupIdentifier

instance Prelude.NFData WeightedTargetGroup where
  rnf WeightedTargetGroup' {..} =
    Prelude.rnf weight
      `Prelude.seq` Prelude.rnf targetGroupIdentifier

instance Data.ToJSON WeightedTargetGroup where
  toJSON WeightedTargetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("weight" Data..=) Prelude.<$> weight,
            Prelude.Just
              ( "targetGroupIdentifier"
                  Data..= targetGroupIdentifier
              )
          ]
      )
