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
-- Module      : Amazonka.SSMContacts.Types.Plan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Plan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.Stage

-- | Information about the stages and on-call rotation teams associated with
-- an escalation plan or engagement plan.
--
-- /See:/ 'newPlan' smart constructor.
data Plan = Plan'
  { -- | The Amazon Resource Names (ARNs) of the on-call rotations associated
    -- with the plan.
    rotationIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of stages that the escalation plan or engagement plan uses to
    -- engage contacts and contact methods.
    stages :: Prelude.Maybe [Stage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Plan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationIds', 'plan_rotationIds' - The Amazon Resource Names (ARNs) of the on-call rotations associated
-- with the plan.
--
-- 'stages', 'plan_stages' - A list of stages that the escalation plan or engagement plan uses to
-- engage contacts and contact methods.
newPlan ::
  Plan
newPlan =
  Plan'
    { rotationIds = Prelude.Nothing,
      stages = Prelude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of the on-call rotations associated
-- with the plan.
plan_rotationIds :: Lens.Lens' Plan (Prelude.Maybe [Prelude.Text])
plan_rotationIds = Lens.lens (\Plan' {rotationIds} -> rotationIds) (\s@Plan' {} a -> s {rotationIds = a} :: Plan) Prelude.. Lens.mapping Lens.coerced

-- | A list of stages that the escalation plan or engagement plan uses to
-- engage contacts and contact methods.
plan_stages :: Lens.Lens' Plan (Prelude.Maybe [Stage])
plan_stages = Lens.lens (\Plan' {stages} -> stages) (\s@Plan' {} a -> s {stages = a} :: Plan) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Plan where
  parseJSON =
    Data.withObject
      "Plan"
      ( \x ->
          Plan'
            Prelude.<$> (x Data..:? "RotationIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Stages" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Plan where
  hashWithSalt _salt Plan' {..} =
    _salt
      `Prelude.hashWithSalt` rotationIds
      `Prelude.hashWithSalt` stages

instance Prelude.NFData Plan where
  rnf Plan' {..} =
    Prelude.rnf rotationIds
      `Prelude.seq` Prelude.rnf stages

instance Data.ToJSON Plan where
  toJSON Plan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RotationIds" Data..=) Prelude.<$> rotationIds,
            ("Stages" Data..=) Prelude.<$> stages
          ]
      )
