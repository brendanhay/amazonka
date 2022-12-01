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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Plan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.Stage

-- | The stages that an escalation plan or engagement plan engages contacts
-- and contact methods in.
--
-- /See:/ 'newPlan' smart constructor.
data Plan = Plan'
  { -- | A list of stages that the escalation plan or engagement plan uses to
    -- engage contacts and contact methods.
    stages :: [Stage]
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
-- 'stages', 'plan_stages' - A list of stages that the escalation plan or engagement plan uses to
-- engage contacts and contact methods.
newPlan ::
  Plan
newPlan = Plan' {stages = Prelude.mempty}

-- | A list of stages that the escalation plan or engagement plan uses to
-- engage contacts and contact methods.
plan_stages :: Lens.Lens' Plan [Stage]
plan_stages = Lens.lens (\Plan' {stages} -> stages) (\s@Plan' {} a -> s {stages = a} :: Plan) Prelude.. Lens.coerced

instance Core.FromJSON Plan where
  parseJSON =
    Core.withObject
      "Plan"
      ( \x ->
          Plan'
            Prelude.<$> (x Core..:? "Stages" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Plan where
  hashWithSalt _salt Plan' {..} =
    _salt `Prelude.hashWithSalt` stages

instance Prelude.NFData Plan where
  rnf Plan' {..} = Prelude.rnf stages

instance Core.ToJSON Plan where
  toJSON Plan' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Stages" Core..= stages)]
      )
