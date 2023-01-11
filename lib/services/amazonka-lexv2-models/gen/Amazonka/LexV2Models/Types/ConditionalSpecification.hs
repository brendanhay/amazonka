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
-- Module      : Amazonka.LexV2Models.Types.ConditionalSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConditionalSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConditionalBranch
import Amazonka.LexV2Models.Types.DefaultConditionalBranch
import qualified Amazonka.Prelude as Prelude

-- | Provides a list of conditional branches. Branches are evaluated in the
-- order that they are entered in the list. The first branch with a
-- condition that evaluates to true is executed. The last branch in the
-- list is the default branch. The default branch should not have any
-- condition expression. The default branch is executed if no other branch
-- has a matching condition.
--
-- /See:/ 'newConditionalSpecification' smart constructor.
data ConditionalSpecification = ConditionalSpecification'
  { -- | Determines whether a conditional branch is active. When @active@ is
    -- false, the conditions are not evaluated.
    active :: Prelude.Bool,
    -- | A list of conditional branches. A conditional branch is made up of a
    -- condition, a response and a next step. The response and next step are
    -- executed when the condition is true.
    conditionalBranches :: Prelude.NonEmpty ConditionalBranch,
    -- | The conditional branch that should be followed when the conditions for
    -- other branches are not satisfied. A conditional branch is made up of a
    -- condition, a response and a next step.
    defaultBranch :: DefaultConditionalBranch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'conditionalSpecification_active' - Determines whether a conditional branch is active. When @active@ is
-- false, the conditions are not evaluated.
--
-- 'conditionalBranches', 'conditionalSpecification_conditionalBranches' - A list of conditional branches. A conditional branch is made up of a
-- condition, a response and a next step. The response and next step are
-- executed when the condition is true.
--
-- 'defaultBranch', 'conditionalSpecification_defaultBranch' - The conditional branch that should be followed when the conditions for
-- other branches are not satisfied. A conditional branch is made up of a
-- condition, a response and a next step.
newConditionalSpecification ::
  -- | 'active'
  Prelude.Bool ->
  -- | 'conditionalBranches'
  Prelude.NonEmpty ConditionalBranch ->
  -- | 'defaultBranch'
  DefaultConditionalBranch ->
  ConditionalSpecification
newConditionalSpecification
  pActive_
  pConditionalBranches_
  pDefaultBranch_ =
    ConditionalSpecification'
      { active = pActive_,
        conditionalBranches =
          Lens.coerced Lens.# pConditionalBranches_,
        defaultBranch = pDefaultBranch_
      }

-- | Determines whether a conditional branch is active. When @active@ is
-- false, the conditions are not evaluated.
conditionalSpecification_active :: Lens.Lens' ConditionalSpecification Prelude.Bool
conditionalSpecification_active = Lens.lens (\ConditionalSpecification' {active} -> active) (\s@ConditionalSpecification' {} a -> s {active = a} :: ConditionalSpecification)

-- | A list of conditional branches. A conditional branch is made up of a
-- condition, a response and a next step. The response and next step are
-- executed when the condition is true.
conditionalSpecification_conditionalBranches :: Lens.Lens' ConditionalSpecification (Prelude.NonEmpty ConditionalBranch)
conditionalSpecification_conditionalBranches = Lens.lens (\ConditionalSpecification' {conditionalBranches} -> conditionalBranches) (\s@ConditionalSpecification' {} a -> s {conditionalBranches = a} :: ConditionalSpecification) Prelude.. Lens.coerced

-- | The conditional branch that should be followed when the conditions for
-- other branches are not satisfied. A conditional branch is made up of a
-- condition, a response and a next step.
conditionalSpecification_defaultBranch :: Lens.Lens' ConditionalSpecification DefaultConditionalBranch
conditionalSpecification_defaultBranch = Lens.lens (\ConditionalSpecification' {defaultBranch} -> defaultBranch) (\s@ConditionalSpecification' {} a -> s {defaultBranch = a} :: ConditionalSpecification)

instance Data.FromJSON ConditionalSpecification where
  parseJSON =
    Data.withObject
      "ConditionalSpecification"
      ( \x ->
          ConditionalSpecification'
            Prelude.<$> (x Data..: "active")
            Prelude.<*> (x Data..: "conditionalBranches")
            Prelude.<*> (x Data..: "defaultBranch")
      )

instance Prelude.Hashable ConditionalSpecification where
  hashWithSalt _salt ConditionalSpecification' {..} =
    _salt `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` conditionalBranches
      `Prelude.hashWithSalt` defaultBranch

instance Prelude.NFData ConditionalSpecification where
  rnf ConditionalSpecification' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf conditionalBranches
      `Prelude.seq` Prelude.rnf defaultBranch

instance Data.ToJSON ConditionalSpecification where
  toJSON ConditionalSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("active" Data..= active),
            Prelude.Just
              ("conditionalBranches" Data..= conditionalBranches),
            Prelude.Just
              ("defaultBranch" Data..= defaultBranch)
          ]
      )
