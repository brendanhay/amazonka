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
-- Module      : Amazonka.FraudDetector.Types.Outcome
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.Outcome where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The outcome.
--
-- /See:/ 'newOutcome' smart constructor.
data Outcome = Outcome'
  { -- | The timestamp when the outcome was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The outcome ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the outcome was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The outcome name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The outcome description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Outcome' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'outcome_lastUpdatedTime' - The timestamp when the outcome was last updated.
--
-- 'arn', 'outcome_arn' - The outcome ARN.
--
-- 'createdTime', 'outcome_createdTime' - The timestamp when the outcome was created.
--
-- 'name', 'outcome_name' - The outcome name.
--
-- 'description', 'outcome_description' - The outcome description.
newOutcome ::
  Outcome
newOutcome =
  Outcome'
    { lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The timestamp when the outcome was last updated.
outcome_lastUpdatedTime :: Lens.Lens' Outcome (Prelude.Maybe Prelude.Text)
outcome_lastUpdatedTime = Lens.lens (\Outcome' {lastUpdatedTime} -> lastUpdatedTime) (\s@Outcome' {} a -> s {lastUpdatedTime = a} :: Outcome)

-- | The outcome ARN.
outcome_arn :: Lens.Lens' Outcome (Prelude.Maybe Prelude.Text)
outcome_arn = Lens.lens (\Outcome' {arn} -> arn) (\s@Outcome' {} a -> s {arn = a} :: Outcome)

-- | The timestamp when the outcome was created.
outcome_createdTime :: Lens.Lens' Outcome (Prelude.Maybe Prelude.Text)
outcome_createdTime = Lens.lens (\Outcome' {createdTime} -> createdTime) (\s@Outcome' {} a -> s {createdTime = a} :: Outcome)

-- | The outcome name.
outcome_name :: Lens.Lens' Outcome (Prelude.Maybe Prelude.Text)
outcome_name = Lens.lens (\Outcome' {name} -> name) (\s@Outcome' {} a -> s {name = a} :: Outcome)

-- | The outcome description.
outcome_description :: Lens.Lens' Outcome (Prelude.Maybe Prelude.Text)
outcome_description = Lens.lens (\Outcome' {description} -> description) (\s@Outcome' {} a -> s {description = a} :: Outcome)

instance Core.FromJSON Outcome where
  parseJSON =
    Core.withObject
      "Outcome"
      ( \x ->
          Outcome'
            Prelude.<$> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable Outcome where
  hashWithSalt _salt Outcome' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description

instance Prelude.NFData Outcome where
  rnf Outcome' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
