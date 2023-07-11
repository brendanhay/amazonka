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
-- Module      : Amazonka.Rekognition.Types.HumanLoopActivationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.HumanLoopActivationOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Shows the results of the human in the loop evaluation. If there is no
-- HumanLoopArn, the input did not trigger human review.
--
-- /See:/ 'newHumanLoopActivationOutput' smart constructor.
data HumanLoopActivationOutput = HumanLoopActivationOutput'
  { -- | Shows the result of condition evaluations, including those conditions
    -- which activated a human review.
    humanLoopActivationConditionsEvaluationResults :: Prelude.Maybe Prelude.Text,
    -- | Shows if and why human review was needed.
    humanLoopActivationReasons :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the HumanLoop created.
    humanLoopArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopActivationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopActivationConditionsEvaluationResults', 'humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults' - Shows the result of condition evaluations, including those conditions
-- which activated a human review.
--
-- 'humanLoopActivationReasons', 'humanLoopActivationOutput_humanLoopActivationReasons' - Shows if and why human review was needed.
--
-- 'humanLoopArn', 'humanLoopActivationOutput_humanLoopArn' - The Amazon Resource Name (ARN) of the HumanLoop created.
newHumanLoopActivationOutput ::
  HumanLoopActivationOutput
newHumanLoopActivationOutput =
  HumanLoopActivationOutput'
    { humanLoopActivationConditionsEvaluationResults =
        Prelude.Nothing,
      humanLoopActivationReasons = Prelude.Nothing,
      humanLoopArn = Prelude.Nothing
    }

-- | Shows the result of condition evaluations, including those conditions
-- which activated a human review.
humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults :: Lens.Lens' HumanLoopActivationOutput (Prelude.Maybe Prelude.Text)
humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults = Lens.lens (\HumanLoopActivationOutput' {humanLoopActivationConditionsEvaluationResults} -> humanLoopActivationConditionsEvaluationResults) (\s@HumanLoopActivationOutput' {} a -> s {humanLoopActivationConditionsEvaluationResults = a} :: HumanLoopActivationOutput)

-- | Shows if and why human review was needed.
humanLoopActivationOutput_humanLoopActivationReasons :: Lens.Lens' HumanLoopActivationOutput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
humanLoopActivationOutput_humanLoopActivationReasons = Lens.lens (\HumanLoopActivationOutput' {humanLoopActivationReasons} -> humanLoopActivationReasons) (\s@HumanLoopActivationOutput' {} a -> s {humanLoopActivationReasons = a} :: HumanLoopActivationOutput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the HumanLoop created.
humanLoopActivationOutput_humanLoopArn :: Lens.Lens' HumanLoopActivationOutput (Prelude.Maybe Prelude.Text)
humanLoopActivationOutput_humanLoopArn = Lens.lens (\HumanLoopActivationOutput' {humanLoopArn} -> humanLoopArn) (\s@HumanLoopActivationOutput' {} a -> s {humanLoopArn = a} :: HumanLoopActivationOutput)

instance Data.FromJSON HumanLoopActivationOutput where
  parseJSON =
    Data.withObject
      "HumanLoopActivationOutput"
      ( \x ->
          HumanLoopActivationOutput'
            Prelude.<$> ( x
                            Data..:? "HumanLoopActivationConditionsEvaluationResults"
                        )
            Prelude.<*> (x Data..:? "HumanLoopActivationReasons")
            Prelude.<*> (x Data..:? "HumanLoopArn")
      )

instance Prelude.Hashable HumanLoopActivationOutput where
  hashWithSalt _salt HumanLoopActivationOutput' {..} =
    _salt
      `Prelude.hashWithSalt` humanLoopActivationConditionsEvaluationResults
      `Prelude.hashWithSalt` humanLoopActivationReasons
      `Prelude.hashWithSalt` humanLoopArn

instance Prelude.NFData HumanLoopActivationOutput where
  rnf HumanLoopActivationOutput' {..} =
    Prelude.rnf
      humanLoopActivationConditionsEvaluationResults
      `Prelude.seq` Prelude.rnf humanLoopActivationReasons
      `Prelude.seq` Prelude.rnf humanLoopArn
