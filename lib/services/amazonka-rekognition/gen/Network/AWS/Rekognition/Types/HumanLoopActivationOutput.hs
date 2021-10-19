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
-- Module      : Network.AWS.Rekognition.Types.HumanLoopActivationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopActivationOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Shows the results of the human in the loop evaluation. If there is no
-- HumanLoopArn, the input did not trigger human review.
--
-- /See:/ 'newHumanLoopActivationOutput' smart constructor.
data HumanLoopActivationOutput = HumanLoopActivationOutput'
  { -- | Shows if and why human review was needed.
    humanLoopActivationReasons :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the HumanLoop created.
    humanLoopArn :: Prelude.Maybe Prelude.Text,
    -- | Shows the result of condition evaluations, including those conditions
    -- which activated a human review.
    humanLoopActivationConditionsEvaluationResults :: Prelude.Maybe Prelude.Text
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
-- 'humanLoopActivationReasons', 'humanLoopActivationOutput_humanLoopActivationReasons' - Shows if and why human review was needed.
--
-- 'humanLoopArn', 'humanLoopActivationOutput_humanLoopArn' - The Amazon Resource Name (ARN) of the HumanLoop created.
--
-- 'humanLoopActivationConditionsEvaluationResults', 'humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults' - Shows the result of condition evaluations, including those conditions
-- which activated a human review.
newHumanLoopActivationOutput ::
  HumanLoopActivationOutput
newHumanLoopActivationOutput =
  HumanLoopActivationOutput'
    { humanLoopActivationReasons =
        Prelude.Nothing,
      humanLoopArn = Prelude.Nothing,
      humanLoopActivationConditionsEvaluationResults =
        Prelude.Nothing
    }

-- | Shows if and why human review was needed.
humanLoopActivationOutput_humanLoopActivationReasons :: Lens.Lens' HumanLoopActivationOutput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
humanLoopActivationOutput_humanLoopActivationReasons = Lens.lens (\HumanLoopActivationOutput' {humanLoopActivationReasons} -> humanLoopActivationReasons) (\s@HumanLoopActivationOutput' {} a -> s {humanLoopActivationReasons = a} :: HumanLoopActivationOutput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the HumanLoop created.
humanLoopActivationOutput_humanLoopArn :: Lens.Lens' HumanLoopActivationOutput (Prelude.Maybe Prelude.Text)
humanLoopActivationOutput_humanLoopArn = Lens.lens (\HumanLoopActivationOutput' {humanLoopArn} -> humanLoopArn) (\s@HumanLoopActivationOutput' {} a -> s {humanLoopArn = a} :: HumanLoopActivationOutput)

-- | Shows the result of condition evaluations, including those conditions
-- which activated a human review.
humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults :: Lens.Lens' HumanLoopActivationOutput (Prelude.Maybe Prelude.Text)
humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults = Lens.lens (\HumanLoopActivationOutput' {humanLoopActivationConditionsEvaluationResults} -> humanLoopActivationConditionsEvaluationResults) (\s@HumanLoopActivationOutput' {} a -> s {humanLoopActivationConditionsEvaluationResults = a} :: HumanLoopActivationOutput)

instance Core.FromJSON HumanLoopActivationOutput where
  parseJSON =
    Core.withObject
      "HumanLoopActivationOutput"
      ( \x ->
          HumanLoopActivationOutput'
            Prelude.<$> (x Core..:? "HumanLoopActivationReasons")
            Prelude.<*> (x Core..:? "HumanLoopArn")
            Prelude.<*> ( x
                            Core..:? "HumanLoopActivationConditionsEvaluationResults"
                        )
      )

instance Prelude.Hashable HumanLoopActivationOutput

instance Prelude.NFData HumanLoopActivationOutput
