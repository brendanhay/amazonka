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
-- Module      : Amazonka.SageMaker.Types.SelectedStep
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SelectedStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A step selected to run in selective execution mode.
--
-- /See:/ 'newSelectedStep' smart constructor.
data SelectedStep = SelectedStep'
  { -- | The name of the pipeline step.
    stepName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectedStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepName', 'selectedStep_stepName' - The name of the pipeline step.
newSelectedStep ::
  -- | 'stepName'
  Prelude.Text ->
  SelectedStep
newSelectedStep pStepName_ =
  SelectedStep' {stepName = pStepName_}

-- | The name of the pipeline step.
selectedStep_stepName :: Lens.Lens' SelectedStep Prelude.Text
selectedStep_stepName = Lens.lens (\SelectedStep' {stepName} -> stepName) (\s@SelectedStep' {} a -> s {stepName = a} :: SelectedStep)

instance Data.FromJSON SelectedStep where
  parseJSON =
    Data.withObject
      "SelectedStep"
      ( \x ->
          SelectedStep' Prelude.<$> (x Data..: "StepName")
      )

instance Prelude.Hashable SelectedStep where
  hashWithSalt _salt SelectedStep' {..} =
    _salt `Prelude.hashWithSalt` stepName

instance Prelude.NFData SelectedStep where
  rnf SelectedStep' {..} = Prelude.rnf stepName

instance Data.ToJSON SelectedStep where
  toJSON SelectedStep' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StepName" Data..= stepName)]
      )
