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
-- Module      : Amazonka.SageMaker.Types.Parent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Parent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The trial that a trial component is associated with and the experiment
-- the trial is part of. A component might not be associated with a trial.
-- A component can be associated with multiple trials.
--
-- /See:/ 'newParent' smart constructor.
data Parent = Parent'
  { -- | The name of the experiment.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentName', 'parent_experimentName' - The name of the experiment.
--
-- 'trialName', 'parent_trialName' - The name of the trial.
newParent ::
  Parent
newParent =
  Parent'
    { experimentName = Prelude.Nothing,
      trialName = Prelude.Nothing
    }

-- | The name of the experiment.
parent_experimentName :: Lens.Lens' Parent (Prelude.Maybe Prelude.Text)
parent_experimentName = Lens.lens (\Parent' {experimentName} -> experimentName) (\s@Parent' {} a -> s {experimentName = a} :: Parent)

-- | The name of the trial.
parent_trialName :: Lens.Lens' Parent (Prelude.Maybe Prelude.Text)
parent_trialName = Lens.lens (\Parent' {trialName} -> trialName) (\s@Parent' {} a -> s {trialName = a} :: Parent)

instance Data.FromJSON Parent where
  parseJSON =
    Data.withObject
      "Parent"
      ( \x ->
          Parent'
            Prelude.<$> (x Data..:? "ExperimentName")
            Prelude.<*> (x Data..:? "TrialName")
      )

instance Prelude.Hashable Parent where
  hashWithSalt _salt Parent' {..} =
    _salt `Prelude.hashWithSalt` experimentName
      `Prelude.hashWithSalt` trialName

instance Prelude.NFData Parent where
  rnf Parent' {..} =
    Prelude.rnf experimentName
      `Prelude.seq` Prelude.rnf trialName
