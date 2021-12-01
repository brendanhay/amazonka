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
-- Module      : Amazonka.FIS.Types.ExperimentStopCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentStopCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the stop condition for an experiment.
--
-- /See:/ 'newExperimentStopCondition' smart constructor.
data ExperimentStopCondition = ExperimentStopCondition'
  { -- | The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
    value :: Prelude.Maybe Prelude.Text,
    -- | The source for the stop condition.
    source :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentStopCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'experimentStopCondition_value' - The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
--
-- 'source', 'experimentStopCondition_source' - The source for the stop condition.
newExperimentStopCondition ::
  ExperimentStopCondition
newExperimentStopCondition =
  ExperimentStopCondition'
    { value = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
experimentStopCondition_value :: Lens.Lens' ExperimentStopCondition (Prelude.Maybe Prelude.Text)
experimentStopCondition_value = Lens.lens (\ExperimentStopCondition' {value} -> value) (\s@ExperimentStopCondition' {} a -> s {value = a} :: ExperimentStopCondition)

-- | The source for the stop condition.
experimentStopCondition_source :: Lens.Lens' ExperimentStopCondition (Prelude.Maybe Prelude.Text)
experimentStopCondition_source = Lens.lens (\ExperimentStopCondition' {source} -> source) (\s@ExperimentStopCondition' {} a -> s {source = a} :: ExperimentStopCondition)

instance Core.FromJSON ExperimentStopCondition where
  parseJSON =
    Core.withObject
      "ExperimentStopCondition"
      ( \x ->
          ExperimentStopCondition'
            Prelude.<$> (x Core..:? "value")
            Prelude.<*> (x Core..:? "source")
      )

instance Prelude.Hashable ExperimentStopCondition where
  hashWithSalt salt' ExperimentStopCondition' {..} =
    salt' `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` value

instance Prelude.NFData ExperimentStopCondition where
  rnf ExperimentStopCondition' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf source
