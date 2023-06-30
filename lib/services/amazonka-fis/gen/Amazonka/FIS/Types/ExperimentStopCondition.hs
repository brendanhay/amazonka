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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentStopCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the stop condition for an experiment.
--
-- /See:/ 'newExperimentStopCondition' smart constructor.
data ExperimentStopCondition = ExperimentStopCondition'
  { -- | The source for the stop condition.
    source :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
    value :: Prelude.Maybe Prelude.Text
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
-- 'source', 'experimentStopCondition_source' - The source for the stop condition.
--
-- 'value', 'experimentStopCondition_value' - The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
newExperimentStopCondition ::
  ExperimentStopCondition
newExperimentStopCondition =
  ExperimentStopCondition'
    { source = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The source for the stop condition.
experimentStopCondition_source :: Lens.Lens' ExperimentStopCondition (Prelude.Maybe Prelude.Text)
experimentStopCondition_source = Lens.lens (\ExperimentStopCondition' {source} -> source) (\s@ExperimentStopCondition' {} a -> s {source = a} :: ExperimentStopCondition)

-- | The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
experimentStopCondition_value :: Lens.Lens' ExperimentStopCondition (Prelude.Maybe Prelude.Text)
experimentStopCondition_value = Lens.lens (\ExperimentStopCondition' {value} -> value) (\s@ExperimentStopCondition' {} a -> s {value = a} :: ExperimentStopCondition)

instance Data.FromJSON ExperimentStopCondition where
  parseJSON =
    Data.withObject
      "ExperimentStopCondition"
      ( \x ->
          ExperimentStopCondition'
            Prelude.<$> (x Data..:? "source")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ExperimentStopCondition where
  hashWithSalt _salt ExperimentStopCondition' {..} =
    _salt
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` value

instance Prelude.NFData ExperimentStopCondition where
  rnf ExperimentStopCondition' {..} =
    Prelude.rnf source `Prelude.seq` Prelude.rnf value
