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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateStopCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateStopCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a stop condition for an experiment template.
--
-- /See:/ 'newExperimentTemplateStopCondition' smart constructor.
data ExperimentTemplateStopCondition = ExperimentTemplateStopCondition'
  { -- | The source for the stop condition.
    source :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateStopCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'experimentTemplateStopCondition_source' - The source for the stop condition.
--
-- 'value', 'experimentTemplateStopCondition_value' - The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
newExperimentTemplateStopCondition ::
  ExperimentTemplateStopCondition
newExperimentTemplateStopCondition =
  ExperimentTemplateStopCondition'
    { source =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The source for the stop condition.
experimentTemplateStopCondition_source :: Lens.Lens' ExperimentTemplateStopCondition (Prelude.Maybe Prelude.Text)
experimentTemplateStopCondition_source = Lens.lens (\ExperimentTemplateStopCondition' {source} -> source) (\s@ExperimentTemplateStopCondition' {} a -> s {source = a} :: ExperimentTemplateStopCondition)

-- | The Amazon Resource Name (ARN) of the CloudWatch alarm, if applicable.
experimentTemplateStopCondition_value :: Lens.Lens' ExperimentTemplateStopCondition (Prelude.Maybe Prelude.Text)
experimentTemplateStopCondition_value = Lens.lens (\ExperimentTemplateStopCondition' {value} -> value) (\s@ExperimentTemplateStopCondition' {} a -> s {value = a} :: ExperimentTemplateStopCondition)

instance
  Data.FromJSON
    ExperimentTemplateStopCondition
  where
  parseJSON =
    Data.withObject
      "ExperimentTemplateStopCondition"
      ( \x ->
          ExperimentTemplateStopCondition'
            Prelude.<$> (x Data..:? "source")
            Prelude.<*> (x Data..:? "value")
      )

instance
  Prelude.Hashable
    ExperimentTemplateStopCondition
  where
  hashWithSalt
    _salt
    ExperimentTemplateStopCondition' {..} =
      _salt
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    ExperimentTemplateStopCondition
  where
  rnf ExperimentTemplateStopCondition' {..} =
    Prelude.rnf source `Prelude.seq` Prelude.rnf value
