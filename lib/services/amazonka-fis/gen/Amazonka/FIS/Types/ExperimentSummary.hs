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
-- Module      : Amazonka.FIS.Types.ExperimentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentState
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of an experiment.
--
-- /See:/ 'newExperimentSummary' smart constructor.
data ExperimentSummary = ExperimentSummary'
  { -- | The time that the experiment was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the experiment template.
    experimentTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the experiment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The state of the experiment.
    state :: Prelude.Maybe ExperimentState,
    -- | The tags for the experiment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'experimentSummary_creationTime' - The time that the experiment was created.
--
-- 'experimentTemplateId', 'experimentSummary_experimentTemplateId' - The ID of the experiment template.
--
-- 'id', 'experimentSummary_id' - The ID of the experiment.
--
-- 'state', 'experimentSummary_state' - The state of the experiment.
--
-- 'tags', 'experimentSummary_tags' - The tags for the experiment.
newExperimentSummary ::
  ExperimentSummary
newExperimentSummary =
  ExperimentSummary'
    { creationTime = Prelude.Nothing,
      experimentTemplateId = Prelude.Nothing,
      id = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time that the experiment was created.
experimentSummary_creationTime :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.UTCTime)
experimentSummary_creationTime = Lens.lens (\ExperimentSummary' {creationTime} -> creationTime) (\s@ExperimentSummary' {} a -> s {creationTime = a} :: ExperimentSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the experiment template.
experimentSummary_experimentTemplateId :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.Text)
experimentSummary_experimentTemplateId = Lens.lens (\ExperimentSummary' {experimentTemplateId} -> experimentTemplateId) (\s@ExperimentSummary' {} a -> s {experimentTemplateId = a} :: ExperimentSummary)

-- | The ID of the experiment.
experimentSummary_id :: Lens.Lens' ExperimentSummary (Prelude.Maybe Prelude.Text)
experimentSummary_id = Lens.lens (\ExperimentSummary' {id} -> id) (\s@ExperimentSummary' {} a -> s {id = a} :: ExperimentSummary)

-- | The state of the experiment.
experimentSummary_state :: Lens.Lens' ExperimentSummary (Prelude.Maybe ExperimentState)
experimentSummary_state = Lens.lens (\ExperimentSummary' {state} -> state) (\s@ExperimentSummary' {} a -> s {state = a} :: ExperimentSummary)

-- | The tags for the experiment.
experimentSummary_tags :: Lens.Lens' ExperimentSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentSummary_tags = Lens.lens (\ExperimentSummary' {tags} -> tags) (\s@ExperimentSummary' {} a -> s {tags = a} :: ExperimentSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExperimentSummary where
  parseJSON =
    Data.withObject
      "ExperimentSummary"
      ( \x ->
          ExperimentSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "experimentTemplateId")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExperimentSummary where
  hashWithSalt _salt ExperimentSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` experimentTemplateId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ExperimentSummary where
  rnf ExperimentSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf experimentTemplateId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
