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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of an experiment template.
--
-- /See:/ 'newExperimentTemplateSummary' smart constructor.
data ExperimentTemplateSummary = ExperimentTemplateSummary'
  { -- | The tags for the experiment template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the experiment template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the experiment template.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time that the experiment template was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The time that the experiment template was last updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'experimentTemplateSummary_tags' - The tags for the experiment template.
--
-- 'description', 'experimentTemplateSummary_description' - The description of the experiment template.
--
-- 'id', 'experimentTemplateSummary_id' - The ID of the experiment template.
--
-- 'creationTime', 'experimentTemplateSummary_creationTime' - The time that the experiment template was created.
--
-- 'lastUpdateTime', 'experimentTemplateSummary_lastUpdateTime' - The time that the experiment template was last updated.
newExperimentTemplateSummary ::
  ExperimentTemplateSummary
newExperimentTemplateSummary =
  ExperimentTemplateSummary'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | The tags for the experiment template.
experimentTemplateSummary_tags :: Lens.Lens' ExperimentTemplateSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplateSummary_tags = Lens.lens (\ExperimentTemplateSummary' {tags} -> tags) (\s@ExperimentTemplateSummary' {} a -> s {tags = a} :: ExperimentTemplateSummary) Prelude.. Lens.mapping Lens.coerced

-- | The description of the experiment template.
experimentTemplateSummary_description :: Lens.Lens' ExperimentTemplateSummary (Prelude.Maybe Prelude.Text)
experimentTemplateSummary_description = Lens.lens (\ExperimentTemplateSummary' {description} -> description) (\s@ExperimentTemplateSummary' {} a -> s {description = a} :: ExperimentTemplateSummary)

-- | The ID of the experiment template.
experimentTemplateSummary_id :: Lens.Lens' ExperimentTemplateSummary (Prelude.Maybe Prelude.Text)
experimentTemplateSummary_id = Lens.lens (\ExperimentTemplateSummary' {id} -> id) (\s@ExperimentTemplateSummary' {} a -> s {id = a} :: ExperimentTemplateSummary)

-- | The time that the experiment template was created.
experimentTemplateSummary_creationTime :: Lens.Lens' ExperimentTemplateSummary (Prelude.Maybe Prelude.UTCTime)
experimentTemplateSummary_creationTime = Lens.lens (\ExperimentTemplateSummary' {creationTime} -> creationTime) (\s@ExperimentTemplateSummary' {} a -> s {creationTime = a} :: ExperimentTemplateSummary) Prelude.. Lens.mapping Core._Time

-- | The time that the experiment template was last updated.
experimentTemplateSummary_lastUpdateTime :: Lens.Lens' ExperimentTemplateSummary (Prelude.Maybe Prelude.UTCTime)
experimentTemplateSummary_lastUpdateTime = Lens.lens (\ExperimentTemplateSummary' {lastUpdateTime} -> lastUpdateTime) (\s@ExperimentTemplateSummary' {} a -> s {lastUpdateTime = a} :: ExperimentTemplateSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ExperimentTemplateSummary where
  parseJSON =
    Core.withObject
      "ExperimentTemplateSummary"
      ( \x ->
          ExperimentTemplateSummary'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "lastUpdateTime")
      )

instance Prelude.Hashable ExperimentTemplateSummary where
  hashWithSalt _salt ExperimentTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime

instance Prelude.NFData ExperimentTemplateSummary where
  rnf ExperimentTemplateSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
