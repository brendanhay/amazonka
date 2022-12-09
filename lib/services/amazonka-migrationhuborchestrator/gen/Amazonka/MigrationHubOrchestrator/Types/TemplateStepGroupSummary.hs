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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.TemplateStepGroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.TemplateStepGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of the step group in the template.
--
-- /See:/ 'newTemplateStepGroupSummary' smart constructor.
data TemplateStepGroupSummary = TemplateStepGroupSummary'
  { -- | The ID of the step group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the step group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step group.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The previous step group.
    previous :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateStepGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'templateStepGroupSummary_id' - The ID of the step group.
--
-- 'name', 'templateStepGroupSummary_name' - The name of the step group.
--
-- 'next', 'templateStepGroupSummary_next' - The next step group.
--
-- 'previous', 'templateStepGroupSummary_previous' - The previous step group.
newTemplateStepGroupSummary ::
  TemplateStepGroupSummary
newTemplateStepGroupSummary =
  TemplateStepGroupSummary'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      previous = Prelude.Nothing
    }

-- | The ID of the step group.
templateStepGroupSummary_id :: Lens.Lens' TemplateStepGroupSummary (Prelude.Maybe Prelude.Text)
templateStepGroupSummary_id = Lens.lens (\TemplateStepGroupSummary' {id} -> id) (\s@TemplateStepGroupSummary' {} a -> s {id = a} :: TemplateStepGroupSummary)

-- | The name of the step group.
templateStepGroupSummary_name :: Lens.Lens' TemplateStepGroupSummary (Prelude.Maybe Prelude.Text)
templateStepGroupSummary_name = Lens.lens (\TemplateStepGroupSummary' {name} -> name) (\s@TemplateStepGroupSummary' {} a -> s {name = a} :: TemplateStepGroupSummary)

-- | The next step group.
templateStepGroupSummary_next :: Lens.Lens' TemplateStepGroupSummary (Prelude.Maybe [Prelude.Text])
templateStepGroupSummary_next = Lens.lens (\TemplateStepGroupSummary' {next} -> next) (\s@TemplateStepGroupSummary' {} a -> s {next = a} :: TemplateStepGroupSummary) Prelude.. Lens.mapping Lens.coerced

-- | The previous step group.
templateStepGroupSummary_previous :: Lens.Lens' TemplateStepGroupSummary (Prelude.Maybe [Prelude.Text])
templateStepGroupSummary_previous = Lens.lens (\TemplateStepGroupSummary' {previous} -> previous) (\s@TemplateStepGroupSummary' {} a -> s {previous = a} :: TemplateStepGroupSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TemplateStepGroupSummary where
  parseJSON =
    Data.withObject
      "TemplateStepGroupSummary"
      ( \x ->
          TemplateStepGroupSummary'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "next" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "previous" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TemplateStepGroupSummary where
  hashWithSalt _salt TemplateStepGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` previous

instance Prelude.NFData TemplateStepGroupSummary where
  rnf TemplateStepGroupSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf previous
