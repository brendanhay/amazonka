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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.TemplateSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.TemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of the template.
--
-- /See:/ 'newTemplateSummary' smart constructor.
data TemplateSummary = TemplateSummary'
  { -- | The Amazon Resource Name (ARN) of the template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the template.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the template.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'templateSummary_arn' - The Amazon Resource Name (ARN) of the template.
--
-- 'description', 'templateSummary_description' - The description of the template.
--
-- 'id', 'templateSummary_id' - The ID of the template.
--
-- 'name', 'templateSummary_name' - The name of the template.
newTemplateSummary ::
  TemplateSummary
newTemplateSummary =
  TemplateSummary'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the template.
templateSummary_arn :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_arn = Lens.lens (\TemplateSummary' {arn} -> arn) (\s@TemplateSummary' {} a -> s {arn = a} :: TemplateSummary)

-- | The description of the template.
templateSummary_description :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_description = Lens.lens (\TemplateSummary' {description} -> description) (\s@TemplateSummary' {} a -> s {description = a} :: TemplateSummary)

-- | The ID of the template.
templateSummary_id :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_id = Lens.lens (\TemplateSummary' {id} -> id) (\s@TemplateSummary' {} a -> s {id = a} :: TemplateSummary)

-- | The name of the template.
templateSummary_name :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_name = Lens.lens (\TemplateSummary' {name} -> name) (\s@TemplateSummary' {} a -> s {name = a} :: TemplateSummary)

instance Data.FromJSON TemplateSummary where
  parseJSON =
    Data.withObject
      "TemplateSummary"
      ( \x ->
          TemplateSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable TemplateSummary where
  hashWithSalt _salt TemplateSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData TemplateSummary where
  rnf TemplateSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf name
