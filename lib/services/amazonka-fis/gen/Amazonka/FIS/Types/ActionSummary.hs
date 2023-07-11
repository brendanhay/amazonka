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
-- Module      : Amazonka.FIS.Types.ActionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ActionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ActionTarget
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of an action.
--
-- /See:/ 'newActionSummary' smart constructor.
data ActionSummary = ActionSummary'
  { -- | The description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the action.
    id :: Prelude.Maybe Prelude.Text,
    -- | The tags for the action.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'actionSummary_description' - The description for the action.
--
-- 'id', 'actionSummary_id' - The ID of the action.
--
-- 'tags', 'actionSummary_tags' - The tags for the action.
--
-- 'targets', 'actionSummary_targets' - The targets for the action.
newActionSummary ::
  ActionSummary
newActionSummary =
  ActionSummary'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      targets = Prelude.Nothing
    }

-- | The description for the action.
actionSummary_description :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_description = Lens.lens (\ActionSummary' {description} -> description) (\s@ActionSummary' {} a -> s {description = a} :: ActionSummary)

-- | The ID of the action.
actionSummary_id :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_id = Lens.lens (\ActionSummary' {id} -> id) (\s@ActionSummary' {} a -> s {id = a} :: ActionSummary)

-- | The tags for the action.
actionSummary_tags :: Lens.Lens' ActionSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionSummary_tags = Lens.lens (\ActionSummary' {tags} -> tags) (\s@ActionSummary' {} a -> s {tags = a} :: ActionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the action.
actionSummary_targets :: Lens.Lens' ActionSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget))
actionSummary_targets = Lens.lens (\ActionSummary' {targets} -> targets) (\s@ActionSummary' {} a -> s {targets = a} :: ActionSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ActionSummary where
  parseJSON =
    Data.withObject
      "ActionSummary"
      ( \x ->
          ActionSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ActionSummary where
  hashWithSalt _salt ActionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targets

instance Prelude.NFData ActionSummary where
  rnf ActionSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targets
