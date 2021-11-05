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
-- Module      : Network.AWS.FIS.Types.ActionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FIS.Types.ActionSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.FIS.Types.ActionTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a summary of an action.
--
-- /See:/ 'newActionSummary' smart constructor.
data ActionSummary = ActionSummary'
  { -- | The targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget),
    -- | The ID of the action.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the action.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'targets', 'actionSummary_targets' - The targets for the action.
--
-- 'id', 'actionSummary_id' - The ID of the action.
--
-- 'description', 'actionSummary_description' - The description for the action.
--
-- 'tags', 'actionSummary_tags' - The tags for the action.
newActionSummary ::
  ActionSummary
newActionSummary =
  ActionSummary'
    { targets = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The targets for the action.
actionSummary_targets :: Lens.Lens' ActionSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget))
actionSummary_targets = Lens.lens (\ActionSummary' {targets} -> targets) (\s@ActionSummary' {} a -> s {targets = a} :: ActionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the action.
actionSummary_id :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_id = Lens.lens (\ActionSummary' {id} -> id) (\s@ActionSummary' {} a -> s {id = a} :: ActionSummary)

-- | The description for the action.
actionSummary_description :: Lens.Lens' ActionSummary (Prelude.Maybe Prelude.Text)
actionSummary_description = Lens.lens (\ActionSummary' {description} -> description) (\s@ActionSummary' {} a -> s {description = a} :: ActionSummary)

-- | The tags for the action.
actionSummary_tags :: Lens.Lens' ActionSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionSummary_tags = Lens.lens (\ActionSummary' {tags} -> tags) (\s@ActionSummary' {} a -> s {tags = a} :: ActionSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ActionSummary where
  parseJSON =
    Core.withObject
      "ActionSummary"
      ( \x ->
          ActionSummary'
            Prelude.<$> (x Core..:? "targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ActionSummary

instance Prelude.NFData ActionSummary
