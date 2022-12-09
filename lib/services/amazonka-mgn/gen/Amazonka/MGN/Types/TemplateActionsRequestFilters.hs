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
-- Module      : Amazonka.MGN.Types.TemplateActionsRequestFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.TemplateActionsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Template post migration custom action filters.
--
-- /See:/ 'newTemplateActionsRequestFilters' smart constructor.
data TemplateActionsRequestFilters = TemplateActionsRequestFilters'
  { -- | Action IDs to filter template post migration custom actions by.
    actionIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateActionsRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionIDs', 'templateActionsRequestFilters_actionIDs' - Action IDs to filter template post migration custom actions by.
newTemplateActionsRequestFilters ::
  TemplateActionsRequestFilters
newTemplateActionsRequestFilters =
  TemplateActionsRequestFilters'
    { actionIDs =
        Prelude.Nothing
    }

-- | Action IDs to filter template post migration custom actions by.
templateActionsRequestFilters_actionIDs :: Lens.Lens' TemplateActionsRequestFilters (Prelude.Maybe [Prelude.Text])
templateActionsRequestFilters_actionIDs = Lens.lens (\TemplateActionsRequestFilters' {actionIDs} -> actionIDs) (\s@TemplateActionsRequestFilters' {} a -> s {actionIDs = a} :: TemplateActionsRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    TemplateActionsRequestFilters
  where
  hashWithSalt _salt TemplateActionsRequestFilters' {..} =
    _salt `Prelude.hashWithSalt` actionIDs

instance Prelude.NFData TemplateActionsRequestFilters where
  rnf TemplateActionsRequestFilters' {..} =
    Prelude.rnf actionIDs

instance Data.ToJSON TemplateActionsRequestFilters where
  toJSON TemplateActionsRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [("actionIDs" Data..=) Prelude.<$> actionIDs]
      )
