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
-- Module      : Amazonka.MGN.Types.SourceServerActionsRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SourceServerActionsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Source server post migration custom action filters.
--
-- /See:/ 'newSourceServerActionsRequestFilters' smart constructor.
data SourceServerActionsRequestFilters = SourceServerActionsRequestFilters'
  { -- | Action IDs to filter source server post migration custom actions by.
    actionIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceServerActionsRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionIDs', 'sourceServerActionsRequestFilters_actionIDs' - Action IDs to filter source server post migration custom actions by.
newSourceServerActionsRequestFilters ::
  SourceServerActionsRequestFilters
newSourceServerActionsRequestFilters =
  SourceServerActionsRequestFilters'
    { actionIDs =
        Prelude.Nothing
    }

-- | Action IDs to filter source server post migration custom actions by.
sourceServerActionsRequestFilters_actionIDs :: Lens.Lens' SourceServerActionsRequestFilters (Prelude.Maybe [Prelude.Text])
sourceServerActionsRequestFilters_actionIDs = Lens.lens (\SourceServerActionsRequestFilters' {actionIDs} -> actionIDs) (\s@SourceServerActionsRequestFilters' {} a -> s {actionIDs = a} :: SourceServerActionsRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SourceServerActionsRequestFilters
  where
  hashWithSalt
    _salt
    SourceServerActionsRequestFilters' {..} =
      _salt `Prelude.hashWithSalt` actionIDs

instance
  Prelude.NFData
    SourceServerActionsRequestFilters
  where
  rnf SourceServerActionsRequestFilters' {..} =
    Prelude.rnf actionIDs

instance
  Data.ToJSON
    SourceServerActionsRequestFilters
  where
  toJSON SourceServerActionsRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [("actionIDs" Data..=) Prelude.<$> actionIDs]
      )
