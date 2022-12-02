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
-- Module      : Amazonka.FMS.Types.ResourceSetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ResourceSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summarizes the resource sets used in a policy.
--
-- /See:/ 'newResourceSetSummary' smart constructor.
data ResourceSetSummary = ResourceSetSummary'
  { -- | The descriptive name of the resource set. You can\'t change the name of
    -- a resource set after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the resource set. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | A description of the resource set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last time that the resource set was changed.
    lastUpdateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resourceSetSummary_name' - The descriptive name of the resource set. You can\'t change the name of
-- a resource set after you create it.
--
-- 'id', 'resourceSetSummary_id' - A unique identifier for the resource set. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'description', 'resourceSetSummary_description' - A description of the resource set.
--
-- 'lastUpdateTime', 'resourceSetSummary_lastUpdateTime' - The last time that the resource set was changed.
newResourceSetSummary ::
  ResourceSetSummary
newResourceSetSummary =
  ResourceSetSummary'
    { name = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | The descriptive name of the resource set. You can\'t change the name of
-- a resource set after you create it.
resourceSetSummary_name :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.Text)
resourceSetSummary_name = Lens.lens (\ResourceSetSummary' {name} -> name) (\s@ResourceSetSummary' {} a -> s {name = a} :: ResourceSetSummary)

-- | A unique identifier for the resource set. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
resourceSetSummary_id :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.Text)
resourceSetSummary_id = Lens.lens (\ResourceSetSummary' {id} -> id) (\s@ResourceSetSummary' {} a -> s {id = a} :: ResourceSetSummary)

-- | A description of the resource set.
resourceSetSummary_description :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.Text)
resourceSetSummary_description = Lens.lens (\ResourceSetSummary' {description} -> description) (\s@ResourceSetSummary' {} a -> s {description = a} :: ResourceSetSummary)

-- | The last time that the resource set was changed.
resourceSetSummary_lastUpdateTime :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.UTCTime)
resourceSetSummary_lastUpdateTime = Lens.lens (\ResourceSetSummary' {lastUpdateTime} -> lastUpdateTime) (\s@ResourceSetSummary' {} a -> s {lastUpdateTime = a} :: ResourceSetSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ResourceSetSummary where
  parseJSON =
    Data.withObject
      "ResourceSetSummary"
      ( \x ->
          ResourceSetSummary'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastUpdateTime")
      )

instance Prelude.Hashable ResourceSetSummary where
  hashWithSalt _salt ResourceSetSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdateTime

instance Prelude.NFData ResourceSetSummary where
  rnf ResourceSetSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdateTime
