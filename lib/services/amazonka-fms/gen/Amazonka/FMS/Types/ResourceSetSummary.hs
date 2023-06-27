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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ResourceSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ResourceSetStatus
import qualified Amazonka.Prelude as Prelude

-- | Summarizes the resource sets used in a policy.
--
-- /See:/ 'newResourceSetSummary' smart constructor.
data ResourceSetSummary = ResourceSetSummary'
  { -- | A description of the resource set.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the resource set. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | The last time that the resource set was changed.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The descriptive name of the resource set. You can\'t change the name of
    -- a resource set after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the resource set is in or out of an admin\'s Region
    -- scope.
    --
    -- -   @ACTIVE@ - The administrator can manage and delete the resource set.
    --
    -- -   @OUT_OF_ADMIN_SCOPE@ - The administrator can view the resource set,
    --     but they can\'t edit or delete the resource set. Existing
    --     protections stay in place. Any new resource that come into scope of
    --     the resource set won\'t be protected.
    resourceSetStatus :: Prelude.Maybe ResourceSetStatus
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
-- 'description', 'resourceSetSummary_description' - A description of the resource set.
--
-- 'id', 'resourceSetSummary_id' - A unique identifier for the resource set. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'lastUpdateTime', 'resourceSetSummary_lastUpdateTime' - The last time that the resource set was changed.
--
-- 'name', 'resourceSetSummary_name' - The descriptive name of the resource set. You can\'t change the name of
-- a resource set after you create it.
--
-- 'resourceSetStatus', 'resourceSetSummary_resourceSetStatus' - Indicates whether the resource set is in or out of an admin\'s Region
-- scope.
--
-- -   @ACTIVE@ - The administrator can manage and delete the resource set.
--
-- -   @OUT_OF_ADMIN_SCOPE@ - The administrator can view the resource set,
--     but they can\'t edit or delete the resource set. Existing
--     protections stay in place. Any new resource that come into scope of
--     the resource set won\'t be protected.
newResourceSetSummary ::
  ResourceSetSummary
newResourceSetSummary =
  ResourceSetSummary'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceSetStatus = Prelude.Nothing
    }

-- | A description of the resource set.
resourceSetSummary_description :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.Text)
resourceSetSummary_description = Lens.lens (\ResourceSetSummary' {description} -> description) (\s@ResourceSetSummary' {} a -> s {description = a} :: ResourceSetSummary)

-- | A unique identifier for the resource set. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
resourceSetSummary_id :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.Text)
resourceSetSummary_id = Lens.lens (\ResourceSetSummary' {id} -> id) (\s@ResourceSetSummary' {} a -> s {id = a} :: ResourceSetSummary)

-- | The last time that the resource set was changed.
resourceSetSummary_lastUpdateTime :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.UTCTime)
resourceSetSummary_lastUpdateTime = Lens.lens (\ResourceSetSummary' {lastUpdateTime} -> lastUpdateTime) (\s@ResourceSetSummary' {} a -> s {lastUpdateTime = a} :: ResourceSetSummary) Prelude.. Lens.mapping Data._Time

-- | The descriptive name of the resource set. You can\'t change the name of
-- a resource set after you create it.
resourceSetSummary_name :: Lens.Lens' ResourceSetSummary (Prelude.Maybe Prelude.Text)
resourceSetSummary_name = Lens.lens (\ResourceSetSummary' {name} -> name) (\s@ResourceSetSummary' {} a -> s {name = a} :: ResourceSetSummary)

-- | Indicates whether the resource set is in or out of an admin\'s Region
-- scope.
--
-- -   @ACTIVE@ - The administrator can manage and delete the resource set.
--
-- -   @OUT_OF_ADMIN_SCOPE@ - The administrator can view the resource set,
--     but they can\'t edit or delete the resource set. Existing
--     protections stay in place. Any new resource that come into scope of
--     the resource set won\'t be protected.
resourceSetSummary_resourceSetStatus :: Lens.Lens' ResourceSetSummary (Prelude.Maybe ResourceSetStatus)
resourceSetSummary_resourceSetStatus = Lens.lens (\ResourceSetSummary' {resourceSetStatus} -> resourceSetStatus) (\s@ResourceSetSummary' {} a -> s {resourceSetStatus = a} :: ResourceSetSummary)

instance Data.FromJSON ResourceSetSummary where
  parseJSON =
    Data.withObject
      "ResourceSetSummary"
      ( \x ->
          ResourceSetSummary'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceSetStatus")
      )

instance Prelude.Hashable ResourceSetSummary where
  hashWithSalt _salt ResourceSetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceSetStatus

instance Prelude.NFData ResourceSetSummary where
  rnf ResourceSetSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceSetStatus
