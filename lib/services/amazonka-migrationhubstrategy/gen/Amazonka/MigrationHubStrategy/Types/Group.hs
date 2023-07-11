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
-- Module      : Amazonka.MigrationHubStrategy.Types.Group
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.GroupName
import qualified Amazonka.Prelude as Prelude

-- | The object containing information about distinct imports or groups for
-- Strategy Recommendations.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The key of the specific import group.
    name :: Prelude.Maybe GroupName,
    -- | The value of the specific import group.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'group_name' - The key of the specific import group.
--
-- 'value', 'group_value' - The value of the specific import group.
newGroup ::
  Group
newGroup =
  Group'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key of the specific import group.
group_name :: Lens.Lens' Group (Prelude.Maybe GroupName)
group_name = Lens.lens (\Group' {name} -> name) (\s@Group' {} a -> s {name = a} :: Group)

-- | The value of the specific import group.
group_value :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_value = Lens.lens (\Group' {value} -> value) (\s@Group' {} a -> s {value = a} :: Group)

instance Prelude.Hashable Group where
  hashWithSalt _salt Group' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Group where
  toJSON Group' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("value" Data..=) Prelude.<$> value
          ]
      )
