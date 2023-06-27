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
-- Module      : Amazonka.QuickSight.Types.DynamicDefaultValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DynamicDefaultValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | Defines different defaults to the users or groups based on mapping.
--
-- /See:/ 'newDynamicDefaultValue' smart constructor.
data DynamicDefaultValue = DynamicDefaultValue'
  { -- | The column that contains the group name.
    groupNameColumn :: Prelude.Maybe ColumnIdentifier,
    -- | The column that contains the username.
    userNameColumn :: Prelude.Maybe ColumnIdentifier,
    -- | The column that contains the default value of each user or group.
    defaultValueColumn :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamicDefaultValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupNameColumn', 'dynamicDefaultValue_groupNameColumn' - The column that contains the group name.
--
-- 'userNameColumn', 'dynamicDefaultValue_userNameColumn' - The column that contains the username.
--
-- 'defaultValueColumn', 'dynamicDefaultValue_defaultValueColumn' - The column that contains the default value of each user or group.
newDynamicDefaultValue ::
  -- | 'defaultValueColumn'
  ColumnIdentifier ->
  DynamicDefaultValue
newDynamicDefaultValue pDefaultValueColumn_ =
  DynamicDefaultValue'
    { groupNameColumn =
        Prelude.Nothing,
      userNameColumn = Prelude.Nothing,
      defaultValueColumn = pDefaultValueColumn_
    }

-- | The column that contains the group name.
dynamicDefaultValue_groupNameColumn :: Lens.Lens' DynamicDefaultValue (Prelude.Maybe ColumnIdentifier)
dynamicDefaultValue_groupNameColumn = Lens.lens (\DynamicDefaultValue' {groupNameColumn} -> groupNameColumn) (\s@DynamicDefaultValue' {} a -> s {groupNameColumn = a} :: DynamicDefaultValue)

-- | The column that contains the username.
dynamicDefaultValue_userNameColumn :: Lens.Lens' DynamicDefaultValue (Prelude.Maybe ColumnIdentifier)
dynamicDefaultValue_userNameColumn = Lens.lens (\DynamicDefaultValue' {userNameColumn} -> userNameColumn) (\s@DynamicDefaultValue' {} a -> s {userNameColumn = a} :: DynamicDefaultValue)

-- | The column that contains the default value of each user or group.
dynamicDefaultValue_defaultValueColumn :: Lens.Lens' DynamicDefaultValue ColumnIdentifier
dynamicDefaultValue_defaultValueColumn = Lens.lens (\DynamicDefaultValue' {defaultValueColumn} -> defaultValueColumn) (\s@DynamicDefaultValue' {} a -> s {defaultValueColumn = a} :: DynamicDefaultValue)

instance Data.FromJSON DynamicDefaultValue where
  parseJSON =
    Data.withObject
      "DynamicDefaultValue"
      ( \x ->
          DynamicDefaultValue'
            Prelude.<$> (x Data..:? "GroupNameColumn")
            Prelude.<*> (x Data..:? "UserNameColumn")
            Prelude.<*> (x Data..: "DefaultValueColumn")
      )

instance Prelude.Hashable DynamicDefaultValue where
  hashWithSalt _salt DynamicDefaultValue' {..} =
    _salt
      `Prelude.hashWithSalt` groupNameColumn
      `Prelude.hashWithSalt` userNameColumn
      `Prelude.hashWithSalt` defaultValueColumn

instance Prelude.NFData DynamicDefaultValue where
  rnf DynamicDefaultValue' {..} =
    Prelude.rnf groupNameColumn
      `Prelude.seq` Prelude.rnf userNameColumn
      `Prelude.seq` Prelude.rnf defaultValueColumn

instance Data.ToJSON DynamicDefaultValue where
  toJSON DynamicDefaultValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupNameColumn" Data..=)
              Prelude.<$> groupNameColumn,
            ("UserNameColumn" Data..=)
              Prelude.<$> userNameColumn,
            Prelude.Just
              ("DefaultValueColumn" Data..= defaultValueColumn)
          ]
      )
