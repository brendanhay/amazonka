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
-- Module      : Amazonka.Connect.Types.UserSearchCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserSearchCriteria where

import Amazonka.Connect.Types.HierarchyGroupCondition
import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return users.
--
-- The @name@ and @description@ fields support \"contains\" queries with a
-- minimum of 2 characters and a maximum of 25 characters. Any queries with
-- character lengths outside of this range will throw invalid results.
--
-- /See:/ 'newUserSearchCriteria' smart constructor.
data UserSearchCriteria = UserSearchCriteria'
  { -- | A leaf node condition which can be used to specify a hierarchy group
    -- condition.
    hierarchyGroupCondition :: Prelude.Maybe HierarchyGroupCondition,
    -- | A leaf node condition which can be used to specify a string condition.
    stringCondition :: Prelude.Maybe StringCondition,
    -- | A list of conditions which would be applied together with an @OR@
    -- condition.
    orConditions :: Prelude.Maybe [UserSearchCriteria],
    -- | A list of conditions which would be applied together with an @AND@
    -- condition.
    andConditions :: Prelude.Maybe [UserSearchCriteria]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroupCondition', 'userSearchCriteria_hierarchyGroupCondition' - A leaf node condition which can be used to specify a hierarchy group
-- condition.
--
-- 'stringCondition', 'userSearchCriteria_stringCondition' - A leaf node condition which can be used to specify a string condition.
--
-- 'orConditions', 'userSearchCriteria_orConditions' - A list of conditions which would be applied together with an @OR@
-- condition.
--
-- 'andConditions', 'userSearchCriteria_andConditions' - A list of conditions which would be applied together with an @AND@
-- condition.
newUserSearchCriteria ::
  UserSearchCriteria
newUserSearchCriteria =
  UserSearchCriteria'
    { hierarchyGroupCondition =
        Prelude.Nothing,
      stringCondition = Prelude.Nothing,
      orConditions = Prelude.Nothing,
      andConditions = Prelude.Nothing
    }

-- | A leaf node condition which can be used to specify a hierarchy group
-- condition.
userSearchCriteria_hierarchyGroupCondition :: Lens.Lens' UserSearchCriteria (Prelude.Maybe HierarchyGroupCondition)
userSearchCriteria_hierarchyGroupCondition = Lens.lens (\UserSearchCriteria' {hierarchyGroupCondition} -> hierarchyGroupCondition) (\s@UserSearchCriteria' {} a -> s {hierarchyGroupCondition = a} :: UserSearchCriteria)

-- | A leaf node condition which can be used to specify a string condition.
userSearchCriteria_stringCondition :: Lens.Lens' UserSearchCriteria (Prelude.Maybe StringCondition)
userSearchCriteria_stringCondition = Lens.lens (\UserSearchCriteria' {stringCondition} -> stringCondition) (\s@UserSearchCriteria' {} a -> s {stringCondition = a} :: UserSearchCriteria)

-- | A list of conditions which would be applied together with an @OR@
-- condition.
userSearchCriteria_orConditions :: Lens.Lens' UserSearchCriteria (Prelude.Maybe [UserSearchCriteria])
userSearchCriteria_orConditions = Lens.lens (\UserSearchCriteria' {orConditions} -> orConditions) (\s@UserSearchCriteria' {} a -> s {orConditions = a} :: UserSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an @AND@
-- condition.
userSearchCriteria_andConditions :: Lens.Lens' UserSearchCriteria (Prelude.Maybe [UserSearchCriteria])
userSearchCriteria_andConditions = Lens.lens (\UserSearchCriteria' {andConditions} -> andConditions) (\s@UserSearchCriteria' {} a -> s {andConditions = a} :: UserSearchCriteria) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UserSearchCriteria where
  hashWithSalt _salt UserSearchCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` hierarchyGroupCondition
      `Prelude.hashWithSalt` stringCondition
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` andConditions

instance Prelude.NFData UserSearchCriteria where
  rnf UserSearchCriteria' {..} =
    Prelude.rnf hierarchyGroupCondition
      `Prelude.seq` Prelude.rnf stringCondition
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf andConditions

instance Core.ToJSON UserSearchCriteria where
  toJSON UserSearchCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HierarchyGroupCondition" Core..=)
              Prelude.<$> hierarchyGroupCondition,
            ("StringCondition" Core..=)
              Prelude.<$> stringCondition,
            ("OrConditions" Core..=) Prelude.<$> orConditions,
            ("AndConditions" Core..=) Prelude.<$> andConditions
          ]
      )
