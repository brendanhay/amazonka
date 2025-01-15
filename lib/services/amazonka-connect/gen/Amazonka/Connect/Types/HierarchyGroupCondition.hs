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
-- Module      : Amazonka.Connect.Types.HierarchyGroupCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyGroupCondition where

import Amazonka.Connect.Types.HierarchyGroupMatchType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A leaf node condition which can be used to specify a hierarchy group
-- condition.
--
-- /See:/ 'newHierarchyGroupCondition' smart constructor.
data HierarchyGroupCondition = HierarchyGroupCondition'
  { -- | The type of hierarchy group match.
    hierarchyGroupMatchType :: Prelude.Maybe HierarchyGroupMatchType,
    -- | The value in the hierarchy group condition.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HierarchyGroupCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroupMatchType', 'hierarchyGroupCondition_hierarchyGroupMatchType' - The type of hierarchy group match.
--
-- 'value', 'hierarchyGroupCondition_value' - The value in the hierarchy group condition.
newHierarchyGroupCondition ::
  HierarchyGroupCondition
newHierarchyGroupCondition =
  HierarchyGroupCondition'
    { hierarchyGroupMatchType =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The type of hierarchy group match.
hierarchyGroupCondition_hierarchyGroupMatchType :: Lens.Lens' HierarchyGroupCondition (Prelude.Maybe HierarchyGroupMatchType)
hierarchyGroupCondition_hierarchyGroupMatchType = Lens.lens (\HierarchyGroupCondition' {hierarchyGroupMatchType} -> hierarchyGroupMatchType) (\s@HierarchyGroupCondition' {} a -> s {hierarchyGroupMatchType = a} :: HierarchyGroupCondition)

-- | The value in the hierarchy group condition.
hierarchyGroupCondition_value :: Lens.Lens' HierarchyGroupCondition (Prelude.Maybe Prelude.Text)
hierarchyGroupCondition_value = Lens.lens (\HierarchyGroupCondition' {value} -> value) (\s@HierarchyGroupCondition' {} a -> s {value = a} :: HierarchyGroupCondition)

instance Prelude.Hashable HierarchyGroupCondition where
  hashWithSalt _salt HierarchyGroupCondition' {..} =
    _salt
      `Prelude.hashWithSalt` hierarchyGroupMatchType
      `Prelude.hashWithSalt` value

instance Prelude.NFData HierarchyGroupCondition where
  rnf HierarchyGroupCondition' {..} =
    Prelude.rnf hierarchyGroupMatchType `Prelude.seq`
      Prelude.rnf value

instance Data.ToJSON HierarchyGroupCondition where
  toJSON HierarchyGroupCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HierarchyGroupMatchType" Data..=)
              Prelude.<$> hierarchyGroupMatchType,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
