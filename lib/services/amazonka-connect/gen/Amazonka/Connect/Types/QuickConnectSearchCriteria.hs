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
-- Module      : Amazonka.Connect.Types.QuickConnectSearchCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QuickConnectSearchCriteria where

import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return quick connects.
--
-- /See:/ 'newQuickConnectSearchCriteria' smart constructor.
data QuickConnectSearchCriteria = QuickConnectSearchCriteria'
  { -- | A list of conditions which would be applied together with an AND
    -- condition.
    andConditions :: Prelude.Maybe [QuickConnectSearchCriteria],
    -- | A list of conditions which would be applied together with an OR
    -- condition.
    orConditions :: Prelude.Maybe [QuickConnectSearchCriteria],
    -- | A leaf node condition which can be used to specify a string condition.
    --
    -- The currently supported values for @FieldName@ are @name@,
    -- @description@, and @resourceID@.
    stringCondition :: Prelude.Maybe StringCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuickConnectSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andConditions', 'quickConnectSearchCriteria_andConditions' - A list of conditions which would be applied together with an AND
-- condition.
--
-- 'orConditions', 'quickConnectSearchCriteria_orConditions' - A list of conditions which would be applied together with an OR
-- condition.
--
-- 'stringCondition', 'quickConnectSearchCriteria_stringCondition' - A leaf node condition which can be used to specify a string condition.
--
-- The currently supported values for @FieldName@ are @name@,
-- @description@, and @resourceID@.
newQuickConnectSearchCriteria ::
  QuickConnectSearchCriteria
newQuickConnectSearchCriteria =
  QuickConnectSearchCriteria'
    { andConditions =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      stringCondition = Prelude.Nothing
    }

-- | A list of conditions which would be applied together with an AND
-- condition.
quickConnectSearchCriteria_andConditions :: Lens.Lens' QuickConnectSearchCriteria (Prelude.Maybe [QuickConnectSearchCriteria])
quickConnectSearchCriteria_andConditions = Lens.lens (\QuickConnectSearchCriteria' {andConditions} -> andConditions) (\s@QuickConnectSearchCriteria' {} a -> s {andConditions = a} :: QuickConnectSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an OR
-- condition.
quickConnectSearchCriteria_orConditions :: Lens.Lens' QuickConnectSearchCriteria (Prelude.Maybe [QuickConnectSearchCriteria])
quickConnectSearchCriteria_orConditions = Lens.lens (\QuickConnectSearchCriteria' {orConditions} -> orConditions) (\s@QuickConnectSearchCriteria' {} a -> s {orConditions = a} :: QuickConnectSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A leaf node condition which can be used to specify a string condition.
--
-- The currently supported values for @FieldName@ are @name@,
-- @description@, and @resourceID@.
quickConnectSearchCriteria_stringCondition :: Lens.Lens' QuickConnectSearchCriteria (Prelude.Maybe StringCondition)
quickConnectSearchCriteria_stringCondition = Lens.lens (\QuickConnectSearchCriteria' {stringCondition} -> stringCondition) (\s@QuickConnectSearchCriteria' {} a -> s {stringCondition = a} :: QuickConnectSearchCriteria)

instance Prelude.Hashable QuickConnectSearchCriteria where
  hashWithSalt _salt QuickConnectSearchCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` andConditions
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` stringCondition

instance Prelude.NFData QuickConnectSearchCriteria where
  rnf QuickConnectSearchCriteria' {..} =
    Prelude.rnf andConditions
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf stringCondition

instance Data.ToJSON QuickConnectSearchCriteria where
  toJSON QuickConnectSearchCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndConditions" Data..=) Prelude.<$> andConditions,
            ("OrConditions" Data..=) Prelude.<$> orConditions,
            ("StringCondition" Data..=)
              Prelude.<$> stringCondition
          ]
      )
