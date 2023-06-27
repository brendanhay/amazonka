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
-- Module      : Amazonka.Connect.Types.HoursOfOperationSearchCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HoursOfOperationSearchCriteria where

import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return hours of operations.
--
-- /See:/ 'newHoursOfOperationSearchCriteria' smart constructor.
data HoursOfOperationSearchCriteria = HoursOfOperationSearchCriteria'
  { -- | A list of conditions which would be applied together with an AND
    -- condition.
    andConditions :: Prelude.Maybe [HoursOfOperationSearchCriteria],
    -- | A list of conditions which would be applied together with an OR
    -- condition.
    orConditions :: Prelude.Maybe [HoursOfOperationSearchCriteria],
    -- | A leaf node condition which can be used to specify a string condition.
    --
    -- The currently supported values for @FieldName@ are @name@,
    -- @description@, @timezone@, and @resourceID@.
    stringCondition :: Prelude.Maybe StringCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoursOfOperationSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andConditions', 'hoursOfOperationSearchCriteria_andConditions' - A list of conditions which would be applied together with an AND
-- condition.
--
-- 'orConditions', 'hoursOfOperationSearchCriteria_orConditions' - A list of conditions which would be applied together with an OR
-- condition.
--
-- 'stringCondition', 'hoursOfOperationSearchCriteria_stringCondition' - A leaf node condition which can be used to specify a string condition.
--
-- The currently supported values for @FieldName@ are @name@,
-- @description@, @timezone@, and @resourceID@.
newHoursOfOperationSearchCriteria ::
  HoursOfOperationSearchCriteria
newHoursOfOperationSearchCriteria =
  HoursOfOperationSearchCriteria'
    { andConditions =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      stringCondition = Prelude.Nothing
    }

-- | A list of conditions which would be applied together with an AND
-- condition.
hoursOfOperationSearchCriteria_andConditions :: Lens.Lens' HoursOfOperationSearchCriteria (Prelude.Maybe [HoursOfOperationSearchCriteria])
hoursOfOperationSearchCriteria_andConditions = Lens.lens (\HoursOfOperationSearchCriteria' {andConditions} -> andConditions) (\s@HoursOfOperationSearchCriteria' {} a -> s {andConditions = a} :: HoursOfOperationSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an OR
-- condition.
hoursOfOperationSearchCriteria_orConditions :: Lens.Lens' HoursOfOperationSearchCriteria (Prelude.Maybe [HoursOfOperationSearchCriteria])
hoursOfOperationSearchCriteria_orConditions = Lens.lens (\HoursOfOperationSearchCriteria' {orConditions} -> orConditions) (\s@HoursOfOperationSearchCriteria' {} a -> s {orConditions = a} :: HoursOfOperationSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A leaf node condition which can be used to specify a string condition.
--
-- The currently supported values for @FieldName@ are @name@,
-- @description@, @timezone@, and @resourceID@.
hoursOfOperationSearchCriteria_stringCondition :: Lens.Lens' HoursOfOperationSearchCriteria (Prelude.Maybe StringCondition)
hoursOfOperationSearchCriteria_stringCondition = Lens.lens (\HoursOfOperationSearchCriteria' {stringCondition} -> stringCondition) (\s@HoursOfOperationSearchCriteria' {} a -> s {stringCondition = a} :: HoursOfOperationSearchCriteria)

instance
  Prelude.Hashable
    HoursOfOperationSearchCriteria
  where
  hashWithSalt
    _salt
    HoursOfOperationSearchCriteria' {..} =
      _salt
        `Prelude.hashWithSalt` andConditions
        `Prelude.hashWithSalt` orConditions
        `Prelude.hashWithSalt` stringCondition

instance
  Prelude.NFData
    HoursOfOperationSearchCriteria
  where
  rnf HoursOfOperationSearchCriteria' {..} =
    Prelude.rnf andConditions
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf stringCondition

instance Data.ToJSON HoursOfOperationSearchCriteria where
  toJSON HoursOfOperationSearchCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndConditions" Data..=) Prelude.<$> andConditions,
            ("OrConditions" Data..=) Prelude.<$> orConditions,
            ("StringCondition" Data..=)
              Prelude.<$> stringCondition
          ]
      )
