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
-- Module      : Amazonka.ConnectCases.Types.FieldFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldFilter where

import Amazonka.ConnectCases.Types.FieldValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter for fields. Only one value can be provided.
--
-- /See:/ 'newFieldFilter' smart constructor.
data FieldFilter = FieldFilter'
  { -- | Object containing field identifier and value information.
    lessThan :: Prelude.Maybe FieldValue,
    -- | Object containing field identifier and value information.
    contains :: Prelude.Maybe FieldValue,
    -- | Object containing field identifier and value information.
    greaterThanOrEqualTo :: Prelude.Maybe FieldValue,
    -- | Object containing field identifier and value information.
    equalTo :: Prelude.Maybe FieldValue,
    -- | Object containing field identifier and value information.
    lessThanOrEqualTo :: Prelude.Maybe FieldValue,
    -- | Object containing field identifier and value information.
    greaterThan :: Prelude.Maybe FieldValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lessThan', 'fieldFilter_lessThan' - Object containing field identifier and value information.
--
-- 'contains', 'fieldFilter_contains' - Object containing field identifier and value information.
--
-- 'greaterThanOrEqualTo', 'fieldFilter_greaterThanOrEqualTo' - Object containing field identifier and value information.
--
-- 'equalTo', 'fieldFilter_equalTo' - Object containing field identifier and value information.
--
-- 'lessThanOrEqualTo', 'fieldFilter_lessThanOrEqualTo' - Object containing field identifier and value information.
--
-- 'greaterThan', 'fieldFilter_greaterThan' - Object containing field identifier and value information.
newFieldFilter ::
  FieldFilter
newFieldFilter =
  FieldFilter'
    { lessThan = Prelude.Nothing,
      contains = Prelude.Nothing,
      greaterThanOrEqualTo = Prelude.Nothing,
      equalTo = Prelude.Nothing,
      lessThanOrEqualTo = Prelude.Nothing,
      greaterThan = Prelude.Nothing
    }

-- | Object containing field identifier and value information.
fieldFilter_lessThan :: Lens.Lens' FieldFilter (Prelude.Maybe FieldValue)
fieldFilter_lessThan = Lens.lens (\FieldFilter' {lessThan} -> lessThan) (\s@FieldFilter' {} a -> s {lessThan = a} :: FieldFilter)

-- | Object containing field identifier and value information.
fieldFilter_contains :: Lens.Lens' FieldFilter (Prelude.Maybe FieldValue)
fieldFilter_contains = Lens.lens (\FieldFilter' {contains} -> contains) (\s@FieldFilter' {} a -> s {contains = a} :: FieldFilter)

-- | Object containing field identifier and value information.
fieldFilter_greaterThanOrEqualTo :: Lens.Lens' FieldFilter (Prelude.Maybe FieldValue)
fieldFilter_greaterThanOrEqualTo = Lens.lens (\FieldFilter' {greaterThanOrEqualTo} -> greaterThanOrEqualTo) (\s@FieldFilter' {} a -> s {greaterThanOrEqualTo = a} :: FieldFilter)

-- | Object containing field identifier and value information.
fieldFilter_equalTo :: Lens.Lens' FieldFilter (Prelude.Maybe FieldValue)
fieldFilter_equalTo = Lens.lens (\FieldFilter' {equalTo} -> equalTo) (\s@FieldFilter' {} a -> s {equalTo = a} :: FieldFilter)

-- | Object containing field identifier and value information.
fieldFilter_lessThanOrEqualTo :: Lens.Lens' FieldFilter (Prelude.Maybe FieldValue)
fieldFilter_lessThanOrEqualTo = Lens.lens (\FieldFilter' {lessThanOrEqualTo} -> lessThanOrEqualTo) (\s@FieldFilter' {} a -> s {lessThanOrEqualTo = a} :: FieldFilter)

-- | Object containing field identifier and value information.
fieldFilter_greaterThan :: Lens.Lens' FieldFilter (Prelude.Maybe FieldValue)
fieldFilter_greaterThan = Lens.lens (\FieldFilter' {greaterThan} -> greaterThan) (\s@FieldFilter' {} a -> s {greaterThan = a} :: FieldFilter)

instance Prelude.Hashable FieldFilter where
  hashWithSalt _salt FieldFilter' {..} =
    _salt `Prelude.hashWithSalt` lessThan
      `Prelude.hashWithSalt` contains
      `Prelude.hashWithSalt` greaterThanOrEqualTo
      `Prelude.hashWithSalt` equalTo
      `Prelude.hashWithSalt` lessThanOrEqualTo
      `Prelude.hashWithSalt` greaterThan

instance Prelude.NFData FieldFilter where
  rnf FieldFilter' {..} =
    Prelude.rnf lessThan
      `Prelude.seq` Prelude.rnf contains
      `Prelude.seq` Prelude.rnf greaterThanOrEqualTo
      `Prelude.seq` Prelude.rnf equalTo
      `Prelude.seq` Prelude.rnf lessThanOrEqualTo
      `Prelude.seq` Prelude.rnf greaterThan

instance Data.ToJSON FieldFilter where
  toJSON FieldFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lessThan" Data..=) Prelude.<$> lessThan,
            ("contains" Data..=) Prelude.<$> contains,
            ("greaterThanOrEqualTo" Data..=)
              Prelude.<$> greaterThanOrEqualTo,
            ("equalTo" Data..=) Prelude.<$> equalTo,
            ("lessThanOrEqualTo" Data..=)
              Prelude.<$> lessThanOrEqualTo,
            ("greaterThan" Data..=) Prelude.<$> greaterThan
          ]
      )
