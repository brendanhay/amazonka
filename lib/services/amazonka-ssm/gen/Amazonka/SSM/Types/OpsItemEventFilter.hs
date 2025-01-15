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
-- Module      : Amazonka.SSM.Types.OpsItemEventFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemEventFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OpsItemEventFilterKey
import Amazonka.SSM.Types.OpsItemEventFilterOperator

-- | Describes a filter for a specific list of OpsItem events. You can filter
-- event information by using tags. You specify tags by using a key-value
-- pair mapping.
--
-- /See:/ 'newOpsItemEventFilter' smart constructor.
data OpsItemEventFilter = OpsItemEventFilter'
  { -- | The name of the filter key. Currently, the only supported value is
    -- @OpsItemId@.
    key :: OpsItemEventFilterKey,
    -- | The values for the filter, consisting of one or more OpsItem IDs.
    values :: [Prelude.Text],
    -- | The operator used by the filter call. Currently, the only supported
    -- value is @Equal@.
    operator :: OpsItemEventFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItemEventFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'opsItemEventFilter_key' - The name of the filter key. Currently, the only supported value is
-- @OpsItemId@.
--
-- 'values', 'opsItemEventFilter_values' - The values for the filter, consisting of one or more OpsItem IDs.
--
-- 'operator', 'opsItemEventFilter_operator' - The operator used by the filter call. Currently, the only supported
-- value is @Equal@.
newOpsItemEventFilter ::
  -- | 'key'
  OpsItemEventFilterKey ->
  -- | 'operator'
  OpsItemEventFilterOperator ->
  OpsItemEventFilter
newOpsItemEventFilter pKey_ pOperator_ =
  OpsItemEventFilter'
    { key = pKey_,
      values = Prelude.mempty,
      operator = pOperator_
    }

-- | The name of the filter key. Currently, the only supported value is
-- @OpsItemId@.
opsItemEventFilter_key :: Lens.Lens' OpsItemEventFilter OpsItemEventFilterKey
opsItemEventFilter_key = Lens.lens (\OpsItemEventFilter' {key} -> key) (\s@OpsItemEventFilter' {} a -> s {key = a} :: OpsItemEventFilter)

-- | The values for the filter, consisting of one or more OpsItem IDs.
opsItemEventFilter_values :: Lens.Lens' OpsItemEventFilter [Prelude.Text]
opsItemEventFilter_values = Lens.lens (\OpsItemEventFilter' {values} -> values) (\s@OpsItemEventFilter' {} a -> s {values = a} :: OpsItemEventFilter) Prelude.. Lens.coerced

-- | The operator used by the filter call. Currently, the only supported
-- value is @Equal@.
opsItemEventFilter_operator :: Lens.Lens' OpsItemEventFilter OpsItemEventFilterOperator
opsItemEventFilter_operator = Lens.lens (\OpsItemEventFilter' {operator} -> operator) (\s@OpsItemEventFilter' {} a -> s {operator = a} :: OpsItemEventFilter)

instance Prelude.Hashable OpsItemEventFilter where
  hashWithSalt _salt OpsItemEventFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData OpsItemEventFilter where
  rnf OpsItemEventFilter' {..} =
    Prelude.rnf key `Prelude.seq`
      Prelude.rnf values `Prelude.seq`
        Prelude.rnf operator

instance Data.ToJSON OpsItemEventFilter where
  toJSON OpsItemEventFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
