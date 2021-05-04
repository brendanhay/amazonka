{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.OpsItemFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OpsItemFilterKey
import Network.AWS.SSM.Types.OpsItemFilterOperator

-- | Describes an OpsItem filter.
--
-- /See:/ 'newOpsItemFilter' smart constructor.
data OpsItemFilter = OpsItemFilter'
  { -- | The name of the filter.
    key :: OpsItemFilterKey,
    -- | The filter value.
    values :: [Prelude.Text],
    -- | The operator used by the filter call.
    operator :: OpsItemFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OpsItemFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'opsItemFilter_key' - The name of the filter.
--
-- 'values', 'opsItemFilter_values' - The filter value.
--
-- 'operator', 'opsItemFilter_operator' - The operator used by the filter call.
newOpsItemFilter ::
  -- | 'key'
  OpsItemFilterKey ->
  -- | 'operator'
  OpsItemFilterOperator ->
  OpsItemFilter
newOpsItemFilter pKey_ pOperator_ =
  OpsItemFilter'
    { key = pKey_,
      values = Prelude.mempty,
      operator = pOperator_
    }

-- | The name of the filter.
opsItemFilter_key :: Lens.Lens' OpsItemFilter OpsItemFilterKey
opsItemFilter_key = Lens.lens (\OpsItemFilter' {key} -> key) (\s@OpsItemFilter' {} a -> s {key = a} :: OpsItemFilter)

-- | The filter value.
opsItemFilter_values :: Lens.Lens' OpsItemFilter [Prelude.Text]
opsItemFilter_values = Lens.lens (\OpsItemFilter' {values} -> values) (\s@OpsItemFilter' {} a -> s {values = a} :: OpsItemFilter) Prelude.. Prelude._Coerce

-- | The operator used by the filter call.
opsItemFilter_operator :: Lens.Lens' OpsItemFilter OpsItemFilterOperator
opsItemFilter_operator = Lens.lens (\OpsItemFilter' {operator} -> operator) (\s@OpsItemFilter' {} a -> s {operator = a} :: OpsItemFilter)

instance Prelude.Hashable OpsItemFilter

instance Prelude.NFData OpsItemFilter

instance Prelude.ToJSON OpsItemFilter where
  toJSON OpsItemFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Prelude..= key),
            Prelude.Just ("Values" Prelude..= values),
            Prelude.Just ("Operator" Prelude..= operator)
          ]
      )
