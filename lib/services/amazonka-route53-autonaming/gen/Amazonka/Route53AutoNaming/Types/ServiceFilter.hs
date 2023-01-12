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
-- Module      : Amazonka.Route53AutoNaming.Types.ServiceFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.ServiceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.FilterCondition
import Amazonka.Route53AutoNaming.Types.ServiceFilterName

-- | A complex type that lets you specify the namespaces that you want to
-- list services for.
--
-- /See:/ 'newServiceFilter' smart constructor.
data ServiceFilter = ServiceFilter'
  { -- | The operator that you want to use to determine whether a service is
    -- returned by @ListServices@. Valid values for @Condition@ include the
    -- following:
    --
    -- -   @EQ@: When you specify @EQ@, specify one namespace ID for @Values@.
    --     @EQ@ is the default condition and can be omitted.
    condition :: Prelude.Maybe FilterCondition,
    -- | Specify @NAMESPACE_ID@.
    name :: ServiceFilterName,
    -- | The values that are applicable to the value that you specify for
    -- @Condition@ to filter the list of services.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'serviceFilter_condition' - The operator that you want to use to determine whether a service is
-- returned by @ListServices@. Valid values for @Condition@ include the
-- following:
--
-- -   @EQ@: When you specify @EQ@, specify one namespace ID for @Values@.
--     @EQ@ is the default condition and can be omitted.
--
-- 'name', 'serviceFilter_name' - Specify @NAMESPACE_ID@.
--
-- 'values', 'serviceFilter_values' - The values that are applicable to the value that you specify for
-- @Condition@ to filter the list of services.
newServiceFilter ::
  -- | 'name'
  ServiceFilterName ->
  ServiceFilter
newServiceFilter pName_ =
  ServiceFilter'
    { condition = Prelude.Nothing,
      name = pName_,
      values = Prelude.mempty
    }

-- | The operator that you want to use to determine whether a service is
-- returned by @ListServices@. Valid values for @Condition@ include the
-- following:
--
-- -   @EQ@: When you specify @EQ@, specify one namespace ID for @Values@.
--     @EQ@ is the default condition and can be omitted.
serviceFilter_condition :: Lens.Lens' ServiceFilter (Prelude.Maybe FilterCondition)
serviceFilter_condition = Lens.lens (\ServiceFilter' {condition} -> condition) (\s@ServiceFilter' {} a -> s {condition = a} :: ServiceFilter)

-- | Specify @NAMESPACE_ID@.
serviceFilter_name :: Lens.Lens' ServiceFilter ServiceFilterName
serviceFilter_name = Lens.lens (\ServiceFilter' {name} -> name) (\s@ServiceFilter' {} a -> s {name = a} :: ServiceFilter)

-- | The values that are applicable to the value that you specify for
-- @Condition@ to filter the list of services.
serviceFilter_values :: Lens.Lens' ServiceFilter [Prelude.Text]
serviceFilter_values = Lens.lens (\ServiceFilter' {values} -> values) (\s@ServiceFilter' {} a -> s {values = a} :: ServiceFilter) Prelude.. Lens.coerced

instance Prelude.Hashable ServiceFilter where
  hashWithSalt _salt ServiceFilter' {..} =
    _salt `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData ServiceFilter where
  rnf ServiceFilter' {..} =
    Prelude.rnf condition
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ServiceFilter where
  toJSON ServiceFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Condition" Data..=) Prelude.<$> condition,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
