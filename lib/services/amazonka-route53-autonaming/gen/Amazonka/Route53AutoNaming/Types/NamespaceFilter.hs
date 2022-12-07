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
-- Module      : Amazonka.Route53AutoNaming.Types.NamespaceFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.NamespaceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.FilterCondition
import Amazonka.Route53AutoNaming.Types.NamespaceFilterName

-- | A complex type that identifies the namespaces that you want to list. You
-- can choose to list public or private namespaces.
--
-- /See:/ 'newNamespaceFilter' smart constructor.
data NamespaceFilter = NamespaceFilter'
  { -- | Specify the operator that you want to use to determine whether a
    -- namespace matches the specified value. Valid values for @Condition@ are
    -- one of the following.
    --
    -- -   @EQ@: When you specify @EQ@ for @Condition@, you can specify only
    --     one value. @EQ@ is supported for @TYPE@, @NAME@, and @HTTP_NAME@.
    --     @EQ@ is the default condition and can be omitted.
    --
    -- -   @BEGINS_WITH@: When you specify @BEGINS_WITH@ for @Condition@, you
    --     can specify only one value. @BEGINS_WITH@ is supported for @TYPE@,
    --     @NAME@, and @HTTP_NAME@.
    condition :: Prelude.Maybe FilterCondition,
    -- | Specify the namespaces that you want to get using one of the following.
    --
    -- -   @TYPE@: Gets the namespaces of the specified type.
    --
    -- -   @NAME@: Gets the namespaces with the specified name.
    --
    -- -   @HTTP_NAME@: Gets the namespaces with the specified HTTP name.
    name :: NamespaceFilterName,
    -- | Specify the values that are applicable to the value that you specify for
    -- @Name@.
    --
    -- -   @TYPE@: Specify @HTTP@, @DNS_PUBLIC@, or @DNS_PRIVATE@.
    --
    -- -   @NAME@: Specify the name of the namespace, which is found in
    --     @Namespace.Name@.
    --
    -- -   @HTTP_NAME@: Specify the HTTP name of the namespace, which is found
    --     in @Namespace.Properties.HttpProperties.HttpName@.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NamespaceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'namespaceFilter_condition' - Specify the operator that you want to use to determine whether a
-- namespace matches the specified value. Valid values for @Condition@ are
-- one of the following.
--
-- -   @EQ@: When you specify @EQ@ for @Condition@, you can specify only
--     one value. @EQ@ is supported for @TYPE@, @NAME@, and @HTTP_NAME@.
--     @EQ@ is the default condition and can be omitted.
--
-- -   @BEGINS_WITH@: When you specify @BEGINS_WITH@ for @Condition@, you
--     can specify only one value. @BEGINS_WITH@ is supported for @TYPE@,
--     @NAME@, and @HTTP_NAME@.
--
-- 'name', 'namespaceFilter_name' - Specify the namespaces that you want to get using one of the following.
--
-- -   @TYPE@: Gets the namespaces of the specified type.
--
-- -   @NAME@: Gets the namespaces with the specified name.
--
-- -   @HTTP_NAME@: Gets the namespaces with the specified HTTP name.
--
-- 'values', 'namespaceFilter_values' - Specify the values that are applicable to the value that you specify for
-- @Name@.
--
-- -   @TYPE@: Specify @HTTP@, @DNS_PUBLIC@, or @DNS_PRIVATE@.
--
-- -   @NAME@: Specify the name of the namespace, which is found in
--     @Namespace.Name@.
--
-- -   @HTTP_NAME@: Specify the HTTP name of the namespace, which is found
--     in @Namespace.Properties.HttpProperties.HttpName@.
newNamespaceFilter ::
  -- | 'name'
  NamespaceFilterName ->
  NamespaceFilter
newNamespaceFilter pName_ =
  NamespaceFilter'
    { condition = Prelude.Nothing,
      name = pName_,
      values = Prelude.mempty
    }

-- | Specify the operator that you want to use to determine whether a
-- namespace matches the specified value. Valid values for @Condition@ are
-- one of the following.
--
-- -   @EQ@: When you specify @EQ@ for @Condition@, you can specify only
--     one value. @EQ@ is supported for @TYPE@, @NAME@, and @HTTP_NAME@.
--     @EQ@ is the default condition and can be omitted.
--
-- -   @BEGINS_WITH@: When you specify @BEGINS_WITH@ for @Condition@, you
--     can specify only one value. @BEGINS_WITH@ is supported for @TYPE@,
--     @NAME@, and @HTTP_NAME@.
namespaceFilter_condition :: Lens.Lens' NamespaceFilter (Prelude.Maybe FilterCondition)
namespaceFilter_condition = Lens.lens (\NamespaceFilter' {condition} -> condition) (\s@NamespaceFilter' {} a -> s {condition = a} :: NamespaceFilter)

-- | Specify the namespaces that you want to get using one of the following.
--
-- -   @TYPE@: Gets the namespaces of the specified type.
--
-- -   @NAME@: Gets the namespaces with the specified name.
--
-- -   @HTTP_NAME@: Gets the namespaces with the specified HTTP name.
namespaceFilter_name :: Lens.Lens' NamespaceFilter NamespaceFilterName
namespaceFilter_name = Lens.lens (\NamespaceFilter' {name} -> name) (\s@NamespaceFilter' {} a -> s {name = a} :: NamespaceFilter)

-- | Specify the values that are applicable to the value that you specify for
-- @Name@.
--
-- -   @TYPE@: Specify @HTTP@, @DNS_PUBLIC@, or @DNS_PRIVATE@.
--
-- -   @NAME@: Specify the name of the namespace, which is found in
--     @Namespace.Name@.
--
-- -   @HTTP_NAME@: Specify the HTTP name of the namespace, which is found
--     in @Namespace.Properties.HttpProperties.HttpName@.
namespaceFilter_values :: Lens.Lens' NamespaceFilter [Prelude.Text]
namespaceFilter_values = Lens.lens (\NamespaceFilter' {values} -> values) (\s@NamespaceFilter' {} a -> s {values = a} :: NamespaceFilter) Prelude.. Lens.coerced

instance Prelude.Hashable NamespaceFilter where
  hashWithSalt _salt NamespaceFilter' {..} =
    _salt `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData NamespaceFilter where
  rnf NamespaceFilter' {..} =
    Prelude.rnf condition
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON NamespaceFilter where
  toJSON NamespaceFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Condition" Data..=) Prelude.<$> condition,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
