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
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.NamespaceFilterName

-- | A complex type that identifies the namespaces that you want to list. You
-- can choose to list public or private namespaces.
--
-- /See:/ 'newNamespaceFilter' smart constructor.
data NamespaceFilter = NamespaceFilter'
  { -- | The operator that you want to use to determine whether @ListNamespaces@
    -- returns a namespace. Valid values for @condition@ include:
    --
    -- -   @EQ@: When you specify @EQ@ for the condition, you can choose to
    --     list only public namespaces or private namespaces, but not both.
    --     @EQ@ is the default condition and can be omitted.
    --
    -- -   @IN@: When you specify @IN@ for the condition, you can choose to
    --     list public namespaces, private namespaces, or both.
    --
    -- -   @BETWEEN@: Not applicable
    condition :: Prelude.Maybe FilterCondition,
    -- | Specify @TYPE@.
    name :: NamespaceFilterName,
    -- | If you specify @EQ@ for @Condition@, specify either @DNS_PUBLIC@ or
    -- @DNS_PRIVATE@.
    --
    -- If you specify @IN@ for @Condition@, you can specify @DNS_PUBLIC@,
    -- @DNS_PRIVATE@, or both.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NamespaceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'namespaceFilter_condition' - The operator that you want to use to determine whether @ListNamespaces@
-- returns a namespace. Valid values for @condition@ include:
--
-- -   @EQ@: When you specify @EQ@ for the condition, you can choose to
--     list only public namespaces or private namespaces, but not both.
--     @EQ@ is the default condition and can be omitted.
--
-- -   @IN@: When you specify @IN@ for the condition, you can choose to
--     list public namespaces, private namespaces, or both.
--
-- -   @BETWEEN@: Not applicable
--
-- 'name', 'namespaceFilter_name' - Specify @TYPE@.
--
-- 'values', 'namespaceFilter_values' - If you specify @EQ@ for @Condition@, specify either @DNS_PUBLIC@ or
-- @DNS_PRIVATE@.
--
-- If you specify @IN@ for @Condition@, you can specify @DNS_PUBLIC@,
-- @DNS_PRIVATE@, or both.
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

-- | The operator that you want to use to determine whether @ListNamespaces@
-- returns a namespace. Valid values for @condition@ include:
--
-- -   @EQ@: When you specify @EQ@ for the condition, you can choose to
--     list only public namespaces or private namespaces, but not both.
--     @EQ@ is the default condition and can be omitted.
--
-- -   @IN@: When you specify @IN@ for the condition, you can choose to
--     list public namespaces, private namespaces, or both.
--
-- -   @BETWEEN@: Not applicable
namespaceFilter_condition :: Lens.Lens' NamespaceFilter (Prelude.Maybe FilterCondition)
namespaceFilter_condition = Lens.lens (\NamespaceFilter' {condition} -> condition) (\s@NamespaceFilter' {} a -> s {condition = a} :: NamespaceFilter)

-- | Specify @TYPE@.
namespaceFilter_name :: Lens.Lens' NamespaceFilter NamespaceFilterName
namespaceFilter_name = Lens.lens (\NamespaceFilter' {name} -> name) (\s@NamespaceFilter' {} a -> s {name = a} :: NamespaceFilter)

-- | If you specify @EQ@ for @Condition@, specify either @DNS_PUBLIC@ or
-- @DNS_PRIVATE@.
--
-- If you specify @IN@ for @Condition@, you can specify @DNS_PUBLIC@,
-- @DNS_PRIVATE@, or both.
namespaceFilter_values :: Lens.Lens' NamespaceFilter [Prelude.Text]
namespaceFilter_values = Lens.lens (\NamespaceFilter' {values} -> values) (\s@NamespaceFilter' {} a -> s {values = a} :: NamespaceFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable NamespaceFilter

instance Prelude.NFData NamespaceFilter

instance Prelude.ToJSON NamespaceFilter where
  toJSON NamespaceFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Condition" Prelude..=) Prelude.<$> condition,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Values" Prelude..= values)
          ]
      )
