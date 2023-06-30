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
-- Module      : Amazonka.Kendra.Types.AttributeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AttributeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DocumentAttribute
import qualified Amazonka.Prelude as Prelude

-- | Provides filtering the query results based on document attributes or
-- metadata fields.
--
-- When you use the @AndAllFilters@ or @OrAllFilters@, filters you can use
-- 2 layers under the first attribute filter. For example, you can use:
--
-- @\<AndAllFilters>@
--
-- 1.  @ \<OrAllFilters>@
--
-- 2.  @ \<EqualsTo>@
--
-- If you use more than 2 layers, you receive a @ValidationException@
-- exception with the message \"@AttributeFilter@ cannot have a depth of
-- more than 2.\"
--
-- If you use more than 10 attribute filters in a given list for
-- @AndAllFilters@ or @OrAllFilters@, you receive a @ValidationException@
-- with the message \"@AttributeFilter@ cannot have a length of more than
-- 10\".
--
-- /See:/ 'newAttributeFilter' smart constructor.
data AttributeFilter = AttributeFilter'
  { -- | Performs a logical @AND@ operation on all supplied filters.
    andAllFilters :: Prelude.Maybe [AttributeFilter],
    -- | Returns true when a document contains all of the specified document
    -- attributes or metadata fields. This filter is only applicable to
    -- @StringListValue@ metadata.
    containsAll :: Prelude.Maybe DocumentAttribute,
    -- | Returns true when a document contains any of the specified document
    -- attributes or metadata fields. This filter is only applicable to
    -- @StringListValue@ metadata.
    containsAny :: Prelude.Maybe DocumentAttribute,
    -- | Performs an equals operation on two document attributes or metadata
    -- fields.
    equalsTo :: Prelude.Maybe DocumentAttribute,
    -- | Performs a greater than operation on two document attributes or metadata
    -- fields. Use with a document attribute of type @Date@ or @Long@.
    greaterThan :: Prelude.Maybe DocumentAttribute,
    -- | Performs a greater or equals than operation on two document attributes
    -- or metadata fields. Use with a document attribute of type @Date@ or
    -- @Long@.
    greaterThanOrEquals :: Prelude.Maybe DocumentAttribute,
    -- | Performs a less than operation on two document attributes or metadata
    -- fields. Use with a document attribute of type @Date@ or @Long@.
    lessThan :: Prelude.Maybe DocumentAttribute,
    -- | Performs a less than or equals operation on two document attributes or
    -- metadata fields. Use with a document attribute of type @Date@ or @Long@.
    lessThanOrEquals :: Prelude.Maybe DocumentAttribute,
    -- | Performs a logical @NOT@ operation on all supplied filters.
    notFilter :: Prelude.Maybe AttributeFilter,
    -- | Performs a logical @OR@ operation on all supplied filters.
    orAllFilters :: Prelude.Maybe [AttributeFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andAllFilters', 'attributeFilter_andAllFilters' - Performs a logical @AND@ operation on all supplied filters.
--
-- 'containsAll', 'attributeFilter_containsAll' - Returns true when a document contains all of the specified document
-- attributes or metadata fields. This filter is only applicable to
-- @StringListValue@ metadata.
--
-- 'containsAny', 'attributeFilter_containsAny' - Returns true when a document contains any of the specified document
-- attributes or metadata fields. This filter is only applicable to
-- @StringListValue@ metadata.
--
-- 'equalsTo', 'attributeFilter_equalsTo' - Performs an equals operation on two document attributes or metadata
-- fields.
--
-- 'greaterThan', 'attributeFilter_greaterThan' - Performs a greater than operation on two document attributes or metadata
-- fields. Use with a document attribute of type @Date@ or @Long@.
--
-- 'greaterThanOrEquals', 'attributeFilter_greaterThanOrEquals' - Performs a greater or equals than operation on two document attributes
-- or metadata fields. Use with a document attribute of type @Date@ or
-- @Long@.
--
-- 'lessThan', 'attributeFilter_lessThan' - Performs a less than operation on two document attributes or metadata
-- fields. Use with a document attribute of type @Date@ or @Long@.
--
-- 'lessThanOrEquals', 'attributeFilter_lessThanOrEquals' - Performs a less than or equals operation on two document attributes or
-- metadata fields. Use with a document attribute of type @Date@ or @Long@.
--
-- 'notFilter', 'attributeFilter_notFilter' - Performs a logical @NOT@ operation on all supplied filters.
--
-- 'orAllFilters', 'attributeFilter_orAllFilters' - Performs a logical @OR@ operation on all supplied filters.
newAttributeFilter ::
  AttributeFilter
newAttributeFilter =
  AttributeFilter'
    { andAllFilters = Prelude.Nothing,
      containsAll = Prelude.Nothing,
      containsAny = Prelude.Nothing,
      equalsTo = Prelude.Nothing,
      greaterThan = Prelude.Nothing,
      greaterThanOrEquals = Prelude.Nothing,
      lessThan = Prelude.Nothing,
      lessThanOrEquals = Prelude.Nothing,
      notFilter = Prelude.Nothing,
      orAllFilters = Prelude.Nothing
    }

-- | Performs a logical @AND@ operation on all supplied filters.
attributeFilter_andAllFilters :: Lens.Lens' AttributeFilter (Prelude.Maybe [AttributeFilter])
attributeFilter_andAllFilters = Lens.lens (\AttributeFilter' {andAllFilters} -> andAllFilters) (\s@AttributeFilter' {} a -> s {andAllFilters = a} :: AttributeFilter) Prelude.. Lens.mapping Lens.coerced

-- | Returns true when a document contains all of the specified document
-- attributes or metadata fields. This filter is only applicable to
-- @StringListValue@ metadata.
attributeFilter_containsAll :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
attributeFilter_containsAll = Lens.lens (\AttributeFilter' {containsAll} -> containsAll) (\s@AttributeFilter' {} a -> s {containsAll = a} :: AttributeFilter)

-- | Returns true when a document contains any of the specified document
-- attributes or metadata fields. This filter is only applicable to
-- @StringListValue@ metadata.
attributeFilter_containsAny :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
attributeFilter_containsAny = Lens.lens (\AttributeFilter' {containsAny} -> containsAny) (\s@AttributeFilter' {} a -> s {containsAny = a} :: AttributeFilter)

-- | Performs an equals operation on two document attributes or metadata
-- fields.
attributeFilter_equalsTo :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
attributeFilter_equalsTo = Lens.lens (\AttributeFilter' {equalsTo} -> equalsTo) (\s@AttributeFilter' {} a -> s {equalsTo = a} :: AttributeFilter)

-- | Performs a greater than operation on two document attributes or metadata
-- fields. Use with a document attribute of type @Date@ or @Long@.
attributeFilter_greaterThan :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
attributeFilter_greaterThan = Lens.lens (\AttributeFilter' {greaterThan} -> greaterThan) (\s@AttributeFilter' {} a -> s {greaterThan = a} :: AttributeFilter)

-- | Performs a greater or equals than operation on two document attributes
-- or metadata fields. Use with a document attribute of type @Date@ or
-- @Long@.
attributeFilter_greaterThanOrEquals :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
attributeFilter_greaterThanOrEquals = Lens.lens (\AttributeFilter' {greaterThanOrEquals} -> greaterThanOrEquals) (\s@AttributeFilter' {} a -> s {greaterThanOrEquals = a} :: AttributeFilter)

-- | Performs a less than operation on two document attributes or metadata
-- fields. Use with a document attribute of type @Date@ or @Long@.
attributeFilter_lessThan :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
attributeFilter_lessThan = Lens.lens (\AttributeFilter' {lessThan} -> lessThan) (\s@AttributeFilter' {} a -> s {lessThan = a} :: AttributeFilter)

-- | Performs a less than or equals operation on two document attributes or
-- metadata fields. Use with a document attribute of type @Date@ or @Long@.
attributeFilter_lessThanOrEquals :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
attributeFilter_lessThanOrEquals = Lens.lens (\AttributeFilter' {lessThanOrEquals} -> lessThanOrEquals) (\s@AttributeFilter' {} a -> s {lessThanOrEquals = a} :: AttributeFilter)

-- | Performs a logical @NOT@ operation on all supplied filters.
attributeFilter_notFilter :: Lens.Lens' AttributeFilter (Prelude.Maybe AttributeFilter)
attributeFilter_notFilter = Lens.lens (\AttributeFilter' {notFilter} -> notFilter) (\s@AttributeFilter' {} a -> s {notFilter = a} :: AttributeFilter)

-- | Performs a logical @OR@ operation on all supplied filters.
attributeFilter_orAllFilters :: Lens.Lens' AttributeFilter (Prelude.Maybe [AttributeFilter])
attributeFilter_orAllFilters = Lens.lens (\AttributeFilter' {orAllFilters} -> orAllFilters) (\s@AttributeFilter' {} a -> s {orAllFilters = a} :: AttributeFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AttributeFilter where
  hashWithSalt _salt AttributeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` andAllFilters
      `Prelude.hashWithSalt` containsAll
      `Prelude.hashWithSalt` containsAny
      `Prelude.hashWithSalt` equalsTo
      `Prelude.hashWithSalt` greaterThan
      `Prelude.hashWithSalt` greaterThanOrEquals
      `Prelude.hashWithSalt` lessThan
      `Prelude.hashWithSalt` lessThanOrEquals
      `Prelude.hashWithSalt` notFilter
      `Prelude.hashWithSalt` orAllFilters

instance Prelude.NFData AttributeFilter where
  rnf AttributeFilter' {..} =
    Prelude.rnf andAllFilters
      `Prelude.seq` Prelude.rnf containsAll
      `Prelude.seq` Prelude.rnf containsAny
      `Prelude.seq` Prelude.rnf equalsTo
      `Prelude.seq` Prelude.rnf greaterThan
      `Prelude.seq` Prelude.rnf greaterThanOrEquals
      `Prelude.seq` Prelude.rnf lessThan
      `Prelude.seq` Prelude.rnf lessThanOrEquals
      `Prelude.seq` Prelude.rnf notFilter
      `Prelude.seq` Prelude.rnf orAllFilters

instance Data.ToJSON AttributeFilter where
  toJSON AttributeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndAllFilters" Data..=) Prelude.<$> andAllFilters,
            ("ContainsAll" Data..=) Prelude.<$> containsAll,
            ("ContainsAny" Data..=) Prelude.<$> containsAny,
            ("EqualsTo" Data..=) Prelude.<$> equalsTo,
            ("GreaterThan" Data..=) Prelude.<$> greaterThan,
            ("GreaterThanOrEquals" Data..=)
              Prelude.<$> greaterThanOrEquals,
            ("LessThan" Data..=) Prelude.<$> lessThan,
            ("LessThanOrEquals" Data..=)
              Prelude.<$> lessThanOrEquals,
            ("NotFilter" Data..=) Prelude.<$> notFilter,
            ("OrAllFilters" Data..=) Prelude.<$> orAllFilters
          ]
      )
