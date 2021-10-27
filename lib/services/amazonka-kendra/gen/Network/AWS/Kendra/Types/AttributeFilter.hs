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
-- Module      : Network.AWS.Kendra.Types.AttributeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.AttributeFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DocumentAttribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides filtering the query results based on document attributes.
--
-- When you use the @AndAllFilters@ or @OrAllFilters@, filters you can use
-- 2 layers under the first attribute filter. For example, you can use:
--
-- @\<AndAllFilters>@
--
-- 1.  @ \<OrAllFilters>@
--
-- 2.  @ \<EqualTo>@
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
  { -- | Performs a logical @OR@ operation on all supplied filters.
    orAllFilters :: Prelude.Maybe [AttributeFilter],
    -- | Returns true when a document contains all of the specified document
    -- attributes. This filter is only applicable to @StringListValue@
    -- metadata.
    containsAll :: Prelude.Maybe DocumentAttribute,
    -- | Performs a less than operation on two document attributes. Use with a
    -- document attribute of type @Date@ or @Long@.
    lessThan :: Prelude.Maybe DocumentAttribute,
    -- | Returns true when a document contains any of the specified document
    -- attributes. This filter is only applicable to @StringListValue@
    -- metadata.
    containsAny :: Prelude.Maybe DocumentAttribute,
    -- | Performs a less than or equals operation on two document attributes. Use
    -- with a document attribute of type @Date@ or @Long@.
    lessThanOrEquals :: Prelude.Maybe DocumentAttribute,
    -- | Performs an equals operation on two document attributes.
    equalsTo :: Prelude.Maybe DocumentAttribute,
    -- | Performs a logical @NOT@ operation on all supplied filters.
    notFilter :: Prelude.Maybe AttributeFilter,
    -- | Performs a greater or equals than operation on two document attributes.
    -- Use with a document attribute of type @Date@ or @Long@.
    greaterThanOrEquals :: Prelude.Maybe DocumentAttribute,
    -- | Performs a greater than operation on two document attributes. Use with a
    -- document attribute of type @Date@ or @Long@.
    greaterThan :: Prelude.Maybe DocumentAttribute,
    -- | Performs a logical @AND@ operation on all supplied filters.
    andAllFilters :: Prelude.Maybe [AttributeFilter]
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
-- 'orAllFilters', 'orAllFilters' - Performs a logical @OR@ operation on all supplied filters.
--
-- 'containsAll', 'containsAll' - Returns true when a document contains all of the specified document
-- attributes. This filter is only applicable to @StringListValue@
-- metadata.
--
-- 'lessThan', 'lessThan' - Performs a less than operation on two document attributes. Use with a
-- document attribute of type @Date@ or @Long@.
--
-- 'containsAny', 'containsAny' - Returns true when a document contains any of the specified document
-- attributes. This filter is only applicable to @StringListValue@
-- metadata.
--
-- 'lessThanOrEquals', 'lessThanOrEquals' - Performs a less than or equals operation on two document attributes. Use
-- with a document attribute of type @Date@ or @Long@.
--
-- 'equalsTo', 'equalsTo' - Performs an equals operation on two document attributes.
--
-- 'notFilter', 'notFilter' - Performs a logical @NOT@ operation on all supplied filters.
--
-- 'greaterThanOrEquals', 'greaterThanOrEquals' - Performs a greater or equals than operation on two document attributes.
-- Use with a document attribute of type @Date@ or @Long@.
--
-- 'greaterThan', 'greaterThan' - Performs a greater than operation on two document attributes. Use with a
-- document attribute of type @Date@ or @Long@.
--
-- 'andAllFilters', 'andAllFilters' - Performs a logical @AND@ operation on all supplied filters.
newAttributeFilter ::
  AttributeFilter
newAttributeFilter =
  AttributeFilter'
    { orAllFilters = Prelude.Nothing,
      containsAll = Prelude.Nothing,
      lessThan = Prelude.Nothing,
      containsAny = Prelude.Nothing,
      lessThanOrEquals = Prelude.Nothing,
      equalsTo = Prelude.Nothing,
      notFilter = Prelude.Nothing,
      greaterThanOrEquals = Prelude.Nothing,
      greaterThan = Prelude.Nothing,
      andAllFilters = Prelude.Nothing
    }

-- | Performs a logical @OR@ operation on all supplied filters.
orAllFilters :: Lens.Lens' AttributeFilter (Prelude.Maybe [AttributeFilter])
orAllFilters = Lens.lens (\AttributeFilter' {orAllFilters} -> orAllFilters) (\s@AttributeFilter' {} a -> s {orAllFilters = a} :: AttributeFilter) Prelude.. Lens.mapping Lens.coerced

-- | Returns true when a document contains all of the specified document
-- attributes. This filter is only applicable to @StringListValue@
-- metadata.
containsAll :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
containsAll = Lens.lens (\AttributeFilter' {containsAll} -> containsAll) (\s@AttributeFilter' {} a -> s {containsAll = a} :: AttributeFilter)

-- | Performs a less than operation on two document attributes. Use with a
-- document attribute of type @Date@ or @Long@.
lessThan :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
lessThan = Lens.lens (\AttributeFilter' {lessThan} -> lessThan) (\s@AttributeFilter' {} a -> s {lessThan = a} :: AttributeFilter)

-- | Returns true when a document contains any of the specified document
-- attributes. This filter is only applicable to @StringListValue@
-- metadata.
containsAny :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
containsAny = Lens.lens (\AttributeFilter' {containsAny} -> containsAny) (\s@AttributeFilter' {} a -> s {containsAny = a} :: AttributeFilter)

-- | Performs a less than or equals operation on two document attributes. Use
-- with a document attribute of type @Date@ or @Long@.
lessThanOrEquals :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
lessThanOrEquals = Lens.lens (\AttributeFilter' {lessThanOrEquals} -> lessThanOrEquals) (\s@AttributeFilter' {} a -> s {lessThanOrEquals = a} :: AttributeFilter)

-- | Performs an equals operation on two document attributes.
equalsTo :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
equalsTo = Lens.lens (\AttributeFilter' {equalsTo} -> equalsTo) (\s@AttributeFilter' {} a -> s {equalsTo = a} :: AttributeFilter)

-- | Performs a logical @NOT@ operation on all supplied filters.
notFilter :: Lens.Lens' AttributeFilter (Prelude.Maybe AttributeFilter)
notFilter = Lens.lens (\AttributeFilter' {notFilter} -> notFilter) (\s@AttributeFilter' {} a -> s {notFilter = a} :: AttributeFilter)

-- | Performs a greater or equals than operation on two document attributes.
-- Use with a document attribute of type @Date@ or @Long@.
greaterThanOrEquals :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
greaterThanOrEquals = Lens.lens (\AttributeFilter' {greaterThanOrEquals} -> greaterThanOrEquals) (\s@AttributeFilter' {} a -> s {greaterThanOrEquals = a} :: AttributeFilter)

-- | Performs a greater than operation on two document attributes. Use with a
-- document attribute of type @Date@ or @Long@.
greaterThan :: Lens.Lens' AttributeFilter (Prelude.Maybe DocumentAttribute)
greaterThan = Lens.lens (\AttributeFilter' {greaterThan} -> greaterThan) (\s@AttributeFilter' {} a -> s {greaterThan = a} :: AttributeFilter)

-- | Performs a logical @AND@ operation on all supplied filters.
andAllFilters :: Lens.Lens' AttributeFilter (Prelude.Maybe [AttributeFilter])
andAllFilters = Lens.lens (\AttributeFilter' {andAllFilters} -> andAllFilters) (\s@AttributeFilter' {} a -> s {andAllFilters = a} :: AttributeFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AttributeFilter

instance Prelude.NFData AttributeFilter

instance Core.ToJSON AttributeFilter where
  toJSON AttributeFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OrAllFilters" Core..=) Prelude.<$> orAllFilters,
            ("ContainsAll" Core..=) Prelude.<$> containsAll,
            ("LessThan" Core..=) Prelude.<$> lessThan,
            ("ContainsAny" Core..=) Prelude.<$> containsAny,
            ("LessThanOrEquals" Core..=)
              Prelude.<$> lessThanOrEquals,
            ("EqualsTo" Core..=) Prelude.<$> equalsTo,
            ("NotFilter" Core..=) Prelude.<$> notFilter,
            ("GreaterThanOrEquals" Core..=)
              Prelude.<$> greaterThanOrEquals,
            ("GreaterThan" Core..=) Prelude.<$> greaterThan,
            ("AndAllFilters" Core..=) Prelude.<$> andAllFilters
          ]
      )
