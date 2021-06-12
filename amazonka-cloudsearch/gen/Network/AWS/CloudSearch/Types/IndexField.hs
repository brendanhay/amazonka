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
-- Module      : Network.AWS.CloudSearch.Types.IndexField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IndexField where

import Network.AWS.CloudSearch.Types.DateArrayOptions
import Network.AWS.CloudSearch.Types.DateOptions
import Network.AWS.CloudSearch.Types.DoubleArrayOptions
import Network.AWS.CloudSearch.Types.DoubleOptions
import Network.AWS.CloudSearch.Types.IndexFieldType
import Network.AWS.CloudSearch.Types.IntArrayOptions
import Network.AWS.CloudSearch.Types.IntOptions
import Network.AWS.CloudSearch.Types.LatLonOptions
import Network.AWS.CloudSearch.Types.LiteralArrayOptions
import Network.AWS.CloudSearch.Types.LiteralOptions
import Network.AWS.CloudSearch.Types.TextArrayOptions
import Network.AWS.CloudSearch.Types.TextOptions
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information for a field in the index, including its name,
-- type, and options. The supported options depend on the @IndexFieldType@.
--
-- /See:/ 'newIndexField' smart constructor.
data IndexField = IndexField'
  { doubleArrayOptions :: Core.Maybe DoubleArrayOptions,
    latLonOptions :: Core.Maybe LatLonOptions,
    textArrayOptions :: Core.Maybe TextArrayOptions,
    dateArrayOptions :: Core.Maybe DateArrayOptions,
    doubleOptions :: Core.Maybe DoubleOptions,
    textOptions :: Core.Maybe TextOptions,
    intArrayOptions :: Core.Maybe IntArrayOptions,
    literalArrayOptions :: Core.Maybe LiteralArrayOptions,
    dateOptions :: Core.Maybe DateOptions,
    intOptions :: Core.Maybe IntOptions,
    literalOptions :: Core.Maybe LiteralOptions,
    -- | A string that represents the name of an index field. CloudSearch
    -- supports regular index fields as well as dynamic fields. A dynamic
    -- field\'s name defines a pattern that begins or ends with a wildcard. Any
    -- document fields that don\'t map to a regular index field but do match a
    -- dynamic field\'s pattern are configured with the dynamic field\'s
    -- indexing options.
    --
    -- Regular field names begin with a letter and can contain the following
    -- characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field
    -- names must begin or end with a wildcard (*). The wildcard can also be
    -- the only character in a dynamic field name. Multiple wildcards, and
    -- wildcards embedded within a string are not supported.
    --
    -- The name @score@ is reserved and cannot be used as a field name. To
    -- reference a document\'s ID, you can use the name @_id@.
    indexFieldName :: Core.Text,
    indexFieldType :: IndexFieldType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IndexField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'doubleArrayOptions', 'indexField_doubleArrayOptions' - Undocumented member.
--
-- 'latLonOptions', 'indexField_latLonOptions' - Undocumented member.
--
-- 'textArrayOptions', 'indexField_textArrayOptions' - Undocumented member.
--
-- 'dateArrayOptions', 'indexField_dateArrayOptions' - Undocumented member.
--
-- 'doubleOptions', 'indexField_doubleOptions' - Undocumented member.
--
-- 'textOptions', 'indexField_textOptions' - Undocumented member.
--
-- 'intArrayOptions', 'indexField_intArrayOptions' - Undocumented member.
--
-- 'literalArrayOptions', 'indexField_literalArrayOptions' - Undocumented member.
--
-- 'dateOptions', 'indexField_dateOptions' - Undocumented member.
--
-- 'intOptions', 'indexField_intOptions' - Undocumented member.
--
-- 'literalOptions', 'indexField_literalOptions' - Undocumented member.
--
-- 'indexFieldName', 'indexField_indexFieldName' - A string that represents the name of an index field. CloudSearch
-- supports regular index fields as well as dynamic fields. A dynamic
-- field\'s name defines a pattern that begins or ends with a wildcard. Any
-- document fields that don\'t map to a regular index field but do match a
-- dynamic field\'s pattern are configured with the dynamic field\'s
-- indexing options.
--
-- Regular field names begin with a letter and can contain the following
-- characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field
-- names must begin or end with a wildcard (*). The wildcard can also be
-- the only character in a dynamic field name. Multiple wildcards, and
-- wildcards embedded within a string are not supported.
--
-- The name @score@ is reserved and cannot be used as a field name. To
-- reference a document\'s ID, you can use the name @_id@.
--
-- 'indexFieldType', 'indexField_indexFieldType' - Undocumented member.
newIndexField ::
  -- | 'indexFieldName'
  Core.Text ->
  -- | 'indexFieldType'
  IndexFieldType ->
  IndexField
newIndexField pIndexFieldName_ pIndexFieldType_ =
  IndexField'
    { doubleArrayOptions = Core.Nothing,
      latLonOptions = Core.Nothing,
      textArrayOptions = Core.Nothing,
      dateArrayOptions = Core.Nothing,
      doubleOptions = Core.Nothing,
      textOptions = Core.Nothing,
      intArrayOptions = Core.Nothing,
      literalArrayOptions = Core.Nothing,
      dateOptions = Core.Nothing,
      intOptions = Core.Nothing,
      literalOptions = Core.Nothing,
      indexFieldName = pIndexFieldName_,
      indexFieldType = pIndexFieldType_
    }

-- | Undocumented member.
indexField_doubleArrayOptions :: Lens.Lens' IndexField (Core.Maybe DoubleArrayOptions)
indexField_doubleArrayOptions = Lens.lens (\IndexField' {doubleArrayOptions} -> doubleArrayOptions) (\s@IndexField' {} a -> s {doubleArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_latLonOptions :: Lens.Lens' IndexField (Core.Maybe LatLonOptions)
indexField_latLonOptions = Lens.lens (\IndexField' {latLonOptions} -> latLonOptions) (\s@IndexField' {} a -> s {latLonOptions = a} :: IndexField)

-- | Undocumented member.
indexField_textArrayOptions :: Lens.Lens' IndexField (Core.Maybe TextArrayOptions)
indexField_textArrayOptions = Lens.lens (\IndexField' {textArrayOptions} -> textArrayOptions) (\s@IndexField' {} a -> s {textArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_dateArrayOptions :: Lens.Lens' IndexField (Core.Maybe DateArrayOptions)
indexField_dateArrayOptions = Lens.lens (\IndexField' {dateArrayOptions} -> dateArrayOptions) (\s@IndexField' {} a -> s {dateArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_doubleOptions :: Lens.Lens' IndexField (Core.Maybe DoubleOptions)
indexField_doubleOptions = Lens.lens (\IndexField' {doubleOptions} -> doubleOptions) (\s@IndexField' {} a -> s {doubleOptions = a} :: IndexField)

-- | Undocumented member.
indexField_textOptions :: Lens.Lens' IndexField (Core.Maybe TextOptions)
indexField_textOptions = Lens.lens (\IndexField' {textOptions} -> textOptions) (\s@IndexField' {} a -> s {textOptions = a} :: IndexField)

-- | Undocumented member.
indexField_intArrayOptions :: Lens.Lens' IndexField (Core.Maybe IntArrayOptions)
indexField_intArrayOptions = Lens.lens (\IndexField' {intArrayOptions} -> intArrayOptions) (\s@IndexField' {} a -> s {intArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_literalArrayOptions :: Lens.Lens' IndexField (Core.Maybe LiteralArrayOptions)
indexField_literalArrayOptions = Lens.lens (\IndexField' {literalArrayOptions} -> literalArrayOptions) (\s@IndexField' {} a -> s {literalArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_dateOptions :: Lens.Lens' IndexField (Core.Maybe DateOptions)
indexField_dateOptions = Lens.lens (\IndexField' {dateOptions} -> dateOptions) (\s@IndexField' {} a -> s {dateOptions = a} :: IndexField)

-- | Undocumented member.
indexField_intOptions :: Lens.Lens' IndexField (Core.Maybe IntOptions)
indexField_intOptions = Lens.lens (\IndexField' {intOptions} -> intOptions) (\s@IndexField' {} a -> s {intOptions = a} :: IndexField)

-- | Undocumented member.
indexField_literalOptions :: Lens.Lens' IndexField (Core.Maybe LiteralOptions)
indexField_literalOptions = Lens.lens (\IndexField' {literalOptions} -> literalOptions) (\s@IndexField' {} a -> s {literalOptions = a} :: IndexField)

-- | A string that represents the name of an index field. CloudSearch
-- supports regular index fields as well as dynamic fields. A dynamic
-- field\'s name defines a pattern that begins or ends with a wildcard. Any
-- document fields that don\'t map to a regular index field but do match a
-- dynamic field\'s pattern are configured with the dynamic field\'s
-- indexing options.
--
-- Regular field names begin with a letter and can contain the following
-- characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field
-- names must begin or end with a wildcard (*). The wildcard can also be
-- the only character in a dynamic field name. Multiple wildcards, and
-- wildcards embedded within a string are not supported.
--
-- The name @score@ is reserved and cannot be used as a field name. To
-- reference a document\'s ID, you can use the name @_id@.
indexField_indexFieldName :: Lens.Lens' IndexField Core.Text
indexField_indexFieldName = Lens.lens (\IndexField' {indexFieldName} -> indexFieldName) (\s@IndexField' {} a -> s {indexFieldName = a} :: IndexField)

-- | Undocumented member.
indexField_indexFieldType :: Lens.Lens' IndexField IndexFieldType
indexField_indexFieldType = Lens.lens (\IndexField' {indexFieldType} -> indexFieldType) (\s@IndexField' {} a -> s {indexFieldType = a} :: IndexField)

instance Core.FromXML IndexField where
  parseXML x =
    IndexField'
      Core.<$> (x Core..@? "DoubleArrayOptions")
      Core.<*> (x Core..@? "LatLonOptions")
      Core.<*> (x Core..@? "TextArrayOptions")
      Core.<*> (x Core..@? "DateArrayOptions")
      Core.<*> (x Core..@? "DoubleOptions")
      Core.<*> (x Core..@? "TextOptions")
      Core.<*> (x Core..@? "IntArrayOptions")
      Core.<*> (x Core..@? "LiteralArrayOptions")
      Core.<*> (x Core..@? "DateOptions")
      Core.<*> (x Core..@? "IntOptions")
      Core.<*> (x Core..@? "LiteralOptions")
      Core.<*> (x Core..@ "IndexFieldName")
      Core.<*> (x Core..@ "IndexFieldType")

instance Core.Hashable IndexField

instance Core.NFData IndexField

instance Core.ToQuery IndexField where
  toQuery IndexField' {..} =
    Core.mconcat
      [ "DoubleArrayOptions" Core.=: doubleArrayOptions,
        "LatLonOptions" Core.=: latLonOptions,
        "TextArrayOptions" Core.=: textArrayOptions,
        "DateArrayOptions" Core.=: dateArrayOptions,
        "DoubleOptions" Core.=: doubleOptions,
        "TextOptions" Core.=: textOptions,
        "IntArrayOptions" Core.=: intArrayOptions,
        "LiteralArrayOptions" Core.=: literalArrayOptions,
        "DateOptions" Core.=: dateOptions,
        "IntOptions" Core.=: intOptions,
        "LiteralOptions" Core.=: literalOptions,
        "IndexFieldName" Core.=: indexFieldName,
        "IndexFieldType" Core.=: indexFieldType
      ]
