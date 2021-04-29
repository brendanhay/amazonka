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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information for a field in the index, including its name,
-- type, and options. The supported options depend on the @IndexFieldType@.
--
-- /See:/ 'newIndexField' smart constructor.
data IndexField = IndexField'
  { doubleArrayOptions :: Prelude.Maybe DoubleArrayOptions,
    latLonOptions :: Prelude.Maybe LatLonOptions,
    textArrayOptions :: Prelude.Maybe TextArrayOptions,
    dateArrayOptions :: Prelude.Maybe DateArrayOptions,
    doubleOptions :: Prelude.Maybe DoubleOptions,
    textOptions :: Prelude.Maybe TextOptions,
    intArrayOptions :: Prelude.Maybe IntArrayOptions,
    literalArrayOptions :: Prelude.Maybe LiteralArrayOptions,
    dateOptions :: Prelude.Maybe DateOptions,
    intOptions :: Prelude.Maybe IntOptions,
    literalOptions :: Prelude.Maybe LiteralOptions,
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
    indexFieldName :: Prelude.Text,
    indexFieldType :: IndexFieldType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'indexFieldType'
  IndexFieldType ->
  IndexField
newIndexField pIndexFieldName_ pIndexFieldType_ =
  IndexField'
    { doubleArrayOptions = Prelude.Nothing,
      latLonOptions = Prelude.Nothing,
      textArrayOptions = Prelude.Nothing,
      dateArrayOptions = Prelude.Nothing,
      doubleOptions = Prelude.Nothing,
      textOptions = Prelude.Nothing,
      intArrayOptions = Prelude.Nothing,
      literalArrayOptions = Prelude.Nothing,
      dateOptions = Prelude.Nothing,
      intOptions = Prelude.Nothing,
      literalOptions = Prelude.Nothing,
      indexFieldName = pIndexFieldName_,
      indexFieldType = pIndexFieldType_
    }

-- | Undocumented member.
indexField_doubleArrayOptions :: Lens.Lens' IndexField (Prelude.Maybe DoubleArrayOptions)
indexField_doubleArrayOptions = Lens.lens (\IndexField' {doubleArrayOptions} -> doubleArrayOptions) (\s@IndexField' {} a -> s {doubleArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_latLonOptions :: Lens.Lens' IndexField (Prelude.Maybe LatLonOptions)
indexField_latLonOptions = Lens.lens (\IndexField' {latLonOptions} -> latLonOptions) (\s@IndexField' {} a -> s {latLonOptions = a} :: IndexField)

-- | Undocumented member.
indexField_textArrayOptions :: Lens.Lens' IndexField (Prelude.Maybe TextArrayOptions)
indexField_textArrayOptions = Lens.lens (\IndexField' {textArrayOptions} -> textArrayOptions) (\s@IndexField' {} a -> s {textArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_dateArrayOptions :: Lens.Lens' IndexField (Prelude.Maybe DateArrayOptions)
indexField_dateArrayOptions = Lens.lens (\IndexField' {dateArrayOptions} -> dateArrayOptions) (\s@IndexField' {} a -> s {dateArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_doubleOptions :: Lens.Lens' IndexField (Prelude.Maybe DoubleOptions)
indexField_doubleOptions = Lens.lens (\IndexField' {doubleOptions} -> doubleOptions) (\s@IndexField' {} a -> s {doubleOptions = a} :: IndexField)

-- | Undocumented member.
indexField_textOptions :: Lens.Lens' IndexField (Prelude.Maybe TextOptions)
indexField_textOptions = Lens.lens (\IndexField' {textOptions} -> textOptions) (\s@IndexField' {} a -> s {textOptions = a} :: IndexField)

-- | Undocumented member.
indexField_intArrayOptions :: Lens.Lens' IndexField (Prelude.Maybe IntArrayOptions)
indexField_intArrayOptions = Lens.lens (\IndexField' {intArrayOptions} -> intArrayOptions) (\s@IndexField' {} a -> s {intArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_literalArrayOptions :: Lens.Lens' IndexField (Prelude.Maybe LiteralArrayOptions)
indexField_literalArrayOptions = Lens.lens (\IndexField' {literalArrayOptions} -> literalArrayOptions) (\s@IndexField' {} a -> s {literalArrayOptions = a} :: IndexField)

-- | Undocumented member.
indexField_dateOptions :: Lens.Lens' IndexField (Prelude.Maybe DateOptions)
indexField_dateOptions = Lens.lens (\IndexField' {dateOptions} -> dateOptions) (\s@IndexField' {} a -> s {dateOptions = a} :: IndexField)

-- | Undocumented member.
indexField_intOptions :: Lens.Lens' IndexField (Prelude.Maybe IntOptions)
indexField_intOptions = Lens.lens (\IndexField' {intOptions} -> intOptions) (\s@IndexField' {} a -> s {intOptions = a} :: IndexField)

-- | Undocumented member.
indexField_literalOptions :: Lens.Lens' IndexField (Prelude.Maybe LiteralOptions)
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
indexField_indexFieldName :: Lens.Lens' IndexField Prelude.Text
indexField_indexFieldName = Lens.lens (\IndexField' {indexFieldName} -> indexFieldName) (\s@IndexField' {} a -> s {indexFieldName = a} :: IndexField)

-- | Undocumented member.
indexField_indexFieldType :: Lens.Lens' IndexField IndexFieldType
indexField_indexFieldType = Lens.lens (\IndexField' {indexFieldType} -> indexFieldType) (\s@IndexField' {} a -> s {indexFieldType = a} :: IndexField)

instance Prelude.FromXML IndexField where
  parseXML x =
    IndexField'
      Prelude.<$> (x Prelude..@? "DoubleArrayOptions")
      Prelude.<*> (x Prelude..@? "LatLonOptions")
      Prelude.<*> (x Prelude..@? "TextArrayOptions")
      Prelude.<*> (x Prelude..@? "DateArrayOptions")
      Prelude.<*> (x Prelude..@? "DoubleOptions")
      Prelude.<*> (x Prelude..@? "TextOptions")
      Prelude.<*> (x Prelude..@? "IntArrayOptions")
      Prelude.<*> (x Prelude..@? "LiteralArrayOptions")
      Prelude.<*> (x Prelude..@? "DateOptions")
      Prelude.<*> (x Prelude..@? "IntOptions")
      Prelude.<*> (x Prelude..@? "LiteralOptions")
      Prelude.<*> (x Prelude..@ "IndexFieldName")
      Prelude.<*> (x Prelude..@ "IndexFieldType")

instance Prelude.Hashable IndexField

instance Prelude.NFData IndexField

instance Prelude.ToQuery IndexField where
  toQuery IndexField' {..} =
    Prelude.mconcat
      [ "DoubleArrayOptions" Prelude.=: doubleArrayOptions,
        "LatLonOptions" Prelude.=: latLonOptions,
        "TextArrayOptions" Prelude.=: textArrayOptions,
        "DateArrayOptions" Prelude.=: dateArrayOptions,
        "DoubleOptions" Prelude.=: doubleOptions,
        "TextOptions" Prelude.=: textOptions,
        "IntArrayOptions" Prelude.=: intArrayOptions,
        "LiteralArrayOptions" Prelude.=: literalArrayOptions,
        "DateOptions" Prelude.=: dateOptions,
        "IntOptions" Prelude.=: intOptions,
        "LiteralOptions" Prelude.=: literalOptions,
        "IndexFieldName" Prelude.=: indexFieldName,
        "IndexFieldType" Prelude.=: indexFieldType
      ]
