{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IndexField
  ( IndexField (..),

    -- * Smart constructor
    mkIndexField,

    -- * Lenses
    ifDoubleArrayOptions,
    ifDateOptions,
    ifTextArrayOptions,
    ifDoubleOptions,
    ifTextOptions,
    ifIndexFieldType,
    ifLatLonOptions,
    ifLiteralArrayOptions,
    ifIntArrayOptions,
    ifDateArrayOptions,
    ifIntOptions,
    ifLiteralOptions,
    ifIndexFieldName,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for a field in the index, including its name, type, and options. The supported options depend on the @'IndexFieldType' @ .
--
-- /See:/ 'mkIndexField' smart constructor.
data IndexField = IndexField'
  { doubleArrayOptions :: Lude.Maybe DoubleArrayOptions,
    dateOptions :: Lude.Maybe DateOptions,
    textArrayOptions :: Lude.Maybe TextArrayOptions,
    doubleOptions :: Lude.Maybe DoubleOptions,
    textOptions :: Lude.Maybe TextOptions,
    indexFieldType :: IndexFieldType,
    latLonOptions :: Lude.Maybe LatLonOptions,
    literalArrayOptions :: Lude.Maybe LiteralArrayOptions,
    intArrayOptions :: Lude.Maybe IntArrayOptions,
    dateArrayOptions :: Lude.Maybe DateArrayOptions,
    intOptions :: Lude.Maybe IntOptions,
    literalOptions :: Lude.Maybe LiteralOptions,
    -- | A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options.
    --
    -- Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported.
    -- The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ .
    indexFieldName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IndexField' with the minimum fields required to make a request.
--
-- * 'doubleArrayOptions' -
-- * 'dateOptions' -
-- * 'textArrayOptions' -
-- * 'doubleOptions' -
-- * 'textOptions' -
-- * 'indexFieldType' -
-- * 'latLonOptions' -
-- * 'literalArrayOptions' -
-- * 'intArrayOptions' -
-- * 'dateArrayOptions' -
-- * 'intOptions' -
-- * 'literalOptions' -
-- * 'indexFieldName' - A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options.
--
-- Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported.
-- The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ .
mkIndexField ::
  -- | 'indexFieldType'
  IndexFieldType ->
  -- | 'indexFieldName'
  Lude.Text ->
  IndexField
mkIndexField pIndexFieldType_ pIndexFieldName_ =
  IndexField'
    { doubleArrayOptions = Lude.Nothing,
      dateOptions = Lude.Nothing,
      textArrayOptions = Lude.Nothing,
      doubleOptions = Lude.Nothing,
      textOptions = Lude.Nothing,
      indexFieldType = pIndexFieldType_,
      latLonOptions = Lude.Nothing,
      literalArrayOptions = Lude.Nothing,
      intArrayOptions = Lude.Nothing,
      dateArrayOptions = Lude.Nothing,
      intOptions = Lude.Nothing,
      literalOptions = Lude.Nothing,
      indexFieldName = pIndexFieldName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'doubleArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDoubleArrayOptions :: Lens.Lens' IndexField (Lude.Maybe DoubleArrayOptions)
ifDoubleArrayOptions = Lens.lens (doubleArrayOptions :: IndexField -> Lude.Maybe DoubleArrayOptions) (\s a -> s {doubleArrayOptions = a} :: IndexField)
{-# DEPRECATED ifDoubleArrayOptions "Use generic-lens or generic-optics with 'doubleArrayOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dateOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDateOptions :: Lens.Lens' IndexField (Lude.Maybe DateOptions)
ifDateOptions = Lens.lens (dateOptions :: IndexField -> Lude.Maybe DateOptions) (\s a -> s {dateOptions = a} :: IndexField)
{-# DEPRECATED ifDateOptions "Use generic-lens or generic-optics with 'dateOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'textArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTextArrayOptions :: Lens.Lens' IndexField (Lude.Maybe TextArrayOptions)
ifTextArrayOptions = Lens.lens (textArrayOptions :: IndexField -> Lude.Maybe TextArrayOptions) (\s a -> s {textArrayOptions = a} :: IndexField)
{-# DEPRECATED ifTextArrayOptions "Use generic-lens or generic-optics with 'textArrayOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'doubleOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDoubleOptions :: Lens.Lens' IndexField (Lude.Maybe DoubleOptions)
ifDoubleOptions = Lens.lens (doubleOptions :: IndexField -> Lude.Maybe DoubleOptions) (\s a -> s {doubleOptions = a} :: IndexField)
{-# DEPRECATED ifDoubleOptions "Use generic-lens or generic-optics with 'doubleOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'textOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTextOptions :: Lens.Lens' IndexField (Lude.Maybe TextOptions)
ifTextOptions = Lens.lens (textOptions :: IndexField -> Lude.Maybe TextOptions) (\s a -> s {textOptions = a} :: IndexField)
{-# DEPRECATED ifTextOptions "Use generic-lens or generic-optics with 'textOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'indexFieldType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIndexFieldType :: Lens.Lens' IndexField IndexFieldType
ifIndexFieldType = Lens.lens (indexFieldType :: IndexField -> IndexFieldType) (\s a -> s {indexFieldType = a} :: IndexField)
{-# DEPRECATED ifIndexFieldType "Use generic-lens or generic-optics with 'indexFieldType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'latLonOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLatLonOptions :: Lens.Lens' IndexField (Lude.Maybe LatLonOptions)
ifLatLonOptions = Lens.lens (latLonOptions :: IndexField -> Lude.Maybe LatLonOptions) (\s a -> s {latLonOptions = a} :: IndexField)
{-# DEPRECATED ifLatLonOptions "Use generic-lens or generic-optics with 'latLonOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'literalArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLiteralArrayOptions :: Lens.Lens' IndexField (Lude.Maybe LiteralArrayOptions)
ifLiteralArrayOptions = Lens.lens (literalArrayOptions :: IndexField -> Lude.Maybe LiteralArrayOptions) (\s a -> s {literalArrayOptions = a} :: IndexField)
{-# DEPRECATED ifLiteralArrayOptions "Use generic-lens or generic-optics with 'literalArrayOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'intArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIntArrayOptions :: Lens.Lens' IndexField (Lude.Maybe IntArrayOptions)
ifIntArrayOptions = Lens.lens (intArrayOptions :: IndexField -> Lude.Maybe IntArrayOptions) (\s a -> s {intArrayOptions = a} :: IndexField)
{-# DEPRECATED ifIntArrayOptions "Use generic-lens or generic-optics with 'intArrayOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dateArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDateArrayOptions :: Lens.Lens' IndexField (Lude.Maybe DateArrayOptions)
ifDateArrayOptions = Lens.lens (dateArrayOptions :: IndexField -> Lude.Maybe DateArrayOptions) (\s a -> s {dateArrayOptions = a} :: IndexField)
{-# DEPRECATED ifDateArrayOptions "Use generic-lens or generic-optics with 'dateArrayOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'intOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIntOptions :: Lens.Lens' IndexField (Lude.Maybe IntOptions)
ifIntOptions = Lens.lens (intOptions :: IndexField -> Lude.Maybe IntOptions) (\s a -> s {intOptions = a} :: IndexField)
{-# DEPRECATED ifIntOptions "Use generic-lens or generic-optics with 'intOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'literalOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLiteralOptions :: Lens.Lens' IndexField (Lude.Maybe LiteralOptions)
ifLiteralOptions = Lens.lens (literalOptions :: IndexField -> Lude.Maybe LiteralOptions) (\s a -> s {literalOptions = a} :: IndexField)
{-# DEPRECATED ifLiteralOptions "Use generic-lens or generic-optics with 'literalOptions' instead." #-}

-- | A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options.
--
-- Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported.
-- The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ .
--
-- /Note:/ Consider using 'indexFieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIndexFieldName :: Lens.Lens' IndexField Lude.Text
ifIndexFieldName = Lens.lens (indexFieldName :: IndexField -> Lude.Text) (\s a -> s {indexFieldName = a} :: IndexField)
{-# DEPRECATED ifIndexFieldName "Use generic-lens or generic-optics with 'indexFieldName' instead." #-}

instance Lude.FromXML IndexField where
  parseXML x =
    IndexField'
      Lude.<$> (x Lude..@? "DoubleArrayOptions")
      Lude.<*> (x Lude..@? "DateOptions")
      Lude.<*> (x Lude..@? "TextArrayOptions")
      Lude.<*> (x Lude..@? "DoubleOptions")
      Lude.<*> (x Lude..@? "TextOptions")
      Lude.<*> (x Lude..@ "IndexFieldType")
      Lude.<*> (x Lude..@? "LatLonOptions")
      Lude.<*> (x Lude..@? "LiteralArrayOptions")
      Lude.<*> (x Lude..@? "IntArrayOptions")
      Lude.<*> (x Lude..@? "DateArrayOptions")
      Lude.<*> (x Lude..@? "IntOptions")
      Lude.<*> (x Lude..@? "LiteralOptions")
      Lude.<*> (x Lude..@ "IndexFieldName")

instance Lude.ToQuery IndexField where
  toQuery IndexField' {..} =
    Lude.mconcat
      [ "DoubleArrayOptions" Lude.=: doubleArrayOptions,
        "DateOptions" Lude.=: dateOptions,
        "TextArrayOptions" Lude.=: textArrayOptions,
        "DoubleOptions" Lude.=: doubleOptions,
        "TextOptions" Lude.=: textOptions,
        "IndexFieldType" Lude.=: indexFieldType,
        "LatLonOptions" Lude.=: latLonOptions,
        "LiteralArrayOptions" Lude.=: literalArrayOptions,
        "IntArrayOptions" Lude.=: intArrayOptions,
        "DateArrayOptions" Lude.=: dateArrayOptions,
        "IntOptions" Lude.=: intOptions,
        "LiteralOptions" Lude.=: literalOptions,
        "IndexFieldName" Lude.=: indexFieldName
      ]
