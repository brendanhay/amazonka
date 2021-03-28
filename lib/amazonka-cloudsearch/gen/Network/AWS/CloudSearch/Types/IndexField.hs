{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.IndexField
  ( IndexField (..)
  -- * Smart constructor
  , mkIndexField
  -- * Lenses
  , ifIndexFieldName
  , ifIndexFieldType
  , ifDateArrayOptions
  , ifDateOptions
  , ifDoubleArrayOptions
  , ifDoubleOptions
  , ifIntArrayOptions
  , ifIntOptions
  , ifLatLonOptions
  , ifLiteralArrayOptions
  , ifLiteralOptions
  , ifTextArrayOptions
  , ifTextOptions
  ) where

import qualified Network.AWS.CloudSearch.Types.DateArrayOptions as Types
import qualified Network.AWS.CloudSearch.Types.DateOptions as Types
import qualified Network.AWS.CloudSearch.Types.DoubleArrayOptions as Types
import qualified Network.AWS.CloudSearch.Types.DoubleOptions as Types
import qualified Network.AWS.CloudSearch.Types.IndexFieldName as Types
import qualified Network.AWS.CloudSearch.Types.IndexFieldType as Types
import qualified Network.AWS.CloudSearch.Types.IntArrayOptions as Types
import qualified Network.AWS.CloudSearch.Types.IntOptions as Types
import qualified Network.AWS.CloudSearch.Types.LatLonOptions as Types
import qualified Network.AWS.CloudSearch.Types.LiteralArrayOptions as Types
import qualified Network.AWS.CloudSearch.Types.LiteralOptions as Types
import qualified Network.AWS.CloudSearch.Types.TextArrayOptions as Types
import qualified Network.AWS.CloudSearch.Types.TextOptions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information for a field in the index, including its name, type, and options. The supported options depend on the @'IndexFieldType' @ .
--
-- /See:/ 'mkIndexField' smart constructor.
data IndexField = IndexField'
  { indexFieldName :: Types.IndexFieldName
    -- ^ A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options. 
--
-- Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported. 
-- The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ . 
  , indexFieldType :: Types.IndexFieldType
  , dateArrayOptions :: Core.Maybe Types.DateArrayOptions
  , dateOptions :: Core.Maybe Types.DateOptions
  , doubleArrayOptions :: Core.Maybe Types.DoubleArrayOptions
  , doubleOptions :: Core.Maybe Types.DoubleOptions
  , intArrayOptions :: Core.Maybe Types.IntArrayOptions
  , intOptions :: Core.Maybe Types.IntOptions
  , latLonOptions :: Core.Maybe Types.LatLonOptions
  , literalArrayOptions :: Core.Maybe Types.LiteralArrayOptions
  , literalOptions :: Core.Maybe Types.LiteralOptions
  , textArrayOptions :: Core.Maybe Types.TextArrayOptions
  , textOptions :: Core.Maybe Types.TextOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IndexField' value with any optional fields omitted.
mkIndexField
    :: Types.IndexFieldName -- ^ 'indexFieldName'
    -> Types.IndexFieldType -- ^ 'indexFieldType'
    -> IndexField
mkIndexField indexFieldName indexFieldType
  = IndexField'{indexFieldName, indexFieldType,
                dateArrayOptions = Core.Nothing, dateOptions = Core.Nothing,
                doubleArrayOptions = Core.Nothing, doubleOptions = Core.Nothing,
                intArrayOptions = Core.Nothing, intOptions = Core.Nothing,
                latLonOptions = Core.Nothing, literalArrayOptions = Core.Nothing,
                literalOptions = Core.Nothing, textArrayOptions = Core.Nothing,
                textOptions = Core.Nothing}

-- | A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options. 
--
-- Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported. 
-- The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ . 
--
-- /Note:/ Consider using 'indexFieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIndexFieldName :: Lens.Lens' IndexField Types.IndexFieldName
ifIndexFieldName = Lens.field @"indexFieldName"
{-# INLINEABLE ifIndexFieldName #-}
{-# DEPRECATED indexFieldName "Use generic-lens or generic-optics with 'indexFieldName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'indexFieldType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIndexFieldType :: Lens.Lens' IndexField Types.IndexFieldType
ifIndexFieldType = Lens.field @"indexFieldType"
{-# INLINEABLE ifIndexFieldType #-}
{-# DEPRECATED indexFieldType "Use generic-lens or generic-optics with 'indexFieldType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dateArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDateArrayOptions :: Lens.Lens' IndexField (Core.Maybe Types.DateArrayOptions)
ifDateArrayOptions = Lens.field @"dateArrayOptions"
{-# INLINEABLE ifDateArrayOptions #-}
{-# DEPRECATED dateArrayOptions "Use generic-lens or generic-optics with 'dateArrayOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dateOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDateOptions :: Lens.Lens' IndexField (Core.Maybe Types.DateOptions)
ifDateOptions = Lens.field @"dateOptions"
{-# INLINEABLE ifDateOptions #-}
{-# DEPRECATED dateOptions "Use generic-lens or generic-optics with 'dateOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'doubleArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDoubleArrayOptions :: Lens.Lens' IndexField (Core.Maybe Types.DoubleArrayOptions)
ifDoubleArrayOptions = Lens.field @"doubleArrayOptions"
{-# INLINEABLE ifDoubleArrayOptions #-}
{-# DEPRECATED doubleArrayOptions "Use generic-lens or generic-optics with 'doubleArrayOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'doubleOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDoubleOptions :: Lens.Lens' IndexField (Core.Maybe Types.DoubleOptions)
ifDoubleOptions = Lens.field @"doubleOptions"
{-# INLINEABLE ifDoubleOptions #-}
{-# DEPRECATED doubleOptions "Use generic-lens or generic-optics with 'doubleOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'intArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIntArrayOptions :: Lens.Lens' IndexField (Core.Maybe Types.IntArrayOptions)
ifIntArrayOptions = Lens.field @"intArrayOptions"
{-# INLINEABLE ifIntArrayOptions #-}
{-# DEPRECATED intArrayOptions "Use generic-lens or generic-optics with 'intArrayOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'intOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIntOptions :: Lens.Lens' IndexField (Core.Maybe Types.IntOptions)
ifIntOptions = Lens.field @"intOptions"
{-# INLINEABLE ifIntOptions #-}
{-# DEPRECATED intOptions "Use generic-lens or generic-optics with 'intOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'latLonOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLatLonOptions :: Lens.Lens' IndexField (Core.Maybe Types.LatLonOptions)
ifLatLonOptions = Lens.field @"latLonOptions"
{-# INLINEABLE ifLatLonOptions #-}
{-# DEPRECATED latLonOptions "Use generic-lens or generic-optics with 'latLonOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'literalArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLiteralArrayOptions :: Lens.Lens' IndexField (Core.Maybe Types.LiteralArrayOptions)
ifLiteralArrayOptions = Lens.field @"literalArrayOptions"
{-# INLINEABLE ifLiteralArrayOptions #-}
{-# DEPRECATED literalArrayOptions "Use generic-lens or generic-optics with 'literalArrayOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'literalOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLiteralOptions :: Lens.Lens' IndexField (Core.Maybe Types.LiteralOptions)
ifLiteralOptions = Lens.field @"literalOptions"
{-# INLINEABLE ifLiteralOptions #-}
{-# DEPRECATED literalOptions "Use generic-lens or generic-optics with 'literalOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'textArrayOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTextArrayOptions :: Lens.Lens' IndexField (Core.Maybe Types.TextArrayOptions)
ifTextArrayOptions = Lens.field @"textArrayOptions"
{-# INLINEABLE ifTextArrayOptions #-}
{-# DEPRECATED textArrayOptions "Use generic-lens or generic-optics with 'textArrayOptions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'textOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTextOptions :: Lens.Lens' IndexField (Core.Maybe Types.TextOptions)
ifTextOptions = Lens.field @"textOptions"
{-# INLINEABLE ifTextOptions #-}
{-# DEPRECATED textOptions "Use generic-lens or generic-optics with 'textOptions' instead"  #-}

instance Core.ToQuery IndexField where
        toQuery IndexField{..}
          = Core.toQueryPair "IndexFieldName" indexFieldName Core.<>
              Core.toQueryPair "IndexFieldType" indexFieldType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DateArrayOptions")
                dateArrayOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DateOptions") dateOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DoubleArrayOptions")
                doubleArrayOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DoubleOptions")
                doubleOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IntArrayOptions")
                intArrayOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IntOptions") intOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LatLonOptions")
                latLonOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LiteralArrayOptions")
                literalArrayOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LiteralOptions")
                literalOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TextArrayOptions")
                textArrayOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TextOptions") textOptions

instance Core.FromXML IndexField where
        parseXML x
          = IndexField' Core.<$>
              (x Core..@ "IndexFieldName") Core.<*> x Core..@ "IndexFieldType"
                Core.<*> x Core..@? "DateArrayOptions"
                Core.<*> x Core..@? "DateOptions"
                Core.<*> x Core..@? "DoubleArrayOptions"
                Core.<*> x Core..@? "DoubleOptions"
                Core.<*> x Core..@? "IntArrayOptions"
                Core.<*> x Core..@? "IntOptions"
                Core.<*> x Core..@? "LatLonOptions"
                Core.<*> x Core..@? "LiteralArrayOptions"
                Core.<*> x Core..@? "LiteralOptions"
                Core.<*> x Core..@? "TextArrayOptions"
                Core.<*> x Core..@? "TextOptions"
