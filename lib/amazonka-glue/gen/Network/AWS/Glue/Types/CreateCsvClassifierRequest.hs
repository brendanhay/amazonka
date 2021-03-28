{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateCsvClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CreateCsvClassifierRequest
  ( CreateCsvClassifierRequest (..)
  -- * Smart constructor
  , mkCreateCsvClassifierRequest
  -- * Lenses
  , cccrName
  , cccrAllowSingleColumn
  , cccrContainsHeader
  , cccrDelimiter
  , cccrDisableValueTrimming
  , cccrHeader
  , cccrQuoteSymbol
  ) where

import qualified Network.AWS.Glue.Types.CsvColumnDelimiter as Types
import qualified Network.AWS.Glue.Types.CsvHeaderOption as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.QuoteSymbol as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a custom CSV classifier for @CreateClassifier@ to create.
--
-- /See:/ 'mkCreateCsvClassifierRequest' smart constructor.
data CreateCsvClassifierRequest = CreateCsvClassifierRequest'
  { name :: Types.Name
    -- ^ The name of the classifier.
  , allowSingleColumn :: Core.Maybe Core.Bool
    -- ^ Enables the processing of files that contain only one column.
  , containsHeader :: Core.Maybe Types.CsvHeaderOption
    -- ^ Indicates whether the CSV file contains a header.
  , delimiter :: Core.Maybe Types.CsvColumnDelimiter
    -- ^ A custom symbol to denote what separates each column entry in the row.
  , disableValueTrimming :: Core.Maybe Core.Bool
    -- ^ Specifies not to trim values before identifying the type of column values. The default value is true.
  , header :: Core.Maybe [Types.NameString]
    -- ^ A list of strings representing column names.
  , quoteSymbol :: Core.Maybe Types.QuoteSymbol
    -- ^ A custom symbol to denote what combines content into a single column value. Must be different from the column delimiter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCsvClassifierRequest' value with any optional fields omitted.
mkCreateCsvClassifierRequest
    :: Types.Name -- ^ 'name'
    -> CreateCsvClassifierRequest
mkCreateCsvClassifierRequest name
  = CreateCsvClassifierRequest'{name,
                                allowSingleColumn = Core.Nothing, containsHeader = Core.Nothing,
                                delimiter = Core.Nothing, disableValueTrimming = Core.Nothing,
                                header = Core.Nothing, quoteSymbol = Core.Nothing}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrName :: Lens.Lens' CreateCsvClassifierRequest Types.Name
cccrName = Lens.field @"name"
{-# INLINEABLE cccrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Enables the processing of files that contain only one column.
--
-- /Note:/ Consider using 'allowSingleColumn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrAllowSingleColumn :: Lens.Lens' CreateCsvClassifierRequest (Core.Maybe Core.Bool)
cccrAllowSingleColumn = Lens.field @"allowSingleColumn"
{-# INLINEABLE cccrAllowSingleColumn #-}
{-# DEPRECATED allowSingleColumn "Use generic-lens or generic-optics with 'allowSingleColumn' instead"  #-}

-- | Indicates whether the CSV file contains a header.
--
-- /Note:/ Consider using 'containsHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrContainsHeader :: Lens.Lens' CreateCsvClassifierRequest (Core.Maybe Types.CsvHeaderOption)
cccrContainsHeader = Lens.field @"containsHeader"
{-# INLINEABLE cccrContainsHeader #-}
{-# DEPRECATED containsHeader "Use generic-lens or generic-optics with 'containsHeader' instead"  #-}

-- | A custom symbol to denote what separates each column entry in the row.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrDelimiter :: Lens.Lens' CreateCsvClassifierRequest (Core.Maybe Types.CsvColumnDelimiter)
cccrDelimiter = Lens.field @"delimiter"
{-# INLINEABLE cccrDelimiter #-}
{-# DEPRECATED delimiter "Use generic-lens or generic-optics with 'delimiter' instead"  #-}

-- | Specifies not to trim values before identifying the type of column values. The default value is true.
--
-- /Note:/ Consider using 'disableValueTrimming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrDisableValueTrimming :: Lens.Lens' CreateCsvClassifierRequest (Core.Maybe Core.Bool)
cccrDisableValueTrimming = Lens.field @"disableValueTrimming"
{-# INLINEABLE cccrDisableValueTrimming #-}
{-# DEPRECATED disableValueTrimming "Use generic-lens or generic-optics with 'disableValueTrimming' instead"  #-}

-- | A list of strings representing column names.
--
-- /Note:/ Consider using 'header' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrHeader :: Lens.Lens' CreateCsvClassifierRequest (Core.Maybe [Types.NameString])
cccrHeader = Lens.field @"header"
{-# INLINEABLE cccrHeader #-}
{-# DEPRECATED header "Use generic-lens or generic-optics with 'header' instead"  #-}

-- | A custom symbol to denote what combines content into a single column value. Must be different from the column delimiter.
--
-- /Note:/ Consider using 'quoteSymbol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrQuoteSymbol :: Lens.Lens' CreateCsvClassifierRequest (Core.Maybe Types.QuoteSymbol)
cccrQuoteSymbol = Lens.field @"quoteSymbol"
{-# INLINEABLE cccrQuoteSymbol #-}
{-# DEPRECATED quoteSymbol "Use generic-lens or generic-optics with 'quoteSymbol' instead"  #-}

instance Core.FromJSON CreateCsvClassifierRequest where
        toJSON CreateCsvClassifierRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("AllowSingleColumn" Core..=) Core.<$> allowSingleColumn,
                  ("ContainsHeader" Core..=) Core.<$> containsHeader,
                  ("Delimiter" Core..=) Core.<$> delimiter,
                  ("DisableValueTrimming" Core..=) Core.<$> disableValueTrimming,
                  ("Header" Core..=) Core.<$> header,
                  ("QuoteSymbol" Core..=) Core.<$> quoteSymbol])
