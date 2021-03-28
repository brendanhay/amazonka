{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateCsvClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.UpdateCsvClassifierRequest
  ( UpdateCsvClassifierRequest (..)
  -- * Smart constructor
  , mkUpdateCsvClassifierRequest
  -- * Lenses
  , uccrName
  , uccrAllowSingleColumn
  , uccrContainsHeader
  , uccrDelimiter
  , uccrDisableValueTrimming
  , uccrHeader
  , uccrQuoteSymbol
  ) where

import qualified Network.AWS.Glue.Types.CsvColumnDelimiter as Types
import qualified Network.AWS.Glue.Types.CsvHeaderOption as Types
import qualified Network.AWS.Glue.Types.CsvQuoteSymbol as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a custom CSV classifier to be updated.
--
-- /See:/ 'mkUpdateCsvClassifierRequest' smart constructor.
data UpdateCsvClassifierRequest = UpdateCsvClassifierRequest'
  { name :: Types.NameString
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
  , quoteSymbol :: Core.Maybe Types.CsvQuoteSymbol
    -- ^ A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCsvClassifierRequest' value with any optional fields omitted.
mkUpdateCsvClassifierRequest
    :: Types.NameString -- ^ 'name'
    -> UpdateCsvClassifierRequest
mkUpdateCsvClassifierRequest name
  = UpdateCsvClassifierRequest'{name,
                                allowSingleColumn = Core.Nothing, containsHeader = Core.Nothing,
                                delimiter = Core.Nothing, disableValueTrimming = Core.Nothing,
                                header = Core.Nothing, quoteSymbol = Core.Nothing}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrName :: Lens.Lens' UpdateCsvClassifierRequest Types.NameString
uccrName = Lens.field @"name"
{-# INLINEABLE uccrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Enables the processing of files that contain only one column.
--
-- /Note:/ Consider using 'allowSingleColumn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrAllowSingleColumn :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Core.Bool)
uccrAllowSingleColumn = Lens.field @"allowSingleColumn"
{-# INLINEABLE uccrAllowSingleColumn #-}
{-# DEPRECATED allowSingleColumn "Use generic-lens or generic-optics with 'allowSingleColumn' instead"  #-}

-- | Indicates whether the CSV file contains a header.
--
-- /Note:/ Consider using 'containsHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrContainsHeader :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Types.CsvHeaderOption)
uccrContainsHeader = Lens.field @"containsHeader"
{-# INLINEABLE uccrContainsHeader #-}
{-# DEPRECATED containsHeader "Use generic-lens or generic-optics with 'containsHeader' instead"  #-}

-- | A custom symbol to denote what separates each column entry in the row.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrDelimiter :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Types.CsvColumnDelimiter)
uccrDelimiter = Lens.field @"delimiter"
{-# INLINEABLE uccrDelimiter #-}
{-# DEPRECATED delimiter "Use generic-lens or generic-optics with 'delimiter' instead"  #-}

-- | Specifies not to trim values before identifying the type of column values. The default value is true.
--
-- /Note:/ Consider using 'disableValueTrimming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrDisableValueTrimming :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Core.Bool)
uccrDisableValueTrimming = Lens.field @"disableValueTrimming"
{-# INLINEABLE uccrDisableValueTrimming #-}
{-# DEPRECATED disableValueTrimming "Use generic-lens or generic-optics with 'disableValueTrimming' instead"  #-}

-- | A list of strings representing column names.
--
-- /Note:/ Consider using 'header' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrHeader :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe [Types.NameString])
uccrHeader = Lens.field @"header"
{-# INLINEABLE uccrHeader #-}
{-# DEPRECATED header "Use generic-lens or generic-optics with 'header' instead"  #-}

-- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
--
-- /Note:/ Consider using 'quoteSymbol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrQuoteSymbol :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Types.CsvQuoteSymbol)
uccrQuoteSymbol = Lens.field @"quoteSymbol"
{-# INLINEABLE uccrQuoteSymbol #-}
{-# DEPRECATED quoteSymbol "Use generic-lens or generic-optics with 'quoteSymbol' instead"  #-}

instance Core.FromJSON UpdateCsvClassifierRequest where
        toJSON UpdateCsvClassifierRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("AllowSingleColumn" Core..=) Core.<$> allowSingleColumn,
                  ("ContainsHeader" Core..=) Core.<$> containsHeader,
                  ("Delimiter" Core..=) Core.<$> delimiter,
                  ("DisableValueTrimming" Core..=) Core.<$> disableValueTrimming,
                  ("Header" Core..=) Core.<$> header,
                  ("QuoteSymbol" Core..=) Core.<$> quoteSymbol])
