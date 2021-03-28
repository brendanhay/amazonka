{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CsvClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CsvClassifier
  ( CsvClassifier (..)
  -- * Smart constructor
  , mkCsvClassifier
  -- * Lenses
  , ccName
  , ccAllowSingleColumn
  , ccContainsHeader
  , ccCreationTime
  , ccDelimiter
  , ccDisableValueTrimming
  , ccHeader
  , ccLastUpdated
  , ccQuoteSymbol
  , ccVersion
  ) where

import qualified Network.AWS.Glue.Types.CsvColumnDelimiter as Types
import qualified Network.AWS.Glue.Types.CsvHeaderOption as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.QuoteSymbol as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A classifier for custom @CSV@ content.
--
-- /See:/ 'mkCsvClassifier' smart constructor.
data CsvClassifier = CsvClassifier'
  { name :: Types.Name
    -- ^ The name of the classifier.
  , allowSingleColumn :: Core.Maybe Core.Bool
    -- ^ Enables the processing of files that contain only one column.
  , containsHeader :: Core.Maybe Types.CsvHeaderOption
    -- ^ Indicates whether the CSV file contains a header.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that this classifier was registered.
  , delimiter :: Core.Maybe Types.CsvColumnDelimiter
    -- ^ A custom symbol to denote what separates each column entry in the row.
  , disableValueTrimming :: Core.Maybe Core.Bool
    -- ^ Specifies not to trim values before identifying the type of column values. The default value is @true@ .
  , header :: Core.Maybe [Types.NameString]
    -- ^ A list of strings representing column names.
  , lastUpdated :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that this classifier was last updated.
  , quoteSymbol :: Core.Maybe Types.QuoteSymbol
    -- ^ A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
  , version :: Core.Maybe Core.Integer
    -- ^ The version of this classifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CsvClassifier' value with any optional fields omitted.
mkCsvClassifier
    :: Types.Name -- ^ 'name'
    -> CsvClassifier
mkCsvClassifier name
  = CsvClassifier'{name, allowSingleColumn = Core.Nothing,
                   containsHeader = Core.Nothing, creationTime = Core.Nothing,
                   delimiter = Core.Nothing, disableValueTrimming = Core.Nothing,
                   header = Core.Nothing, lastUpdated = Core.Nothing,
                   quoteSymbol = Core.Nothing, version = Core.Nothing}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccName :: Lens.Lens' CsvClassifier Types.Name
ccName = Lens.field @"name"
{-# INLINEABLE ccName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Enables the processing of files that contain only one column.
--
-- /Note:/ Consider using 'allowSingleColumn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAllowSingleColumn :: Lens.Lens' CsvClassifier (Core.Maybe Core.Bool)
ccAllowSingleColumn = Lens.field @"allowSingleColumn"
{-# INLINEABLE ccAllowSingleColumn #-}
{-# DEPRECATED allowSingleColumn "Use generic-lens or generic-optics with 'allowSingleColumn' instead"  #-}

-- | Indicates whether the CSV file contains a header.
--
-- /Note:/ Consider using 'containsHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccContainsHeader :: Lens.Lens' CsvClassifier (Core.Maybe Types.CsvHeaderOption)
ccContainsHeader = Lens.field @"containsHeader"
{-# INLINEABLE ccContainsHeader #-}
{-# DEPRECATED containsHeader "Use generic-lens or generic-optics with 'containsHeader' instead"  #-}

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCreationTime :: Lens.Lens' CsvClassifier (Core.Maybe Core.NominalDiffTime)
ccCreationTime = Lens.field @"creationTime"
{-# INLINEABLE ccCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | A custom symbol to denote what separates each column entry in the row.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDelimiter :: Lens.Lens' CsvClassifier (Core.Maybe Types.CsvColumnDelimiter)
ccDelimiter = Lens.field @"delimiter"
{-# INLINEABLE ccDelimiter #-}
{-# DEPRECATED delimiter "Use generic-lens or generic-optics with 'delimiter' instead"  #-}

-- | Specifies not to trim values before identifying the type of column values. The default value is @true@ .
--
-- /Note:/ Consider using 'disableValueTrimming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDisableValueTrimming :: Lens.Lens' CsvClassifier (Core.Maybe Core.Bool)
ccDisableValueTrimming = Lens.field @"disableValueTrimming"
{-# INLINEABLE ccDisableValueTrimming #-}
{-# DEPRECATED disableValueTrimming "Use generic-lens or generic-optics with 'disableValueTrimming' instead"  #-}

-- | A list of strings representing column names.
--
-- /Note:/ Consider using 'header' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHeader :: Lens.Lens' CsvClassifier (Core.Maybe [Types.NameString])
ccHeader = Lens.field @"header"
{-# INLINEABLE ccHeader #-}
{-# DEPRECATED header "Use generic-lens or generic-optics with 'header' instead"  #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLastUpdated :: Lens.Lens' CsvClassifier (Core.Maybe Core.NominalDiffTime)
ccLastUpdated = Lens.field @"lastUpdated"
{-# INLINEABLE ccLastUpdated #-}
{-# DEPRECATED lastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead"  #-}

-- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
--
-- /Note:/ Consider using 'quoteSymbol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccQuoteSymbol :: Lens.Lens' CsvClassifier (Core.Maybe Types.QuoteSymbol)
ccQuoteSymbol = Lens.field @"quoteSymbol"
{-# INLINEABLE ccQuoteSymbol #-}
{-# DEPRECATED quoteSymbol "Use generic-lens or generic-optics with 'quoteSymbol' instead"  #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVersion :: Lens.Lens' CsvClassifier (Core.Maybe Core.Integer)
ccVersion = Lens.field @"version"
{-# INLINEABLE ccVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON CsvClassifier where
        parseJSON
          = Core.withObject "CsvClassifier" Core.$
              \ x ->
                CsvClassifier' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..:? "AllowSingleColumn" Core.<*>
                    x Core..:? "ContainsHeader"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "Delimiter"
                    Core.<*> x Core..:? "DisableValueTrimming"
                    Core.<*> x Core..:? "Header"
                    Core.<*> x Core..:? "LastUpdated"
                    Core.<*> x Core..:? "QuoteSymbol"
                    Core.<*> x Core..:? "Version"
