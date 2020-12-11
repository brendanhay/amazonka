-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CSVClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CSVClassifier
  ( CSVClassifier (..),

    -- * Smart constructor
    mkCSVClassifier,

    -- * Lenses
    csvcCreationTime,
    csvcQuoteSymbol,
    csvcContainsHeader,
    csvcLastUpdated,
    csvcDisableValueTrimming,
    csvcHeader,
    csvcVersion,
    csvcAllowSingleColumn,
    csvcDelimiter,
    csvcName,
  )
where

import Network.AWS.Glue.Types.CSVHeaderOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A classifier for custom @CSV@ content.
--
-- /See:/ 'mkCSVClassifier' smart constructor.
data CSVClassifier = CSVClassifier'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    quoteSymbol :: Lude.Maybe Lude.Text,
    containsHeader :: Lude.Maybe CSVHeaderOption,
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    disableValueTrimming :: Lude.Maybe Lude.Bool,
    header :: Lude.Maybe [Lude.Text],
    version :: Lude.Maybe Lude.Integer,
    allowSingleColumn :: Lude.Maybe Lude.Bool,
    delimiter :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CSVClassifier' with the minimum fields required to make a request.
--
-- * 'allowSingleColumn' - Enables the processing of files that contain only one column.
-- * 'containsHeader' - Indicates whether the CSV file contains a header.
-- * 'creationTime' - The time that this classifier was registered.
-- * 'delimiter' - A custom symbol to denote what separates each column entry in the row.
-- * 'disableValueTrimming' - Specifies not to trim values before identifying the type of column values. The default value is @true@ .
-- * 'header' - A list of strings representing column names.
-- * 'lastUpdated' - The time that this classifier was last updated.
-- * 'name' - The name of the classifier.
-- * 'quoteSymbol' - A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
-- * 'version' - The version of this classifier.
mkCSVClassifier ::
  -- | 'name'
  Lude.Text ->
  CSVClassifier
mkCSVClassifier pName_ =
  CSVClassifier'
    { creationTime = Lude.Nothing,
      quoteSymbol = Lude.Nothing,
      containsHeader = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      disableValueTrimming = Lude.Nothing,
      header = Lude.Nothing,
      version = Lude.Nothing,
      allowSingleColumn = Lude.Nothing,
      delimiter = Lude.Nothing,
      name = pName_
    }

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcCreationTime :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Timestamp)
csvcCreationTime = Lens.lens (creationTime :: CSVClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: CSVClassifier)
{-# DEPRECATED csvcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
--
-- /Note:/ Consider using 'quoteSymbol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcQuoteSymbol :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Text)
csvcQuoteSymbol = Lens.lens (quoteSymbol :: CSVClassifier -> Lude.Maybe Lude.Text) (\s a -> s {quoteSymbol = a} :: CSVClassifier)
{-# DEPRECATED csvcQuoteSymbol "Use generic-lens or generic-optics with 'quoteSymbol' instead." #-}

-- | Indicates whether the CSV file contains a header.
--
-- /Note:/ Consider using 'containsHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcContainsHeader :: Lens.Lens' CSVClassifier (Lude.Maybe CSVHeaderOption)
csvcContainsHeader = Lens.lens (containsHeader :: CSVClassifier -> Lude.Maybe CSVHeaderOption) (\s a -> s {containsHeader = a} :: CSVClassifier)
{-# DEPRECATED csvcContainsHeader "Use generic-lens or generic-optics with 'containsHeader' instead." #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcLastUpdated :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Timestamp)
csvcLastUpdated = Lens.lens (lastUpdated :: CSVClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: CSVClassifier)
{-# DEPRECATED csvcLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | Specifies not to trim values before identifying the type of column values. The default value is @true@ .
--
-- /Note:/ Consider using 'disableValueTrimming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcDisableValueTrimming :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Bool)
csvcDisableValueTrimming = Lens.lens (disableValueTrimming :: CSVClassifier -> Lude.Maybe Lude.Bool) (\s a -> s {disableValueTrimming = a} :: CSVClassifier)
{-# DEPRECATED csvcDisableValueTrimming "Use generic-lens or generic-optics with 'disableValueTrimming' instead." #-}

-- | A list of strings representing column names.
--
-- /Note:/ Consider using 'header' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcHeader :: Lens.Lens' CSVClassifier (Lude.Maybe [Lude.Text])
csvcHeader = Lens.lens (header :: CSVClassifier -> Lude.Maybe [Lude.Text]) (\s a -> s {header = a} :: CSVClassifier)
{-# DEPRECATED csvcHeader "Use generic-lens or generic-optics with 'header' instead." #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcVersion :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Integer)
csvcVersion = Lens.lens (version :: CSVClassifier -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: CSVClassifier)
{-# DEPRECATED csvcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Enables the processing of files that contain only one column.
--
-- /Note:/ Consider using 'allowSingleColumn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcAllowSingleColumn :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Bool)
csvcAllowSingleColumn = Lens.lens (allowSingleColumn :: CSVClassifier -> Lude.Maybe Lude.Bool) (\s a -> s {allowSingleColumn = a} :: CSVClassifier)
{-# DEPRECATED csvcAllowSingleColumn "Use generic-lens or generic-optics with 'allowSingleColumn' instead." #-}

-- | A custom symbol to denote what separates each column entry in the row.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcDelimiter :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Text)
csvcDelimiter = Lens.lens (delimiter :: CSVClassifier -> Lude.Maybe Lude.Text) (\s a -> s {delimiter = a} :: CSVClassifier)
{-# DEPRECATED csvcDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvcName :: Lens.Lens' CSVClassifier Lude.Text
csvcName = Lens.lens (name :: CSVClassifier -> Lude.Text) (\s a -> s {name = a} :: CSVClassifier)
{-# DEPRECATED csvcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CSVClassifier where
  parseJSON =
    Lude.withObject
      "CSVClassifier"
      ( \x ->
          CSVClassifier'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "QuoteSymbol")
            Lude.<*> (x Lude..:? "ContainsHeader")
            Lude.<*> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..:? "DisableValueTrimming")
            Lude.<*> (x Lude..:? "Header" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "AllowSingleColumn")
            Lude.<*> (x Lude..:? "Delimiter")
            Lude.<*> (x Lude..: "Name")
      )
