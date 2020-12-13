{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ccCreationTime,
    ccQuoteSymbol,
    ccContainsHeader,
    ccLastUpdated,
    ccDisableValueTrimming,
    ccName,
    ccHeader,
    ccVersion,
    ccAllowSingleColumn,
    ccDelimiter,
  )
where

import Network.AWS.Glue.Types.CSVHeaderOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A classifier for custom @CSV@ content.
--
-- /See:/ 'mkCSVClassifier' smart constructor.
data CSVClassifier = CSVClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
    quoteSymbol :: Lude.Maybe Lude.Text,
    -- | Indicates whether the CSV file contains a header.
    containsHeader :: Lude.Maybe CSVHeaderOption,
    -- | The time that this classifier was last updated.
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    -- | Specifies not to trim values before identifying the type of column values. The default value is @true@ .
    disableValueTrimming :: Lude.Maybe Lude.Bool,
    -- | The name of the classifier.
    name :: Lude.Text,
    -- | A list of strings representing column names.
    header :: Lude.Maybe [Lude.Text],
    -- | The version of this classifier.
    version :: Lude.Maybe Lude.Integer,
    -- | Enables the processing of files that contain only one column.
    allowSingleColumn :: Lude.Maybe Lude.Bool,
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CSVClassifier' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time that this classifier was registered.
-- * 'quoteSymbol' - A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
-- * 'containsHeader' - Indicates whether the CSV file contains a header.
-- * 'lastUpdated' - The time that this classifier was last updated.
-- * 'disableValueTrimming' - Specifies not to trim values before identifying the type of column values. The default value is @true@ .
-- * 'name' - The name of the classifier.
-- * 'header' - A list of strings representing column names.
-- * 'version' - The version of this classifier.
-- * 'allowSingleColumn' - Enables the processing of files that contain only one column.
-- * 'delimiter' - A custom symbol to denote what separates each column entry in the row.
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
      name = pName_,
      header = Lude.Nothing,
      version = Lude.Nothing,
      allowSingleColumn = Lude.Nothing,
      delimiter = Lude.Nothing
    }

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCreationTime :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Timestamp)
ccCreationTime = Lens.lens (creationTime :: CSVClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: CSVClassifier)
{-# DEPRECATED ccCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
--
-- /Note:/ Consider using 'quoteSymbol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccQuoteSymbol :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Text)
ccQuoteSymbol = Lens.lens (quoteSymbol :: CSVClassifier -> Lude.Maybe Lude.Text) (\s a -> s {quoteSymbol = a} :: CSVClassifier)
{-# DEPRECATED ccQuoteSymbol "Use generic-lens or generic-optics with 'quoteSymbol' instead." #-}

-- | Indicates whether the CSV file contains a header.
--
-- /Note:/ Consider using 'containsHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccContainsHeader :: Lens.Lens' CSVClassifier (Lude.Maybe CSVHeaderOption)
ccContainsHeader = Lens.lens (containsHeader :: CSVClassifier -> Lude.Maybe CSVHeaderOption) (\s a -> s {containsHeader = a} :: CSVClassifier)
{-# DEPRECATED ccContainsHeader "Use generic-lens or generic-optics with 'containsHeader' instead." #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLastUpdated :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Timestamp)
ccLastUpdated = Lens.lens (lastUpdated :: CSVClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: CSVClassifier)
{-# DEPRECATED ccLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | Specifies not to trim values before identifying the type of column values. The default value is @true@ .
--
-- /Note:/ Consider using 'disableValueTrimming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDisableValueTrimming :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Bool)
ccDisableValueTrimming = Lens.lens (disableValueTrimming :: CSVClassifier -> Lude.Maybe Lude.Bool) (\s a -> s {disableValueTrimming = a} :: CSVClassifier)
{-# DEPRECATED ccDisableValueTrimming "Use generic-lens or generic-optics with 'disableValueTrimming' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccName :: Lens.Lens' CSVClassifier Lude.Text
ccName = Lens.lens (name :: CSVClassifier -> Lude.Text) (\s a -> s {name = a} :: CSVClassifier)
{-# DEPRECATED ccName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of strings representing column names.
--
-- /Note:/ Consider using 'header' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHeader :: Lens.Lens' CSVClassifier (Lude.Maybe [Lude.Text])
ccHeader = Lens.lens (header :: CSVClassifier -> Lude.Maybe [Lude.Text]) (\s a -> s {header = a} :: CSVClassifier)
{-# DEPRECATED ccHeader "Use generic-lens or generic-optics with 'header' instead." #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVersion :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Integer)
ccVersion = Lens.lens (version :: CSVClassifier -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: CSVClassifier)
{-# DEPRECATED ccVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Enables the processing of files that contain only one column.
--
-- /Note:/ Consider using 'allowSingleColumn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAllowSingleColumn :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Bool)
ccAllowSingleColumn = Lens.lens (allowSingleColumn :: CSVClassifier -> Lude.Maybe Lude.Bool) (\s a -> s {allowSingleColumn = a} :: CSVClassifier)
{-# DEPRECATED ccAllowSingleColumn "Use generic-lens or generic-optics with 'allowSingleColumn' instead." #-}

-- | A custom symbol to denote what separates each column entry in the row.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDelimiter :: Lens.Lens' CSVClassifier (Lude.Maybe Lude.Text)
ccDelimiter = Lens.lens (delimiter :: CSVClassifier -> Lude.Maybe Lude.Text) (\s a -> s {delimiter = a} :: CSVClassifier)
{-# DEPRECATED ccDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

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
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "Header" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "AllowSingleColumn")
            Lude.<*> (x Lude..:? "Delimiter")
      )
