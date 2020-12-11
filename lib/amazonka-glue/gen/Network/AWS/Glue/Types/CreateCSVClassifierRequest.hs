-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateCSVClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateCSVClassifierRequest
  ( CreateCSVClassifierRequest (..),

    -- * Smart constructor
    mkCreateCSVClassifierRequest,

    -- * Lenses
    cccrQuoteSymbol,
    cccrContainsHeader,
    cccrDisableValueTrimming,
    cccrHeader,
    cccrAllowSingleColumn,
    cccrDelimiter,
    cccrName,
  )
where

import Network.AWS.Glue.Types.CSVHeaderOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a custom CSV classifier for @CreateClassifier@ to create.
--
-- /See:/ 'mkCreateCSVClassifierRequest' smart constructor.
data CreateCSVClassifierRequest = CreateCSVClassifierRequest'
  { quoteSymbol ::
      Lude.Maybe Lude.Text,
    containsHeader ::
      Lude.Maybe CSVHeaderOption,
    disableValueTrimming ::
      Lude.Maybe Lude.Bool,
    header :: Lude.Maybe [Lude.Text],
    allowSingleColumn ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'CreateCSVClassifierRequest' with the minimum fields required to make a request.
--
-- * 'allowSingleColumn' - Enables the processing of files that contain only one column.
-- * 'containsHeader' - Indicates whether the CSV file contains a header.
-- * 'delimiter' - A custom symbol to denote what separates each column entry in the row.
-- * 'disableValueTrimming' - Specifies not to trim values before identifying the type of column values. The default value is true.
-- * 'header' - A list of strings representing column names.
-- * 'name' - The name of the classifier.
-- * 'quoteSymbol' - A custom symbol to denote what combines content into a single column value. Must be different from the column delimiter.
mkCreateCSVClassifierRequest ::
  -- | 'name'
  Lude.Text ->
  CreateCSVClassifierRequest
mkCreateCSVClassifierRequest pName_ =
  CreateCSVClassifierRequest'
    { quoteSymbol = Lude.Nothing,
      containsHeader = Lude.Nothing,
      disableValueTrimming = Lude.Nothing,
      header = Lude.Nothing,
      allowSingleColumn = Lude.Nothing,
      delimiter = Lude.Nothing,
      name = pName_
    }

-- | A custom symbol to denote what combines content into a single column value. Must be different from the column delimiter.
--
-- /Note:/ Consider using 'quoteSymbol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrQuoteSymbol :: Lens.Lens' CreateCSVClassifierRequest (Lude.Maybe Lude.Text)
cccrQuoteSymbol = Lens.lens (quoteSymbol :: CreateCSVClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {quoteSymbol = a} :: CreateCSVClassifierRequest)
{-# DEPRECATED cccrQuoteSymbol "Use generic-lens or generic-optics with 'quoteSymbol' instead." #-}

-- | Indicates whether the CSV file contains a header.
--
-- /Note:/ Consider using 'containsHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrContainsHeader :: Lens.Lens' CreateCSVClassifierRequest (Lude.Maybe CSVHeaderOption)
cccrContainsHeader = Lens.lens (containsHeader :: CreateCSVClassifierRequest -> Lude.Maybe CSVHeaderOption) (\s a -> s {containsHeader = a} :: CreateCSVClassifierRequest)
{-# DEPRECATED cccrContainsHeader "Use generic-lens or generic-optics with 'containsHeader' instead." #-}

-- | Specifies not to trim values before identifying the type of column values. The default value is true.
--
-- /Note:/ Consider using 'disableValueTrimming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrDisableValueTrimming :: Lens.Lens' CreateCSVClassifierRequest (Lude.Maybe Lude.Bool)
cccrDisableValueTrimming = Lens.lens (disableValueTrimming :: CreateCSVClassifierRequest -> Lude.Maybe Lude.Bool) (\s a -> s {disableValueTrimming = a} :: CreateCSVClassifierRequest)
{-# DEPRECATED cccrDisableValueTrimming "Use generic-lens or generic-optics with 'disableValueTrimming' instead." #-}

-- | A list of strings representing column names.
--
-- /Note:/ Consider using 'header' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrHeader :: Lens.Lens' CreateCSVClassifierRequest (Lude.Maybe [Lude.Text])
cccrHeader = Lens.lens (header :: CreateCSVClassifierRequest -> Lude.Maybe [Lude.Text]) (\s a -> s {header = a} :: CreateCSVClassifierRequest)
{-# DEPRECATED cccrHeader "Use generic-lens or generic-optics with 'header' instead." #-}

-- | Enables the processing of files that contain only one column.
--
-- /Note:/ Consider using 'allowSingleColumn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrAllowSingleColumn :: Lens.Lens' CreateCSVClassifierRequest (Lude.Maybe Lude.Bool)
cccrAllowSingleColumn = Lens.lens (allowSingleColumn :: CreateCSVClassifierRequest -> Lude.Maybe Lude.Bool) (\s a -> s {allowSingleColumn = a} :: CreateCSVClassifierRequest)
{-# DEPRECATED cccrAllowSingleColumn "Use generic-lens or generic-optics with 'allowSingleColumn' instead." #-}

-- | A custom symbol to denote what separates each column entry in the row.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrDelimiter :: Lens.Lens' CreateCSVClassifierRequest (Lude.Maybe Lude.Text)
cccrDelimiter = Lens.lens (delimiter :: CreateCSVClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {delimiter = a} :: CreateCSVClassifierRequest)
{-# DEPRECATED cccrDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrName :: Lens.Lens' CreateCSVClassifierRequest Lude.Text
cccrName = Lens.lens (name :: CreateCSVClassifierRequest -> Lude.Text) (\s a -> s {name = a} :: CreateCSVClassifierRequest)
{-# DEPRECATED cccrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON CreateCSVClassifierRequest where
  toJSON CreateCSVClassifierRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("QuoteSymbol" Lude..=) Lude.<$> quoteSymbol,
            ("ContainsHeader" Lude..=) Lude.<$> containsHeader,
            ("DisableValueTrimming" Lude..=) Lude.<$> disableValueTrimming,
            ("Header" Lude..=) Lude.<$> header,
            ("AllowSingleColumn" Lude..=) Lude.<$> allowSingleColumn,
            ("Delimiter" Lude..=) Lude.<$> delimiter,
            Lude.Just ("Name" Lude..= name)
          ]
      )
