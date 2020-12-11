-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Classifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Classifier
  ( Classifier (..),

    -- * Smart constructor
    mkClassifier,

    -- * Lenses
    cGrokClassifier,
    cXMLClassifier,
    cCSVClassifier,
    cJSONClassifier,
  )
where

import Network.AWS.Glue.Types.CSVClassifier
import Network.AWS.Glue.Types.GrokClassifier
import Network.AWS.Glue.Types.JSONClassifier
import Network.AWS.Glue.Types.XMLClassifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Classifiers are triggered during a crawl task. A classifier checks whether a given file is in a format it can handle. If it is, the classifier creates a schema in the form of a @StructType@ object that matches that data format.
--
-- You can use the standard classifiers that AWS Glue provides, or you can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier can be a @grok@ classifier, an @XML@ classifier, a @JSON@ classifier, or a custom @CSV@ classifier, as specified in one of the fields in the @Classifier@ object.
--
-- /See:/ 'mkClassifier' smart constructor.
data Classifier = Classifier'
  { grokClassifier ::
      Lude.Maybe GrokClassifier,
    xmlClassifier :: Lude.Maybe XMLClassifier,
    csvClassifier :: Lude.Maybe CSVClassifier,
    jsonClassifier :: Lude.Maybe JSONClassifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Classifier' with the minimum fields required to make a request.
--
-- * 'csvClassifier' - A classifier for comma-separated values (CSV).
-- * 'grokClassifier' - A classifier that uses @grok@ .
-- * 'jsonClassifier' - A classifier for JSON content.
-- * 'xmlClassifier' - A classifier for XML content.
mkClassifier ::
  Classifier
mkClassifier =
  Classifier'
    { grokClassifier = Lude.Nothing,
      xmlClassifier = Lude.Nothing,
      csvClassifier = Lude.Nothing,
      jsonClassifier = Lude.Nothing
    }

-- | A classifier that uses @grok@ .
--
-- /Note:/ Consider using 'grokClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGrokClassifier :: Lens.Lens' Classifier (Lude.Maybe GrokClassifier)
cGrokClassifier = Lens.lens (grokClassifier :: Classifier -> Lude.Maybe GrokClassifier) (\s a -> s {grokClassifier = a} :: Classifier)
{-# DEPRECATED cGrokClassifier "Use generic-lens or generic-optics with 'grokClassifier' instead." #-}

-- | A classifier for XML content.
--
-- /Note:/ Consider using 'xmlClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cXMLClassifier :: Lens.Lens' Classifier (Lude.Maybe XMLClassifier)
cXMLClassifier = Lens.lens (xmlClassifier :: Classifier -> Lude.Maybe XMLClassifier) (\s a -> s {xmlClassifier = a} :: Classifier)
{-# DEPRECATED cXMLClassifier "Use generic-lens or generic-optics with 'xmlClassifier' instead." #-}

-- | A classifier for comma-separated values (CSV).
--
-- /Note:/ Consider using 'csvClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCSVClassifier :: Lens.Lens' Classifier (Lude.Maybe CSVClassifier)
cCSVClassifier = Lens.lens (csvClassifier :: Classifier -> Lude.Maybe CSVClassifier) (\s a -> s {csvClassifier = a} :: Classifier)
{-# DEPRECATED cCSVClassifier "Use generic-lens or generic-optics with 'csvClassifier' instead." #-}

-- | A classifier for JSON content.
--
-- /Note:/ Consider using 'jsonClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJSONClassifier :: Lens.Lens' Classifier (Lude.Maybe JSONClassifier)
cJSONClassifier = Lens.lens (jsonClassifier :: Classifier -> Lude.Maybe JSONClassifier) (\s a -> s {jsonClassifier = a} :: Classifier)
{-# DEPRECATED cJSONClassifier "Use generic-lens or generic-optics with 'jsonClassifier' instead." #-}

instance Lude.FromJSON Classifier where
  parseJSON =
    Lude.withObject
      "Classifier"
      ( \x ->
          Classifier'
            Lude.<$> (x Lude..:? "GrokClassifier")
            Lude.<*> (x Lude..:? "XMLClassifier")
            Lude.<*> (x Lude..:? "CsvClassifier")
            Lude.<*> (x Lude..:? "JsonClassifier")
      )
