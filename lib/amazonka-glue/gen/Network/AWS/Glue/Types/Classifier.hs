{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cCsvClassifier,
    cGrokClassifier,
    cJsonClassifier,
    cXMLClassifier,
  )
where

import qualified Network.AWS.Glue.Types.CsvClassifier as Types
import qualified Network.AWS.Glue.Types.GrokClassifier as Types
import qualified Network.AWS.Glue.Types.JsonClassifier as Types
import qualified Network.AWS.Glue.Types.XMLClassifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Classifiers are triggered during a crawl task. A classifier checks whether a given file is in a format it can handle. If it is, the classifier creates a schema in the form of a @StructType@ object that matches that data format.
--
-- You can use the standard classifiers that AWS Glue provides, or you can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier can be a @grok@ classifier, an @XML@ classifier, a @JSON@ classifier, or a custom @CSV@ classifier, as specified in one of the fields in the @Classifier@ object.
--
-- /See:/ 'mkClassifier' smart constructor.
data Classifier = Classifier'
  { -- | A classifier for comma-separated values (CSV).
    csvClassifier :: Core.Maybe Types.CsvClassifier,
    -- | A classifier that uses @grok@ .
    grokClassifier :: Core.Maybe Types.GrokClassifier,
    -- | A classifier for JSON content.
    jsonClassifier :: Core.Maybe Types.JsonClassifier,
    -- | A classifier for XML content.
    xMLClassifier :: Core.Maybe Types.XMLClassifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Classifier' value with any optional fields omitted.
mkClassifier ::
  Classifier
mkClassifier =
  Classifier'
    { csvClassifier = Core.Nothing,
      grokClassifier = Core.Nothing,
      jsonClassifier = Core.Nothing,
      xMLClassifier = Core.Nothing
    }

-- | A classifier for comma-separated values (CSV).
--
-- /Note:/ Consider using 'csvClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCsvClassifier :: Lens.Lens' Classifier (Core.Maybe Types.CsvClassifier)
cCsvClassifier = Lens.field @"csvClassifier"
{-# DEPRECATED cCsvClassifier "Use generic-lens or generic-optics with 'csvClassifier' instead." #-}

-- | A classifier that uses @grok@ .
--
-- /Note:/ Consider using 'grokClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGrokClassifier :: Lens.Lens' Classifier (Core.Maybe Types.GrokClassifier)
cGrokClassifier = Lens.field @"grokClassifier"
{-# DEPRECATED cGrokClassifier "Use generic-lens or generic-optics with 'grokClassifier' instead." #-}

-- | A classifier for JSON content.
--
-- /Note:/ Consider using 'jsonClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJsonClassifier :: Lens.Lens' Classifier (Core.Maybe Types.JsonClassifier)
cJsonClassifier = Lens.field @"jsonClassifier"
{-# DEPRECATED cJsonClassifier "Use generic-lens or generic-optics with 'jsonClassifier' instead." #-}

-- | A classifier for XML content.
--
-- /Note:/ Consider using 'xMLClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cXMLClassifier :: Lens.Lens' Classifier (Core.Maybe Types.XMLClassifier)
cXMLClassifier = Lens.field @"xMLClassifier"
{-# DEPRECATED cXMLClassifier "Use generic-lens or generic-optics with 'xMLClassifier' instead." #-}

instance Core.FromJSON Classifier where
  parseJSON =
    Core.withObject "Classifier" Core.$
      \x ->
        Classifier'
          Core.<$> (x Core..:? "CsvClassifier")
          Core.<*> (x Core..:? "GrokClassifier")
          Core.<*> (x Core..:? "JsonClassifier")
          Core.<*> (x Core..:? "XMLClassifier")
