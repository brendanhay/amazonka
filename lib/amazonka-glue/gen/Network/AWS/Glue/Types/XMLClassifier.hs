{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.XMLClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.XMLClassifier
  ( XMLClassifier (..)
  -- * Smart constructor
  , mkXMLClassifier
  -- * Lenses
  , xmlcName
  , xmlcClassification
  , xmlcCreationTime
  , xmlcLastUpdated
  , xmlcRowTag
  , xmlcVersion
  ) where

import qualified Network.AWS.Glue.Types.Classification as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.RowTag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A classifier for @XML@ content.
--
-- /See:/ 'mkXMLClassifier' smart constructor.
data XMLClassifier = XMLClassifier'
  { name :: Types.Name
    -- ^ The name of the classifier.
  , classification :: Types.Classification
    -- ^ An identifier of the data format that the classifier matches.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that this classifier was registered.
  , lastUpdated :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that this classifier was last updated.
  , rowTag :: Core.Maybe Types.RowTag
    -- ^ The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
  , version :: Core.Maybe Core.Integer
    -- ^ The version of this classifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'XMLClassifier' value with any optional fields omitted.
mkXMLClassifier
    :: Types.Name -- ^ 'name'
    -> Types.Classification -- ^ 'classification'
    -> XMLClassifier
mkXMLClassifier name classification
  = XMLClassifier'{name, classification, creationTime = Core.Nothing,
                   lastUpdated = Core.Nothing, rowTag = Core.Nothing,
                   version = Core.Nothing}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmlcName :: Lens.Lens' XMLClassifier Types.Name
xmlcName = Lens.field @"name"
{-# INLINEABLE xmlcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An identifier of the data format that the classifier matches.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmlcClassification :: Lens.Lens' XMLClassifier Types.Classification
xmlcClassification = Lens.field @"classification"
{-# INLINEABLE xmlcClassification #-}
{-# DEPRECATED classification "Use generic-lens or generic-optics with 'classification' instead"  #-}

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmlcCreationTime :: Lens.Lens' XMLClassifier (Core.Maybe Core.NominalDiffTime)
xmlcCreationTime = Lens.field @"creationTime"
{-# INLINEABLE xmlcCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmlcLastUpdated :: Lens.Lens' XMLClassifier (Core.Maybe Core.NominalDiffTime)
xmlcLastUpdated = Lens.field @"lastUpdated"
{-# INLINEABLE xmlcLastUpdated #-}
{-# DEPRECATED lastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead"  #-}

-- | The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- /Note:/ Consider using 'rowTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmlcRowTag :: Lens.Lens' XMLClassifier (Core.Maybe Types.RowTag)
xmlcRowTag = Lens.field @"rowTag"
{-# INLINEABLE xmlcRowTag #-}
{-# DEPRECATED rowTag "Use generic-lens or generic-optics with 'rowTag' instead"  #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmlcVersion :: Lens.Lens' XMLClassifier (Core.Maybe Core.Integer)
xmlcVersion = Lens.field @"version"
{-# INLINEABLE xmlcVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON XMLClassifier where
        parseJSON
          = Core.withObject "XMLClassifier" Core.$
              \ x ->
                XMLClassifier' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Classification" Core.<*>
                    x Core..:? "CreationTime"
                    Core.<*> x Core..:? "LastUpdated"
                    Core.<*> x Core..:? "RowTag"
                    Core.<*> x Core..:? "Version"
