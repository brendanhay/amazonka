{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateXMLClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CreateXMLClassifierRequest
  ( CreateXMLClassifierRequest (..)
  -- * Smart constructor
  , mkCreateXMLClassifierRequest
  -- * Lenses
  , cxmlcrClassification
  , cxmlcrName
  , cxmlcrRowTag
  ) where

import qualified Network.AWS.Glue.Types.Classification as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.RowTag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an XML classifier for @CreateClassifier@ to create.
--
-- /See:/ 'mkCreateXMLClassifierRequest' smart constructor.
data CreateXMLClassifierRequest = CreateXMLClassifierRequest'
  { classification :: Types.Classification
    -- ^ An identifier of the data format that the classifier matches.
  , name :: Types.NameString
    -- ^ The name of the classifier.
  , rowTag :: Core.Maybe Types.RowTag
    -- ^ The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateXMLClassifierRequest' value with any optional fields omitted.
mkCreateXMLClassifierRequest
    :: Types.Classification -- ^ 'classification'
    -> Types.NameString -- ^ 'name'
    -> CreateXMLClassifierRequest
mkCreateXMLClassifierRequest classification name
  = CreateXMLClassifierRequest'{classification, name,
                                rowTag = Core.Nothing}

-- | An identifier of the data format that the classifier matches.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmlcrClassification :: Lens.Lens' CreateXMLClassifierRequest Types.Classification
cxmlcrClassification = Lens.field @"classification"
{-# INLINEABLE cxmlcrClassification #-}
{-# DEPRECATED classification "Use generic-lens or generic-optics with 'classification' instead"  #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmlcrName :: Lens.Lens' CreateXMLClassifierRequest Types.NameString
cxmlcrName = Lens.field @"name"
{-# INLINEABLE cxmlcrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- /Note:/ Consider using 'rowTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmlcrRowTag :: Lens.Lens' CreateXMLClassifierRequest (Core.Maybe Types.RowTag)
cxmlcrRowTag = Lens.field @"rowTag"
{-# INLINEABLE cxmlcrRowTag #-}
{-# DEPRECATED rowTag "Use generic-lens or generic-optics with 'rowTag' instead"  #-}

instance Core.FromJSON CreateXMLClassifierRequest where
        toJSON CreateXMLClassifierRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Classification" Core..= classification),
                  Core.Just ("Name" Core..= name),
                  ("RowTag" Core..=) Core.<$> rowTag])
