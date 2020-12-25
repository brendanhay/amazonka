{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateXMLClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateXMLClassifierRequest
  ( UpdateXMLClassifierRequest (..),

    -- * Smart constructor
    mkUpdateXMLClassifierRequest,

    -- * Lenses
    uxmlcrName,
    uxmlcrClassification,
    uxmlcrRowTag,
  )
where

import qualified Network.AWS.Glue.Types.Classification as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.RowTag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an XML classifier to be updated.
--
-- /See:/ 'mkUpdateXMLClassifierRequest' smart constructor.
data UpdateXMLClassifierRequest = UpdateXMLClassifierRequest'
  { -- | The name of the classifier.
    name :: Types.Name,
    -- | An identifier of the data format that the classifier matches.
    classification :: Core.Maybe Types.Classification,
    -- | The XML tag designating the element that contains each record in an XML document being parsed. This cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
    rowTag :: Core.Maybe Types.RowTag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateXMLClassifierRequest' value with any optional fields omitted.
mkUpdateXMLClassifierRequest ::
  -- | 'name'
  Types.Name ->
  UpdateXMLClassifierRequest
mkUpdateXMLClassifierRequest name =
  UpdateXMLClassifierRequest'
    { name,
      classification = Core.Nothing,
      rowTag = Core.Nothing
    }

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmlcrName :: Lens.Lens' UpdateXMLClassifierRequest Types.Name
uxmlcrName = Lens.field @"name"
{-# DEPRECATED uxmlcrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An identifier of the data format that the classifier matches.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmlcrClassification :: Lens.Lens' UpdateXMLClassifierRequest (Core.Maybe Types.Classification)
uxmlcrClassification = Lens.field @"classification"
{-# DEPRECATED uxmlcrClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The XML tag designating the element that contains each record in an XML document being parsed. This cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- /Note:/ Consider using 'rowTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmlcrRowTag :: Lens.Lens' UpdateXMLClassifierRequest (Core.Maybe Types.RowTag)
uxmlcrRowTag = Lens.field @"rowTag"
{-# DEPRECATED uxmlcrRowTag "Use generic-lens or generic-optics with 'rowTag' instead." #-}

instance Core.FromJSON UpdateXMLClassifierRequest where
  toJSON UpdateXMLClassifierRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Classification" Core..=) Core.<$> classification,
            ("RowTag" Core..=) Core.<$> rowTag
          ]
      )
