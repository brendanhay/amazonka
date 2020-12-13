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
    uxcrClassification,
    uxcrName,
    uxcrRowTag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an XML classifier to be updated.
--
-- /See:/ 'mkUpdateXMLClassifierRequest' smart constructor.
data UpdateXMLClassifierRequest = UpdateXMLClassifierRequest'
  { -- | An identifier of the data format that the classifier matches.
    classification :: Lude.Maybe Lude.Text,
    -- | The name of the classifier.
    name :: Lude.Text,
    -- | The XML tag designating the element that contains each record in an XML document being parsed. This cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
    rowTag :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateXMLClassifierRequest' with the minimum fields required to make a request.
--
-- * 'classification' - An identifier of the data format that the classifier matches.
-- * 'name' - The name of the classifier.
-- * 'rowTag' - The XML tag designating the element that contains each record in an XML document being parsed. This cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
mkUpdateXMLClassifierRequest ::
  -- | 'name'
  Lude.Text ->
  UpdateXMLClassifierRequest
mkUpdateXMLClassifierRequest pName_ =
  UpdateXMLClassifierRequest'
    { classification = Lude.Nothing,
      name = pName_,
      rowTag = Lude.Nothing
    }

-- | An identifier of the data format that the classifier matches.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxcrClassification :: Lens.Lens' UpdateXMLClassifierRequest (Lude.Maybe Lude.Text)
uxcrClassification = Lens.lens (classification :: UpdateXMLClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {classification = a} :: UpdateXMLClassifierRequest)
{-# DEPRECATED uxcrClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxcrName :: Lens.Lens' UpdateXMLClassifierRequest Lude.Text
uxcrName = Lens.lens (name :: UpdateXMLClassifierRequest -> Lude.Text) (\s a -> s {name = a} :: UpdateXMLClassifierRequest)
{-# DEPRECATED uxcrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The XML tag designating the element that contains each record in an XML document being parsed. This cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- /Note:/ Consider using 'rowTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxcrRowTag :: Lens.Lens' UpdateXMLClassifierRequest (Lude.Maybe Lude.Text)
uxcrRowTag = Lens.lens (rowTag :: UpdateXMLClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {rowTag = a} :: UpdateXMLClassifierRequest)
{-# DEPRECATED uxcrRowTag "Use generic-lens or generic-optics with 'rowTag' instead." #-}

instance Lude.ToJSON UpdateXMLClassifierRequest where
  toJSON UpdateXMLClassifierRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Classification" Lude..=) Lude.<$> classification,
            Lude.Just ("Name" Lude..= name),
            ("RowTag" Lude..=) Lude.<$> rowTag
          ]
      )
