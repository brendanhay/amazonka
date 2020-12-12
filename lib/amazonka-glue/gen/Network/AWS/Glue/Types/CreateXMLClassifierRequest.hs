{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateXMLClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateXMLClassifierRequest
  ( CreateXMLClassifierRequest (..),

    -- * Smart constructor
    mkCreateXMLClassifierRequest,

    -- * Lenses
    cxcrRowTag,
    cxcrClassification,
    cxcrName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an XML classifier for @CreateClassifier@ to create.
--
-- /See:/ 'mkCreateXMLClassifierRequest' smart constructor.
data CreateXMLClassifierRequest = CreateXMLClassifierRequest'
  { rowTag ::
      Lude.Maybe Lude.Text,
    classification :: Lude.Text,
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

-- | Creates a value of 'CreateXMLClassifierRequest' with the minimum fields required to make a request.
--
-- * 'classification' - An identifier of the data format that the classifier matches.
-- * 'name' - The name of the classifier.
-- * 'rowTag' - The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
mkCreateXMLClassifierRequest ::
  -- | 'classification'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateXMLClassifierRequest
mkCreateXMLClassifierRequest pClassification_ pName_ =
  CreateXMLClassifierRequest'
    { rowTag = Lude.Nothing,
      classification = pClassification_,
      name = pName_
    }

-- | The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- /Note:/ Consider using 'rowTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxcrRowTag :: Lens.Lens' CreateXMLClassifierRequest (Lude.Maybe Lude.Text)
cxcrRowTag = Lens.lens (rowTag :: CreateXMLClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {rowTag = a} :: CreateXMLClassifierRequest)
{-# DEPRECATED cxcrRowTag "Use generic-lens or generic-optics with 'rowTag' instead." #-}

-- | An identifier of the data format that the classifier matches.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxcrClassification :: Lens.Lens' CreateXMLClassifierRequest Lude.Text
cxcrClassification = Lens.lens (classification :: CreateXMLClassifierRequest -> Lude.Text) (\s a -> s {classification = a} :: CreateXMLClassifierRequest)
{-# DEPRECATED cxcrClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxcrName :: Lens.Lens' CreateXMLClassifierRequest Lude.Text
cxcrName = Lens.lens (name :: CreateXMLClassifierRequest -> Lude.Text) (\s a -> s {name = a} :: CreateXMLClassifierRequest)
{-# DEPRECATED cxcrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON CreateXMLClassifierRequest where
  toJSON CreateXMLClassifierRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RowTag" Lude..=) Lude.<$> rowTag,
            Lude.Just ("Classification" Lude..= classification),
            Lude.Just ("Name" Lude..= name)
          ]
      )
