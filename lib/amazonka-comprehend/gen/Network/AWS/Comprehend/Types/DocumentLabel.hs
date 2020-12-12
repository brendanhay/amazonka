{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentLabel
  ( DocumentLabel (..),

    -- * Smart constructor
    mkDocumentLabel,

    -- * Lenses
    dScore,
    dName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies one of the label or labels that categorize the document being analyzed.
--
-- /See:/ 'mkDocumentLabel' smart constructor.
data DocumentLabel = DocumentLabel'
  { score ::
      Lude.Maybe Lude.Double,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentLabel' with the minimum fields required to make a request.
--
-- * 'name' - The name of the label.
-- * 'score' - The confidence score that Amazon Comprehend has this label correctly attributed.
mkDocumentLabel ::
  DocumentLabel
mkDocumentLabel =
  DocumentLabel' {score = Lude.Nothing, name = Lude.Nothing}

-- | The confidence score that Amazon Comprehend has this label correctly attributed.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScore :: Lens.Lens' DocumentLabel (Lude.Maybe Lude.Double)
dScore = Lens.lens (score :: DocumentLabel -> Lude.Maybe Lude.Double) (\s a -> s {score = a} :: DocumentLabel)
{-# DEPRECATED dScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | The name of the label.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DocumentLabel (Lude.Maybe Lude.Text)
dName = Lens.lens (name :: DocumentLabel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentLabel)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON DocumentLabel where
  parseJSON =
    Lude.withObject
      "DocumentLabel"
      ( \x ->
          DocumentLabel'
            Lude.<$> (x Lude..:? "Score") Lude.<*> (x Lude..:? "Name")
      )
