{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClass
  ( DocumentClass (..),

    -- * Smart constructor
    mkDocumentClass,

    -- * Lenses
    dcScore,
    dcName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the class that categorizes the document being analyzed
--
-- /See:/ 'mkDocumentClass' smart constructor.
data DocumentClass = DocumentClass'
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

-- | Creates a value of 'DocumentClass' with the minimum fields required to make a request.
--
-- * 'name' - The name of the class.
-- * 'score' - The confidence score that Amazon Comprehend has this class correctly attributed.
mkDocumentClass ::
  DocumentClass
mkDocumentClass =
  DocumentClass' {score = Lude.Nothing, name = Lude.Nothing}

-- | The confidence score that Amazon Comprehend has this class correctly attributed.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcScore :: Lens.Lens' DocumentClass (Lude.Maybe Lude.Double)
dcScore = Lens.lens (score :: DocumentClass -> Lude.Maybe Lude.Double) (\s a -> s {score = a} :: DocumentClass)
{-# DEPRECATED dcScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | The name of the class.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DocumentClass (Lude.Maybe Lude.Text)
dcName = Lens.lens (name :: DocumentClass -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentClass)
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON DocumentClass where
  parseJSON =
    Lude.withObject
      "DocumentClass"
      ( \x ->
          DocumentClass'
            Lude.<$> (x Lude..:? "Score") Lude.<*> (x Lude..:? "Name")
      )
