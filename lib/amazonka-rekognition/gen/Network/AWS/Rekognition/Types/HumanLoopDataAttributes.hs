-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.HumanLoopDataAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopDataAttributes
  ( HumanLoopDataAttributes (..),

    -- * Smart constructor
    mkHumanLoopDataAttributes,

    -- * Lenses
    hldaContentClassifiers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.ContentClassifier

-- | Allows you to set attributes of the image. Currently, you can declare an image as free of personally identifiable information.
--
-- /See:/ 'mkHumanLoopDataAttributes' smart constructor.
newtype HumanLoopDataAttributes = HumanLoopDataAttributes'
  { contentClassifiers ::
      Lude.Maybe [ContentClassifier]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HumanLoopDataAttributes' with the minimum fields required to make a request.
--
-- * 'contentClassifiers' - Sets whether the input image is free of personally identifiable information.
mkHumanLoopDataAttributes ::
  HumanLoopDataAttributes
mkHumanLoopDataAttributes =
  HumanLoopDataAttributes' {contentClassifiers = Lude.Nothing}

-- | Sets whether the input image is free of personally identifiable information.
--
-- /Note:/ Consider using 'contentClassifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hldaContentClassifiers :: Lens.Lens' HumanLoopDataAttributes (Lude.Maybe [ContentClassifier])
hldaContentClassifiers = Lens.lens (contentClassifiers :: HumanLoopDataAttributes -> Lude.Maybe [ContentClassifier]) (\s a -> s {contentClassifiers = a} :: HumanLoopDataAttributes)
{-# DEPRECATED hldaContentClassifiers "Use generic-lens or generic-optics with 'contentClassifiers' instead." #-}

instance Lude.ToJSON HumanLoopDataAttributes where
  toJSON HumanLoopDataAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [("ContentClassifiers" Lude..=) Lude.<$> contentClassifiers]
      )
