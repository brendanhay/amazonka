{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.ContentClassifier as Types

-- | Allows you to set attributes of the image. Currently, you can declare an image as free of personally identifiable information.
--
-- /See:/ 'mkHumanLoopDataAttributes' smart constructor.
newtype HumanLoopDataAttributes = HumanLoopDataAttributes'
  { -- | Sets whether the input image is free of personally identifiable information.
    contentClassifiers :: Core.Maybe [Types.ContentClassifier]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HumanLoopDataAttributes' value with any optional fields omitted.
mkHumanLoopDataAttributes ::
  HumanLoopDataAttributes
mkHumanLoopDataAttributes =
  HumanLoopDataAttributes' {contentClassifiers = Core.Nothing}

-- | Sets whether the input image is free of personally identifiable information.
--
-- /Note:/ Consider using 'contentClassifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hldaContentClassifiers :: Lens.Lens' HumanLoopDataAttributes (Core.Maybe [Types.ContentClassifier])
hldaContentClassifiers = Lens.field @"contentClassifiers"
{-# DEPRECATED hldaContentClassifiers "Use generic-lens or generic-optics with 'contentClassifiers' instead." #-}

instance Core.FromJSON HumanLoopDataAttributes where
  toJSON HumanLoopDataAttributes {..} =
    Core.object
      ( Core.catMaybes
          [("ContentClassifiers" Core..=) Core.<$> contentClassifiers]
      )
