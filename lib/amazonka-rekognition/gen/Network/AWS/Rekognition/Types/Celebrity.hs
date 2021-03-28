{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Celebrity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Celebrity
  ( Celebrity (..)
  -- * Smart constructor
  , mkCelebrity
  -- * Lenses
  , cFace
  , cId
  , cMatchConfidence
  , cName
  , cUrls
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.ComparedFace as Types
import qualified Network.AWS.Rekognition.Types.Id as Types
import qualified Network.AWS.Rekognition.Types.Url as Types

-- | Provides information about a celebrity recognized by the 'RecognizeCelebrities' operation.
--
-- /See:/ 'mkCelebrity' smart constructor.
data Celebrity = Celebrity'
  { face :: Core.Maybe Types.ComparedFace
    -- ^ Provides information about the celebrity's face, such as its location on the image.
  , id :: Core.Maybe Types.Id
    -- ^ A unique identifier for the celebrity. 
  , matchConfidence :: Core.Maybe Core.Double
    -- ^ The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the celebrity.
  , urls :: Core.Maybe [Types.Url]
    -- ^ An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Celebrity' value with any optional fields omitted.
mkCelebrity
    :: Celebrity
mkCelebrity
  = Celebrity'{face = Core.Nothing, id = Core.Nothing,
               matchConfidence = Core.Nothing, name = Core.Nothing,
               urls = Core.Nothing}

-- | Provides information about the celebrity's face, such as its location on the image.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cFace :: Lens.Lens' Celebrity (Core.Maybe Types.ComparedFace)
cFace = Lens.field @"face"
{-# INLINEABLE cFace #-}
{-# DEPRECATED face "Use generic-lens or generic-optics with 'face' instead"  #-}

-- | A unique identifier for the celebrity. 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Celebrity (Core.Maybe Types.Id)
cId = Lens.field @"id"
{-# INLINEABLE cId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
--
-- /Note:/ Consider using 'matchConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMatchConfidence :: Lens.Lens' Celebrity (Core.Maybe Core.Double)
cMatchConfidence = Lens.field @"matchConfidence"
{-# INLINEABLE cMatchConfidence #-}
{-# DEPRECATED matchConfidence "Use generic-lens or generic-optics with 'matchConfidence' instead"  #-}

-- | The name of the celebrity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Celebrity (Core.Maybe Core.Text)
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
--
-- /Note:/ Consider using 'urls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cUrls :: Lens.Lens' Celebrity (Core.Maybe [Types.Url])
cUrls = Lens.field @"urls"
{-# INLINEABLE cUrls #-}
{-# DEPRECATED urls "Use generic-lens or generic-optics with 'urls' instead"  #-}

instance Core.FromJSON Celebrity where
        parseJSON
          = Core.withObject "Celebrity" Core.$
              \ x ->
                Celebrity' Core.<$>
                  (x Core..:? "Face") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "MatchConfidence"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Urls"
