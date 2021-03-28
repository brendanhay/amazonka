{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Id3Insertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Id3Insertion
  ( Id3Insertion (..)
  -- * Smart constructor
  , mkId3Insertion
  -- * Lenses
  , iiId3
  , iiTimecode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create multiple instances of ID3 insertion (Id3Insertion).
--
-- /See:/ 'mkId3Insertion' smart constructor.
data Id3Insertion = Id3Insertion'
  { id3 :: Core.Maybe Core.Text
    -- ^ Use ID3 tag (Id3) to provide a tag value in base64-encode format.
  , timecode :: Core.Maybe Core.Text
    -- ^ Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Id3Insertion' value with any optional fields omitted.
mkId3Insertion
    :: Id3Insertion
mkId3Insertion
  = Id3Insertion'{id3 = Core.Nothing, timecode = Core.Nothing}

-- | Use ID3 tag (Id3) to provide a tag value in base64-encode format.
--
-- /Note:/ Consider using 'id3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiId3 :: Lens.Lens' Id3Insertion (Core.Maybe Core.Text)
iiId3 = Lens.field @"id3"
{-# INLINEABLE iiId3 #-}
{-# DEPRECATED id3 "Use generic-lens or generic-optics with 'id3' instead"  #-}

-- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
--
-- /Note:/ Consider using 'timecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiTimecode :: Lens.Lens' Id3Insertion (Core.Maybe Core.Text)
iiTimecode = Lens.field @"timecode"
{-# INLINEABLE iiTimecode #-}
{-# DEPRECATED timecode "Use generic-lens or generic-optics with 'timecode' instead"  #-}

instance Core.FromJSON Id3Insertion where
        toJSON Id3Insertion{..}
          = Core.object
              (Core.catMaybes
                 [("id3" Core..=) Core.<$> id3,
                  ("timecode" Core..=) Core.<$> timecode])

instance Core.FromJSON Id3Insertion where
        parseJSON
          = Core.withObject "Id3Insertion" Core.$
              \ x ->
                Id3Insertion' Core.<$>
                  (x Core..:? "id3") Core.<*> x Core..:? "timecode"
