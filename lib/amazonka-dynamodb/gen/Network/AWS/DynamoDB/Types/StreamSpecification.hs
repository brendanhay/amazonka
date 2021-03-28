{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.StreamSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.StreamSpecification
  ( StreamSpecification (..)
  -- * Smart constructor
  , mkStreamSpecification
  -- * Lenses
  , ssStreamEnabled
  , ssStreamViewType
  ) where

import qualified Network.AWS.DynamoDB.Types.StreamViewType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the DynamoDB Streams configuration for a table in DynamoDB.
--
-- /See:/ 'mkStreamSpecification' smart constructor.
data StreamSpecification = StreamSpecification'
  { streamEnabled :: Core.Bool
    -- ^ Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
  , streamViewType :: Core.Maybe Types.StreamViewType
    -- ^ When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:
--
--
--     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.
--
--
--     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.
--
--
--     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.
--
--
--     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamSpecification' value with any optional fields omitted.
mkStreamSpecification
    :: Core.Bool -- ^ 'streamEnabled'
    -> StreamSpecification
mkStreamSpecification streamEnabled
  = StreamSpecification'{streamEnabled,
                         streamViewType = Core.Nothing}

-- | Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
--
-- /Note:/ Consider using 'streamEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamEnabled :: Lens.Lens' StreamSpecification Core.Bool
ssStreamEnabled = Lens.field @"streamEnabled"
{-# INLINEABLE ssStreamEnabled #-}
{-# DEPRECATED streamEnabled "Use generic-lens or generic-optics with 'streamEnabled' instead"  #-}

-- | When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:
--
--
--     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.
--
--
--     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.
--
--
--     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.
--
--
--     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
--
--
--
-- /Note:/ Consider using 'streamViewType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamViewType :: Lens.Lens' StreamSpecification (Core.Maybe Types.StreamViewType)
ssStreamViewType = Lens.field @"streamViewType"
{-# INLINEABLE ssStreamViewType #-}
{-# DEPRECATED streamViewType "Use generic-lens or generic-optics with 'streamViewType' instead"  #-}

instance Core.FromJSON StreamSpecification where
        toJSON StreamSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamEnabled" Core..= streamEnabled),
                  ("StreamViewType" Core..=) Core.<$> streamViewType])

instance Core.FromJSON StreamSpecification where
        parseJSON
          = Core.withObject "StreamSpecification" Core.$
              \ x ->
                StreamSpecification' Core.<$>
                  (x Core..: "StreamEnabled") Core.<*> x Core..:? "StreamViewType"
