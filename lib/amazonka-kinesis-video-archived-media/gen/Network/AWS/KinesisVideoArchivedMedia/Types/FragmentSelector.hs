{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
  ( FragmentSelector (..)
  -- * Smart constructor
  , mkFragmentSelector
  -- * Lenses
  , fsFragmentSelectorType
  , fsTimestampRange
  ) where

import qualified Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType as Types
import qualified Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the timestamp range and timestamp origin of a range of fragments.
--
-- Only fragments with a start timestamp greater than or equal to the given start time and less than or equal to the end time are returned. For example, if a stream contains fragments with the following start timestamps: 
--
--     * 00:00:00
--
--
--     * 00:00:02
--
--
--     * 00:00:04
--
--
--     * 00:00:06
--
--
-- A fragment selector range with a start time of 00:00:01 and end time of 00:00:04 would return the fragments with start times of 00:00:02 and 00:00:04. 
--
-- /See:/ 'mkFragmentSelector' smart constructor.
data FragmentSelector = FragmentSelector'
  { fragmentSelectorType :: Types.FragmentSelectorType
    -- ^ The origin of the timestamps to use (Server or Producer).
  , timestampRange :: Types.TimestampRange
    -- ^ The range of timestamps to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FragmentSelector' value with any optional fields omitted.
mkFragmentSelector
    :: Types.FragmentSelectorType -- ^ 'fragmentSelectorType'
    -> Types.TimestampRange -- ^ 'timestampRange'
    -> FragmentSelector
mkFragmentSelector fragmentSelectorType timestampRange
  = FragmentSelector'{fragmentSelectorType, timestampRange}

-- | The origin of the timestamps to use (Server or Producer).
--
-- /Note:/ Consider using 'fragmentSelectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsFragmentSelectorType :: Lens.Lens' FragmentSelector Types.FragmentSelectorType
fsFragmentSelectorType = Lens.field @"fragmentSelectorType"
{-# INLINEABLE fsFragmentSelectorType #-}
{-# DEPRECATED fragmentSelectorType "Use generic-lens or generic-optics with 'fragmentSelectorType' instead"  #-}

-- | The range of timestamps to return.
--
-- /Note:/ Consider using 'timestampRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsTimestampRange :: Lens.Lens' FragmentSelector Types.TimestampRange
fsTimestampRange = Lens.field @"timestampRange"
{-# INLINEABLE fsTimestampRange #-}
{-# DEPRECATED timestampRange "Use generic-lens or generic-optics with 'timestampRange' instead"  #-}

instance Core.FromJSON FragmentSelector where
        toJSON FragmentSelector{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FragmentSelectorType" Core..= fragmentSelectorType),
                  Core.Just ("TimestampRange" Core..= timestampRange)])
