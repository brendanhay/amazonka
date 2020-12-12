{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
  ( FragmentSelector (..),

    -- * Smart constructor
    mkFragmentSelector,

    -- * Lenses
    fsFragmentSelectorType,
    fsTimestampRange,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { fragmentSelectorType ::
      FragmentSelectorType,
    timestampRange :: TimestampRange
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FragmentSelector' with the minimum fields required to make a request.
--
-- * 'fragmentSelectorType' - The origin of the timestamps to use (Server or Producer).
-- * 'timestampRange' - The range of timestamps to return.
mkFragmentSelector ::
  -- | 'fragmentSelectorType'
  FragmentSelectorType ->
  -- | 'timestampRange'
  TimestampRange ->
  FragmentSelector
mkFragmentSelector pFragmentSelectorType_ pTimestampRange_ =
  FragmentSelector'
    { fragmentSelectorType = pFragmentSelectorType_,
      timestampRange = pTimestampRange_
    }

-- | The origin of the timestamps to use (Server or Producer).
--
-- /Note:/ Consider using 'fragmentSelectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsFragmentSelectorType :: Lens.Lens' FragmentSelector FragmentSelectorType
fsFragmentSelectorType = Lens.lens (fragmentSelectorType :: FragmentSelector -> FragmentSelectorType) (\s a -> s {fragmentSelectorType = a} :: FragmentSelector)
{-# DEPRECATED fsFragmentSelectorType "Use generic-lens or generic-optics with 'fragmentSelectorType' instead." #-}

-- | The range of timestamps to return.
--
-- /Note:/ Consider using 'timestampRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsTimestampRange :: Lens.Lens' FragmentSelector TimestampRange
fsTimestampRange = Lens.lens (timestampRange :: FragmentSelector -> TimestampRange) (\s a -> s {timestampRange = a} :: FragmentSelector)
{-# DEPRECATED fsTimestampRange "Use generic-lens or generic-optics with 'timestampRange' instead." #-}

instance Lude.ToJSON FragmentSelector where
  toJSON FragmentSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FragmentSelectorType" Lude..= fragmentSelectorType),
            Lude.Just ("TimestampRange" Lude..= timestampRange)
          ]
      )
