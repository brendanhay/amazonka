{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
  ( ClipFragmentSelector (..),

    -- * Smart constructor
    mkClipFragmentSelector,

    -- * Lenses
    cfsFragmentSelectorType,
    cfsTimestampRange,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the timestamp range and timestamp origin of a range of fragments.
--
-- Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the clip will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned.
--
-- /See:/ 'mkClipFragmentSelector' smart constructor.
data ClipFragmentSelector = ClipFragmentSelector'
  { -- | The origin of the timestamps to use (Server or Producer).
    fragmentSelectorType :: ClipFragmentSelectorType,
    -- | The range of timestamps to return.
    timestampRange :: ClipTimestampRange
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClipFragmentSelector' with the minimum fields required to make a request.
--
-- * 'fragmentSelectorType' - The origin of the timestamps to use (Server or Producer).
-- * 'timestampRange' - The range of timestamps to return.
mkClipFragmentSelector ::
  -- | 'fragmentSelectorType'
  ClipFragmentSelectorType ->
  -- | 'timestampRange'
  ClipTimestampRange ->
  ClipFragmentSelector
mkClipFragmentSelector pFragmentSelectorType_ pTimestampRange_ =
  ClipFragmentSelector'
    { fragmentSelectorType =
        pFragmentSelectorType_,
      timestampRange = pTimestampRange_
    }

-- | The origin of the timestamps to use (Server or Producer).
--
-- /Note:/ Consider using 'fragmentSelectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsFragmentSelectorType :: Lens.Lens' ClipFragmentSelector ClipFragmentSelectorType
cfsFragmentSelectorType = Lens.lens (fragmentSelectorType :: ClipFragmentSelector -> ClipFragmentSelectorType) (\s a -> s {fragmentSelectorType = a} :: ClipFragmentSelector)
{-# DEPRECATED cfsFragmentSelectorType "Use generic-lens or generic-optics with 'fragmentSelectorType' instead." #-}

-- | The range of timestamps to return.
--
-- /Note:/ Consider using 'timestampRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsTimestampRange :: Lens.Lens' ClipFragmentSelector ClipTimestampRange
cfsTimestampRange = Lens.lens (timestampRange :: ClipFragmentSelector -> ClipTimestampRange) (\s a -> s {timestampRange = a} :: ClipFragmentSelector)
{-# DEPRECATED cfsTimestampRange "Use generic-lens or generic-optics with 'timestampRange' instead." #-}

instance Lude.ToJSON ClipFragmentSelector where
  toJSON ClipFragmentSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FragmentSelectorType" Lude..= fragmentSelectorType),
            Lude.Just ("TimestampRange" Lude..= timestampRange)
          ]
      )
