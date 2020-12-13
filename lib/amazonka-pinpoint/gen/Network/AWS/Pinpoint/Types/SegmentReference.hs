{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentReference
  ( SegmentReference (..),

    -- * Smart constructor
    mkSegmentReference,

    -- * Lenses
    srVersion,
    srId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the segment identifier and version of a segment.
--
-- /See:/ 'mkSegmentReference' smart constructor.
data SegmentReference = SegmentReference'
  { -- | The version number of the segment.
    version :: Lude.Maybe Lude.Int,
    -- | The unique identifier for the segment.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentReference' with the minimum fields required to make a request.
--
-- * 'version' - The version number of the segment.
-- * 'id' - The unique identifier for the segment.
mkSegmentReference ::
  -- | 'id'
  Lude.Text ->
  SegmentReference
mkSegmentReference pId_ =
  SegmentReference' {version = Lude.Nothing, id = pId_}

-- | The version number of the segment.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srVersion :: Lens.Lens' SegmentReference (Lude.Maybe Lude.Int)
srVersion = Lens.lens (version :: SegmentReference -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: SegmentReference)
{-# DEPRECATED srVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srId :: Lens.Lens' SegmentReference Lude.Text
srId = Lens.lens (id :: SegmentReference -> Lude.Text) (\s a -> s {id = a} :: SegmentReference)
{-# DEPRECATED srId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON SegmentReference where
  parseJSON =
    Lude.withObject
      "SegmentReference"
      ( \x ->
          SegmentReference'
            Lude.<$> (x Lude..:? "Version") Lude.<*> (x Lude..: "Id")
      )

instance Lude.ToJSON SegmentReference where
  toJSON SegmentReference' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Version" Lude..=) Lude.<$> version,
            Lude.Just ("Id" Lude..= id)
          ]
      )
