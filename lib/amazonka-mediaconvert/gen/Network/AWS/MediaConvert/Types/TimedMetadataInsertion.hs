{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimedMetadataInsertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimedMetadataInsertion
  ( TimedMetadataInsertion (..),

    -- * Smart constructor
    mkTimedMetadataInsertion,

    -- * Lenses
    tmiId3Insertions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Id3Insertion
import qualified Network.AWS.Prelude as Lude

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /See:/ 'mkTimedMetadataInsertion' smart constructor.
newtype TimedMetadataInsertion = TimedMetadataInsertion'
  { id3Insertions ::
      Lude.Maybe [Id3Insertion]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimedMetadataInsertion' with the minimum fields required to make a request.
--
-- * 'id3Insertions' - Id3Insertions contains the array of Id3Insertion instances.
mkTimedMetadataInsertion ::
  TimedMetadataInsertion
mkTimedMetadataInsertion =
  TimedMetadataInsertion' {id3Insertions = Lude.Nothing}

-- | Id3Insertions contains the array of Id3Insertion instances.
--
-- /Note:/ Consider using 'id3Insertions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmiId3Insertions :: Lens.Lens' TimedMetadataInsertion (Lude.Maybe [Id3Insertion])
tmiId3Insertions = Lens.lens (id3Insertions :: TimedMetadataInsertion -> Lude.Maybe [Id3Insertion]) (\s a -> s {id3Insertions = a} :: TimedMetadataInsertion)
{-# DEPRECATED tmiId3Insertions "Use generic-lens or generic-optics with 'id3Insertions' instead." #-}

instance Lude.FromJSON TimedMetadataInsertion where
  parseJSON =
    Lude.withObject
      "TimedMetadataInsertion"
      ( \x ->
          TimedMetadataInsertion'
            Lude.<$> (x Lude..:? "id3Insertions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TimedMetadataInsertion where
  toJSON TimedMetadataInsertion' {..} =
    Lude.object
      (Lude.catMaybes [("id3Insertions" Lude..=) Lude.<$> id3Insertions])
