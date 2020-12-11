-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
  ( HlsId3SegmentTaggingScheduleActionSettings (..),

    -- * Smart constructor
    mkHlsId3SegmentTaggingScheduleActionSettings,

    -- * Lenses
    histsasTag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the action to insert a user-defined ID3 tag in each HLS segment
--
-- /See:/ 'mkHlsId3SegmentTaggingScheduleActionSettings' smart constructor.
newtype HlsId3SegmentTaggingScheduleActionSettings = HlsId3SegmentTaggingScheduleActionSettings'
  { tag ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsId3SegmentTaggingScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'tag' - ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
mkHlsId3SegmentTaggingScheduleActionSettings ::
  -- | 'tag'
  Lude.Text ->
  HlsId3SegmentTaggingScheduleActionSettings
mkHlsId3SegmentTaggingScheduleActionSettings pTag_ =
  HlsId3SegmentTaggingScheduleActionSettings' {tag = pTag_}

-- | ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
histsasTag :: Lens.Lens' HlsId3SegmentTaggingScheduleActionSettings Lude.Text
histsasTag = Lens.lens (tag :: HlsId3SegmentTaggingScheduleActionSettings -> Lude.Text) (\s a -> s {tag = a} :: HlsId3SegmentTaggingScheduleActionSettings)
{-# DEPRECATED histsasTag "Use generic-lens or generic-optics with 'tag' instead." #-}

instance Lude.FromJSON HlsId3SegmentTaggingScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "HlsId3SegmentTaggingScheduleActionSettings"
      ( \x ->
          HlsId3SegmentTaggingScheduleActionSettings'
            Lude.<$> (x Lude..: "tag")
      )

instance Lude.ToJSON HlsId3SegmentTaggingScheduleActionSettings where
  toJSON HlsId3SegmentTaggingScheduleActionSettings' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("tag" Lude..= tag)])
