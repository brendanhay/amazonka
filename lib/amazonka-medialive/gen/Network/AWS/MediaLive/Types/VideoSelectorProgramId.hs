{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorProgramId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorProgramId
  ( VideoSelectorProgramId (..),

    -- * Smart constructor
    mkVideoSelectorProgramId,

    -- * Lenses
    vspiProgramId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Video Selector Program Id
--
-- /See:/ 'mkVideoSelectorProgramId' smart constructor.
newtype VideoSelectorProgramId = VideoSelectorProgramId'
  { programId ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoSelectorProgramId' with the minimum fields required to make a request.
--
-- * 'programId' - Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
mkVideoSelectorProgramId ::
  VideoSelectorProgramId
mkVideoSelectorProgramId =
  VideoSelectorProgramId' {programId = Lude.Nothing}

-- | Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
--
-- /Note:/ Consider using 'programId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspiProgramId :: Lens.Lens' VideoSelectorProgramId (Lude.Maybe Lude.Natural)
vspiProgramId = Lens.lens (programId :: VideoSelectorProgramId -> Lude.Maybe Lude.Natural) (\s a -> s {programId = a} :: VideoSelectorProgramId)
{-# DEPRECATED vspiProgramId "Use generic-lens or generic-optics with 'programId' instead." #-}

instance Lude.FromJSON VideoSelectorProgramId where
  parseJSON =
    Lude.withObject
      "VideoSelectorProgramId"
      (\x -> VideoSelectorProgramId' Lude.<$> (x Lude..:? "programId"))

instance Lude.ToJSON VideoSelectorProgramId where
  toJSON VideoSelectorProgramId' {..} =
    Lude.object
      (Lude.catMaybes [("programId" Lude..=) Lude.<$> programId])
