{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
  ( PauseStateScheduleActionSettings (..),

    -- * Smart constructor
    mkPauseStateScheduleActionSettings,

    -- * Lenses
    pssasPipelines,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.PipelinePauseStateSettings
import qualified Network.AWS.Prelude as Lude

-- | Settings for the action to set pause state of a channel.
--
-- /See:/ 'mkPauseStateScheduleActionSettings' smart constructor.
newtype PauseStateScheduleActionSettings = PauseStateScheduleActionSettings'
  { pipelines ::
      Lude.Maybe
        [PipelinePauseStateSettings]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PauseStateScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'pipelines' - Undocumented field.
mkPauseStateScheduleActionSettings ::
  PauseStateScheduleActionSettings
mkPauseStateScheduleActionSettings =
  PauseStateScheduleActionSettings' {pipelines = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pipelines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssasPipelines :: Lens.Lens' PauseStateScheduleActionSettings (Lude.Maybe [PipelinePauseStateSettings])
pssasPipelines = Lens.lens (pipelines :: PauseStateScheduleActionSettings -> Lude.Maybe [PipelinePauseStateSettings]) (\s a -> s {pipelines = a} :: PauseStateScheduleActionSettings)
{-# DEPRECATED pssasPipelines "Use generic-lens or generic-optics with 'pipelines' instead." #-}

instance Lude.FromJSON PauseStateScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "PauseStateScheduleActionSettings"
      ( \x ->
          PauseStateScheduleActionSettings'
            Lude.<$> (x Lude..:? "pipelines" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PauseStateScheduleActionSettings where
  toJSON PauseStateScheduleActionSettings' {..} =
    Lude.object
      (Lude.catMaybes [("pipelines" Lude..=) Lude.<$> pipelines])
