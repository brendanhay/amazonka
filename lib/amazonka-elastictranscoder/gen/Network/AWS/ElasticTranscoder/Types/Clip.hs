-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Clip
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Clip
  ( Clip (..),

    -- * Smart constructor
    mkClip,

    -- * Lenses
    cTimeSpan,
  )
where

import Network.AWS.ElasticTranscoder.Types.TimeSpan
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for one clip in a composition. All jobs in a playlist must have the same clip settings.
--
-- /See:/ 'mkClip' smart constructor.
newtype Clip = Clip' {timeSpan :: Lude.Maybe TimeSpan}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Clip' with the minimum fields required to make a request.
--
-- * 'timeSpan' - Settings that determine when a clip begins and how long it lasts.
mkClip ::
  Clip
mkClip = Clip' {timeSpan = Lude.Nothing}

-- | Settings that determine when a clip begins and how long it lasts.
--
-- /Note:/ Consider using 'timeSpan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTimeSpan :: Lens.Lens' Clip (Lude.Maybe TimeSpan)
cTimeSpan = Lens.lens (timeSpan :: Clip -> Lude.Maybe TimeSpan) (\s a -> s {timeSpan = a} :: Clip)
{-# DEPRECATED cTimeSpan "Use generic-lens or generic-optics with 'timeSpan' instead." #-}

instance Lude.FromJSON Clip where
  parseJSON =
    Lude.withObject
      "Clip"
      (\x -> Clip' Lude.<$> (x Lude..:? "TimeSpan"))

instance Lude.ToJSON Clip where
  toJSON Clip' {..} =
    Lude.object
      (Lude.catMaybes [("TimeSpan" Lude..=) Lude.<$> timeSpan])
