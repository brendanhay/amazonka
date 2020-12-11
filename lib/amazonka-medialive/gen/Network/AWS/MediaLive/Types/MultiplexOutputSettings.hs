-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexOutputSettings
  ( MultiplexOutputSettings (..),

    -- * Smart constructor
    mkMultiplexOutputSettings,

    -- * Lenses
    mosDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import qualified Network.AWS.Prelude as Lude

-- | Multiplex Output Settings
--
-- /See:/ 'mkMultiplexOutputSettings' smart constructor.
newtype MultiplexOutputSettings = MultiplexOutputSettings'
  { destination ::
      OutputLocationRef
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexOutputSettings' with the minimum fields required to make a request.
--
-- * 'destination' - Destination is a Multiplex.
mkMultiplexOutputSettings ::
  -- | 'destination'
  OutputLocationRef ->
  MultiplexOutputSettings
mkMultiplexOutputSettings pDestination_ =
  MultiplexOutputSettings' {destination = pDestination_}

-- | Destination is a Multiplex.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mosDestination :: Lens.Lens' MultiplexOutputSettings OutputLocationRef
mosDestination = Lens.lens (destination :: MultiplexOutputSettings -> OutputLocationRef) (\s a -> s {destination = a} :: MultiplexOutputSettings)
{-# DEPRECATED mosDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON MultiplexOutputSettings where
  parseJSON =
    Lude.withObject
      "MultiplexOutputSettings"
      ( \x ->
          MultiplexOutputSettings' Lude.<$> (x Lude..: "destination")
      )

instance Lude.ToJSON MultiplexOutputSettings where
  toJSON MultiplexOutputSettings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("destination" Lude..= destination)])
