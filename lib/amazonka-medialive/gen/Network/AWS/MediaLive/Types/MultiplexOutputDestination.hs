{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexOutputDestination
  ( MultiplexOutputDestination (..),

    -- * Smart constructor
    mkMultiplexOutputDestination,

    -- * Lenses
    modMediaConnectSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Multiplex output destination settings
--
-- /See:/ 'mkMultiplexOutputDestination' smart constructor.
newtype MultiplexOutputDestination = MultiplexOutputDestination'
  { -- | Multiplex MediaConnect output destination settings.
    mediaConnectSettings :: Lude.Maybe MultiplexMediaConnectOutputDestinationSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexOutputDestination' with the minimum fields required to make a request.
--
-- * 'mediaConnectSettings' - Multiplex MediaConnect output destination settings.
mkMultiplexOutputDestination ::
  MultiplexOutputDestination
mkMultiplexOutputDestination =
  MultiplexOutputDestination' {mediaConnectSettings = Lude.Nothing}

-- | Multiplex MediaConnect output destination settings.
--
-- /Note:/ Consider using 'mediaConnectSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
modMediaConnectSettings :: Lens.Lens' MultiplexOutputDestination (Lude.Maybe MultiplexMediaConnectOutputDestinationSettings)
modMediaConnectSettings = Lens.lens (mediaConnectSettings :: MultiplexOutputDestination -> Lude.Maybe MultiplexMediaConnectOutputDestinationSettings) (\s a -> s {mediaConnectSettings = a} :: MultiplexOutputDestination)
{-# DEPRECATED modMediaConnectSettings "Use generic-lens or generic-optics with 'mediaConnectSettings' instead." #-}

instance Lude.FromJSON MultiplexOutputDestination where
  parseJSON =
    Lude.withObject
      "MultiplexOutputDestination"
      ( \x ->
          MultiplexOutputDestination'
            Lude.<$> (x Lude..:? "mediaConnectSettings")
      )
