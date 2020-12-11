-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailConfiguration
  ( AvailConfiguration (..),

    -- * Smart constructor
    mkAvailConfiguration,

    -- * Lenses
    acAvailSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AvailSettings
import qualified Network.AWS.Prelude as Lude

-- | Avail Configuration
--
-- /See:/ 'mkAvailConfiguration' smart constructor.
newtype AvailConfiguration = AvailConfiguration'
  { availSettings ::
      Lude.Maybe AvailSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailConfiguration' with the minimum fields required to make a request.
--
-- * 'availSettings' - Ad avail settings.
mkAvailConfiguration ::
  AvailConfiguration
mkAvailConfiguration =
  AvailConfiguration' {availSettings = Lude.Nothing}

-- | Ad avail settings.
--
-- /Note:/ Consider using 'availSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAvailSettings :: Lens.Lens' AvailConfiguration (Lude.Maybe AvailSettings)
acAvailSettings = Lens.lens (availSettings :: AvailConfiguration -> Lude.Maybe AvailSettings) (\s a -> s {availSettings = a} :: AvailConfiguration)
{-# DEPRECATED acAvailSettings "Use generic-lens or generic-optics with 'availSettings' instead." #-}

instance Lude.FromJSON AvailConfiguration where
  parseJSON =
    Lude.withObject
      "AvailConfiguration"
      (\x -> AvailConfiguration' Lude.<$> (x Lude..:? "availSettings"))

instance Lude.ToJSON AvailConfiguration where
  toJSON AvailConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [("availSettings" Lude..=) Lude.<$> availSettings])
