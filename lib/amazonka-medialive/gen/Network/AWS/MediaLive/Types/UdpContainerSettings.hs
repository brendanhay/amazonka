{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpContainerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpContainerSettings
  ( UdpContainerSettings (..),

    -- * Smart constructor
    mkUdpContainerSettings,

    -- * Lenses
    ucsM2tsSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.M2tsSettings
import qualified Network.AWS.Prelude as Lude

-- | Udp Container Settings
--
-- /See:/ 'mkUdpContainerSettings' smart constructor.
newtype UdpContainerSettings = UdpContainerSettings'
  { m2tsSettings ::
      Lude.Maybe M2tsSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UdpContainerSettings' with the minimum fields required to make a request.
--
-- * 'm2tsSettings' - Undocumented field.
mkUdpContainerSettings ::
  UdpContainerSettings
mkUdpContainerSettings =
  UdpContainerSettings' {m2tsSettings = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'm2tsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsM2tsSettings :: Lens.Lens' UdpContainerSettings (Lude.Maybe M2tsSettings)
ucsM2tsSettings = Lens.lens (m2tsSettings :: UdpContainerSettings -> Lude.Maybe M2tsSettings) (\s a -> s {m2tsSettings = a} :: UdpContainerSettings)
{-# DEPRECATED ucsM2tsSettings "Use generic-lens or generic-optics with 'm2tsSettings' instead." #-}

instance Lude.FromJSON UdpContainerSettings where
  parseJSON =
    Lude.withObject
      "UdpContainerSettings"
      (\x -> UdpContainerSettings' Lude.<$> (x Lude..:? "m2tsSettings"))

instance Lude.ToJSON UdpContainerSettings where
  toJSON UdpContainerSettings' {..} =
    Lude.object
      (Lude.catMaybes [("m2tsSettings" Lude..=) Lude.<$> m2tsSettings])
