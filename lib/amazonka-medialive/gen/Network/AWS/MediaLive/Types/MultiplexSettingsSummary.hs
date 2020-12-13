{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettingsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettingsSummary
  ( MultiplexSettingsSummary (..),

    -- * Smart constructor
    mkMultiplexSettingsSummary,

    -- * Lenses
    mTransportStreamBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary configuration for a Multiplex event.
--
-- /See:/ 'mkMultiplexSettingsSummary' smart constructor.
newtype MultiplexSettingsSummary = MultiplexSettingsSummary'
  { -- | Transport stream bit rate.
    transportStreamBitrate :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexSettingsSummary' with the minimum fields required to make a request.
--
-- * 'transportStreamBitrate' - Transport stream bit rate.
mkMultiplexSettingsSummary ::
  MultiplexSettingsSummary
mkMultiplexSettingsSummary =
  MultiplexSettingsSummary' {transportStreamBitrate = Lude.Nothing}

-- | Transport stream bit rate.
--
-- /Note:/ Consider using 'transportStreamBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTransportStreamBitrate :: Lens.Lens' MultiplexSettingsSummary (Lude.Maybe Lude.Natural)
mTransportStreamBitrate = Lens.lens (transportStreamBitrate :: MultiplexSettingsSummary -> Lude.Maybe Lude.Natural) (\s a -> s {transportStreamBitrate = a} :: MultiplexSettingsSummary)
{-# DEPRECATED mTransportStreamBitrate "Use generic-lens or generic-optics with 'transportStreamBitrate' instead." #-}

instance Lude.FromJSON MultiplexSettingsSummary where
  parseJSON =
    Lude.withObject
      "MultiplexSettingsSummary"
      ( \x ->
          MultiplexSettingsSummary'
            Lude.<$> (x Lude..:? "transportStreamBitrate")
      )
