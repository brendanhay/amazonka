-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceChannelRequest
  ( VoiceChannelRequest (..),

    -- * Smart constructor
    mkVoiceChannelRequest,

    -- * Lenses
    vcrEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the voice channel for an application.
--
-- /See:/ 'mkVoiceChannelRequest' smart constructor.
newtype VoiceChannelRequest = VoiceChannelRequest'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VoiceChannelRequest' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether to enable the voice channel for the application.
mkVoiceChannelRequest ::
  VoiceChannelRequest
mkVoiceChannelRequest =
  VoiceChannelRequest' {enabled = Lude.Nothing}

-- | Specifies whether to enable the voice channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrEnabled :: Lens.Lens' VoiceChannelRequest (Lude.Maybe Lude.Bool)
vcrEnabled = Lens.lens (enabled :: VoiceChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: VoiceChannelRequest)
{-# DEPRECATED vcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToJSON VoiceChannelRequest where
  toJSON VoiceChannelRequest' {..} =
    Lude.object
      (Lude.catMaybes [("Enabled" Lude..=) Lude.<$> enabled])
