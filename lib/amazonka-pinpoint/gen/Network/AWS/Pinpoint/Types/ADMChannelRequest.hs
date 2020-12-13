{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMChannelRequest
  ( ADMChannelRequest (..),

    -- * Smart constructor
    mkADMChannelRequest,

    -- * Lenses
    admcrClientId,
    admcrClientSecret,
    admcrEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
-- /See:/ 'mkADMChannelRequest' smart constructor.
data ADMChannelRequest = ADMChannelRequest'
  { -- | The Client ID that you received from Amazon to send messages by using ADM.
    clientId :: Lude.Text,
    -- | The Client Secret that you received from Amazon to send messages by using ADM.
    clientSecret :: Lude.Text,
    -- | Specifies whether to enable the ADM channel for the application.
    enabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ADMChannelRequest' with the minimum fields required to make a request.
--
-- * 'clientId' - The Client ID that you received from Amazon to send messages by using ADM.
-- * 'clientSecret' - The Client Secret that you received from Amazon to send messages by using ADM.
-- * 'enabled' - Specifies whether to enable the ADM channel for the application.
mkADMChannelRequest ::
  -- | 'clientId'
  Lude.Text ->
  -- | 'clientSecret'
  Lude.Text ->
  ADMChannelRequest
mkADMChannelRequest pClientId_ pClientSecret_ =
  ADMChannelRequest'
    { clientId = pClientId_,
      clientSecret = pClientSecret_,
      enabled = Lude.Nothing
    }

-- | The Client ID that you received from Amazon to send messages by using ADM.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrClientId :: Lens.Lens' ADMChannelRequest Lude.Text
admcrClientId = Lens.lens (clientId :: ADMChannelRequest -> Lude.Text) (\s a -> s {clientId = a} :: ADMChannelRequest)
{-# DEPRECATED admcrClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The Client Secret that you received from Amazon to send messages by using ADM.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrClientSecret :: Lens.Lens' ADMChannelRequest Lude.Text
admcrClientSecret = Lens.lens (clientSecret :: ADMChannelRequest -> Lude.Text) (\s a -> s {clientSecret = a} :: ADMChannelRequest)
{-# DEPRECATED admcrClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | Specifies whether to enable the ADM channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrEnabled :: Lens.Lens' ADMChannelRequest (Lude.Maybe Lude.Bool)
admcrEnabled = Lens.lens (enabled :: ADMChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ADMChannelRequest)
{-# DEPRECATED admcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToJSON ADMChannelRequest where
  toJSON ADMChannelRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("ClientSecret" Lude..= clientSecret),
            ("Enabled" Lude..=) Lude.<$> enabled
          ]
      )
