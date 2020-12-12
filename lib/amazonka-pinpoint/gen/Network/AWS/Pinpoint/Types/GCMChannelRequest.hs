{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMChannelRequest
  ( GCMChannelRequest (..),

    -- * Smart constructor
    mkGCMChannelRequest,

    -- * Lenses
    gcrEnabled,
    gcrAPIKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the GCM channel for an application. This channel enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'mkGCMChannelRequest' smart constructor.
data GCMChannelRequest = GCMChannelRequest'
  { enabled ::
      Lude.Maybe Lude.Bool,
    apiKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GCMChannelRequest' with the minimum fields required to make a request.
--
-- * 'apiKey' - The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
-- * 'enabled' - Specifies whether to enable the GCM channel for the application.
mkGCMChannelRequest ::
  -- | 'apiKey'
  Lude.Text ->
  GCMChannelRequest
mkGCMChannelRequest pAPIKey_ =
  GCMChannelRequest' {enabled = Lude.Nothing, apiKey = pAPIKey_}

-- | Specifies whether to enable the GCM channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrEnabled :: Lens.Lens' GCMChannelRequest (Lude.Maybe Lude.Bool)
gcrEnabled = Lens.lens (enabled :: GCMChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: GCMChannelRequest)
{-# DEPRECATED gcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrAPIKey :: Lens.Lens' GCMChannelRequest Lude.Text
gcrAPIKey = Lens.lens (apiKey :: GCMChannelRequest -> Lude.Text) (\s a -> s {apiKey = a} :: GCMChannelRequest)
{-# DEPRECATED gcrAPIKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

instance Lude.ToJSON GCMChannelRequest where
  toJSON GCMChannelRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            Lude.Just ("ApiKey" Lude..= apiKey)
          ]
      )
