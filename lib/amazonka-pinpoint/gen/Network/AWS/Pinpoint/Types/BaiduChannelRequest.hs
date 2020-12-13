{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaiduChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduChannelRequest
  ( BaiduChannelRequest (..),

    -- * Smart constructor
    mkBaiduChannelRequest,

    -- * Lenses
    bcrAPIKey,
    bcrEnabled,
    bcrSecretKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the Baidu (Baidu Cloud Push) channel for an application.
--
-- /See:/ 'mkBaiduChannelRequest' smart constructor.
data BaiduChannelRequest = BaiduChannelRequest'
  { -- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
    apiKey :: Lude.Text,
    -- | Specifies whether to enable the Baidu channel for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The secret key that you received from the Baidu Cloud Push service to communicate with the service.
    secretKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BaiduChannelRequest' with the minimum fields required to make a request.
--
-- * 'apiKey' - The API key that you received from the Baidu Cloud Push service to communicate with the service.
-- * 'enabled' - Specifies whether to enable the Baidu channel for the application.
-- * 'secretKey' - The secret key that you received from the Baidu Cloud Push service to communicate with the service.
mkBaiduChannelRequest ::
  -- | 'apiKey'
  Lude.Text ->
  -- | 'secretKey'
  Lude.Text ->
  BaiduChannelRequest
mkBaiduChannelRequest pAPIKey_ pSecretKey_ =
  BaiduChannelRequest'
    { apiKey = pAPIKey_,
      enabled = Lude.Nothing,
      secretKey = pSecretKey_
    }

-- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrAPIKey :: Lens.Lens' BaiduChannelRequest Lude.Text
bcrAPIKey = Lens.lens (apiKey :: BaiduChannelRequest -> Lude.Text) (\s a -> s {apiKey = a} :: BaiduChannelRequest)
{-# DEPRECATED bcrAPIKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

-- | Specifies whether to enable the Baidu channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrEnabled :: Lens.Lens' BaiduChannelRequest (Lude.Maybe Lude.Bool)
bcrEnabled = Lens.lens (enabled :: BaiduChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: BaiduChannelRequest)
{-# DEPRECATED bcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The secret key that you received from the Baidu Cloud Push service to communicate with the service.
--
-- /Note:/ Consider using 'secretKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrSecretKey :: Lens.Lens' BaiduChannelRequest Lude.Text
bcrSecretKey = Lens.lens (secretKey :: BaiduChannelRequest -> Lude.Text) (\s a -> s {secretKey = a} :: BaiduChannelRequest)
{-# DEPRECATED bcrSecretKey "Use generic-lens or generic-optics with 'secretKey' instead." #-}

instance Lude.ToJSON BaiduChannelRequest where
  toJSON BaiduChannelRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApiKey" Lude..= apiKey),
            ("Enabled" Lude..=) Lude.<$> enabled,
            Lude.Just ("SecretKey" Lude..= secretKey)
          ]
      )
