{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
  ( WebhookAuthConfiguration (..),

    -- * Smart constructor
    mkWebhookAuthConfiguration,

    -- * Lenses
    wacAllowedIPRange,
    wacSecretToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authentication applied to incoming webhook trigger requests.
--
-- /See:/ 'mkWebhookAuthConfiguration' smart constructor.
data WebhookAuthConfiguration = WebhookAuthConfiguration'
  { -- | The property used to configure acceptance of webhooks in an IP address range. For IP, only the @AllowedIPRange@ property must be set. This property must be set to a valid CIDR range.
    allowedIPRange :: Lude.Maybe Lude.Text,
    -- | The property used to configure GitHub authentication. For GITHUB_HMAC, only the @SecretToken@ property must be set.
    secretToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebhookAuthConfiguration' with the minimum fields required to make a request.
--
-- * 'allowedIPRange' - The property used to configure acceptance of webhooks in an IP address range. For IP, only the @AllowedIPRange@ property must be set. This property must be set to a valid CIDR range.
-- * 'secretToken' - The property used to configure GitHub authentication. For GITHUB_HMAC, only the @SecretToken@ property must be set.
mkWebhookAuthConfiguration ::
  WebhookAuthConfiguration
mkWebhookAuthConfiguration =
  WebhookAuthConfiguration'
    { allowedIPRange = Lude.Nothing,
      secretToken = Lude.Nothing
    }

-- | The property used to configure acceptance of webhooks in an IP address range. For IP, only the @AllowedIPRange@ property must be set. This property must be set to a valid CIDR range.
--
-- /Note:/ Consider using 'allowedIPRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wacAllowedIPRange :: Lens.Lens' WebhookAuthConfiguration (Lude.Maybe Lude.Text)
wacAllowedIPRange = Lens.lens (allowedIPRange :: WebhookAuthConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {allowedIPRange = a} :: WebhookAuthConfiguration)
{-# DEPRECATED wacAllowedIPRange "Use generic-lens or generic-optics with 'allowedIPRange' instead." #-}

-- | The property used to configure GitHub authentication. For GITHUB_HMAC, only the @SecretToken@ property must be set.
--
-- /Note:/ Consider using 'secretToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wacSecretToken :: Lens.Lens' WebhookAuthConfiguration (Lude.Maybe Lude.Text)
wacSecretToken = Lens.lens (secretToken :: WebhookAuthConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {secretToken = a} :: WebhookAuthConfiguration)
{-# DEPRECATED wacSecretToken "Use generic-lens or generic-optics with 'secretToken' instead." #-}

instance Lude.FromJSON WebhookAuthConfiguration where
  parseJSON =
    Lude.withObject
      "WebhookAuthConfiguration"
      ( \x ->
          WebhookAuthConfiguration'
            Lude.<$> (x Lude..:? "AllowedIPRange") Lude.<*> (x Lude..:? "SecretToken")
      )

instance Lude.ToJSON WebhookAuthConfiguration where
  toJSON WebhookAuthConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AllowedIPRange" Lude..=) Lude.<$> allowedIPRange,
            ("SecretToken" Lude..=) Lude.<$> secretToken
          ]
      )
