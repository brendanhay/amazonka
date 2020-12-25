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

import qualified Network.AWS.CodePipeline.Types.WebhookAuthConfigurationAllowedIPRange as Types
import qualified Network.AWS.CodePipeline.Types.WebhookAuthConfigurationSecretToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authentication applied to incoming webhook trigger requests.
--
-- /See:/ 'mkWebhookAuthConfiguration' smart constructor.
data WebhookAuthConfiguration = WebhookAuthConfiguration'
  { -- | The property used to configure acceptance of webhooks in an IP address range. For IP, only the @AllowedIPRange@ property must be set. This property must be set to a valid CIDR range.
    allowedIPRange :: Core.Maybe Types.WebhookAuthConfigurationAllowedIPRange,
    -- | The property used to configure GitHub authentication. For GITHUB_HMAC, only the @SecretToken@ property must be set.
    secretToken :: Core.Maybe Types.WebhookAuthConfigurationSecretToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebhookAuthConfiguration' value with any optional fields omitted.
mkWebhookAuthConfiguration ::
  WebhookAuthConfiguration
mkWebhookAuthConfiguration =
  WebhookAuthConfiguration'
    { allowedIPRange = Core.Nothing,
      secretToken = Core.Nothing
    }

-- | The property used to configure acceptance of webhooks in an IP address range. For IP, only the @AllowedIPRange@ property must be set. This property must be set to a valid CIDR range.
--
-- /Note:/ Consider using 'allowedIPRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wacAllowedIPRange :: Lens.Lens' WebhookAuthConfiguration (Core.Maybe Types.WebhookAuthConfigurationAllowedIPRange)
wacAllowedIPRange = Lens.field @"allowedIPRange"
{-# DEPRECATED wacAllowedIPRange "Use generic-lens or generic-optics with 'allowedIPRange' instead." #-}

-- | The property used to configure GitHub authentication. For GITHUB_HMAC, only the @SecretToken@ property must be set.
--
-- /Note:/ Consider using 'secretToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wacSecretToken :: Lens.Lens' WebhookAuthConfiguration (Core.Maybe Types.WebhookAuthConfigurationSecretToken)
wacSecretToken = Lens.field @"secretToken"
{-# DEPRECATED wacSecretToken "Use generic-lens or generic-optics with 'secretToken' instead." #-}

instance Core.FromJSON WebhookAuthConfiguration where
  toJSON WebhookAuthConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowedIPRange" Core..=) Core.<$> allowedIPRange,
            ("SecretToken" Core..=) Core.<$> secretToken
          ]
      )

instance Core.FromJSON WebhookAuthConfiguration where
  parseJSON =
    Core.withObject "WebhookAuthConfiguration" Core.$
      \x ->
        WebhookAuthConfiguration'
          Core.<$> (x Core..:? "AllowedIPRange") Core.<*> (x Core..:? "SecretToken")
