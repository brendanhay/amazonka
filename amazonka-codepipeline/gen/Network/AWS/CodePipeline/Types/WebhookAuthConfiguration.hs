{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookAuthConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The authentication applied to incoming webhook trigger requests.
--
-- /See:/ 'newWebhookAuthConfiguration' smart constructor.
data WebhookAuthConfiguration = WebhookAuthConfiguration'
  { -- | The property used to configure acceptance of webhooks in an IP address
    -- range. For IP, only the @AllowedIPRange@ property must be set. This
    -- property must be set to a valid CIDR range.
    allowedIPRange :: Core.Maybe Core.Text,
    -- | The property used to configure GitHub authentication. For GITHUB_HMAC,
    -- only the @SecretToken@ property must be set.
    secretToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WebhookAuthConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedIPRange', 'webhookAuthConfiguration_allowedIPRange' - The property used to configure acceptance of webhooks in an IP address
-- range. For IP, only the @AllowedIPRange@ property must be set. This
-- property must be set to a valid CIDR range.
--
-- 'secretToken', 'webhookAuthConfiguration_secretToken' - The property used to configure GitHub authentication. For GITHUB_HMAC,
-- only the @SecretToken@ property must be set.
newWebhookAuthConfiguration ::
  WebhookAuthConfiguration
newWebhookAuthConfiguration =
  WebhookAuthConfiguration'
    { allowedIPRange =
        Core.Nothing,
      secretToken = Core.Nothing
    }

-- | The property used to configure acceptance of webhooks in an IP address
-- range. For IP, only the @AllowedIPRange@ property must be set. This
-- property must be set to a valid CIDR range.
webhookAuthConfiguration_allowedIPRange :: Lens.Lens' WebhookAuthConfiguration (Core.Maybe Core.Text)
webhookAuthConfiguration_allowedIPRange = Lens.lens (\WebhookAuthConfiguration' {allowedIPRange} -> allowedIPRange) (\s@WebhookAuthConfiguration' {} a -> s {allowedIPRange = a} :: WebhookAuthConfiguration)

-- | The property used to configure GitHub authentication. For GITHUB_HMAC,
-- only the @SecretToken@ property must be set.
webhookAuthConfiguration_secretToken :: Lens.Lens' WebhookAuthConfiguration (Core.Maybe Core.Text)
webhookAuthConfiguration_secretToken = Lens.lens (\WebhookAuthConfiguration' {secretToken} -> secretToken) (\s@WebhookAuthConfiguration' {} a -> s {secretToken = a} :: WebhookAuthConfiguration)

instance Core.FromJSON WebhookAuthConfiguration where
  parseJSON =
    Core.withObject
      "WebhookAuthConfiguration"
      ( \x ->
          WebhookAuthConfiguration'
            Core.<$> (x Core..:? "AllowedIPRange")
            Core.<*> (x Core..:? "SecretToken")
      )

instance Core.Hashable WebhookAuthConfiguration

instance Core.NFData WebhookAuthConfiguration

instance Core.ToJSON WebhookAuthConfiguration where
  toJSON WebhookAuthConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowedIPRange" Core..=) Core.<$> allowedIPRange,
            ("SecretToken" Core..=) Core.<$> secretToken
          ]
      )
