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
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) sandbox channel for an application.
--
-- /See:/ 'newAPNSSandboxChannelRequest' smart constructor.
data APNSSandboxChannelRequest = APNSSandboxChannelRequest'
  { -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with the APNs sandbox environment, key or
    -- certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Core.Maybe Core.Text,
    -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Core.Maybe Core.Text,
    -- | The identifier that\'s assigned to your Apple developer account team.
    -- This identifier is used for APNs tokens.
    teamId :: Core.Maybe Core.Text,
    -- | Specifies whether to enable the APNs sandbox channel for the
    -- application.
    enabled :: Core.Maybe Core.Bool,
    -- | The private key for the APNs client certificate that you want Amazon
    -- Pinpoint to use to communicate with the APNs sandbox environment.
    privateKey :: Core.Maybe Core.Text,
    -- | The key identifier that\'s assigned to your APNs signing key, if you
    -- want Amazon Pinpoint to communicate with the APNs sandbox environment by
    -- using APNs tokens.
    tokenKeyId :: Core.Maybe Core.Text,
    -- | The APNs client certificate that you received from Apple, if you want
    -- Amazon Pinpoint to communicate with the APNs sandbox environment by
    -- using an APNs certificate.
    certificate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'APNSSandboxChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAuthenticationMethod', 'aPNSSandboxChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment, key or
-- certificate.
--
-- 'tokenKey', 'aPNSSandboxChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'bundleId', 'aPNSSandboxChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'teamId', 'aPNSSandboxChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'enabled', 'aPNSSandboxChannelRequest_enabled' - Specifies whether to enable the APNs sandbox channel for the
-- application.
--
-- 'privateKey', 'aPNSSandboxChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
--
-- 'tokenKeyId', 'aPNSSandboxChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
--
-- 'certificate', 'aPNSSandboxChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
newAPNSSandboxChannelRequest ::
  APNSSandboxChannelRequest
newAPNSSandboxChannelRequest =
  APNSSandboxChannelRequest'
    { defaultAuthenticationMethod =
        Core.Nothing,
      tokenKey = Core.Nothing,
      bundleId = Core.Nothing,
      teamId = Core.Nothing,
      enabled = Core.Nothing,
      privateKey = Core.Nothing,
      tokenKeyId = Core.Nothing,
      certificate = Core.Nothing
    }

-- | The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment, key or
-- certificate.
aPNSSandboxChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
aPNSSandboxChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSSandboxChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSSandboxChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSSandboxChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSSandboxChannelRequest_tokenKey :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
aPNSSandboxChannelRequest_tokenKey = Lens.lens (\APNSSandboxChannelRequest' {tokenKey} -> tokenKey) (\s@APNSSandboxChannelRequest' {} a -> s {tokenKey = a} :: APNSSandboxChannelRequest)

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSSandboxChannelRequest_bundleId :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
aPNSSandboxChannelRequest_bundleId = Lens.lens (\APNSSandboxChannelRequest' {bundleId} -> bundleId) (\s@APNSSandboxChannelRequest' {} a -> s {bundleId = a} :: APNSSandboxChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSSandboxChannelRequest_teamId :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
aPNSSandboxChannelRequest_teamId = Lens.lens (\APNSSandboxChannelRequest' {teamId} -> teamId) (\s@APNSSandboxChannelRequest' {} a -> s {teamId = a} :: APNSSandboxChannelRequest)

-- | Specifies whether to enable the APNs sandbox channel for the
-- application.
aPNSSandboxChannelRequest_enabled :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Bool)
aPNSSandboxChannelRequest_enabled = Lens.lens (\APNSSandboxChannelRequest' {enabled} -> enabled) (\s@APNSSandboxChannelRequest' {} a -> s {enabled = a} :: APNSSandboxChannelRequest)

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
aPNSSandboxChannelRequest_privateKey :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
aPNSSandboxChannelRequest_privateKey = Lens.lens (\APNSSandboxChannelRequest' {privateKey} -> privateKey) (\s@APNSSandboxChannelRequest' {} a -> s {privateKey = a} :: APNSSandboxChannelRequest)

-- | The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
aPNSSandboxChannelRequest_tokenKeyId :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
aPNSSandboxChannelRequest_tokenKeyId = Lens.lens (\APNSSandboxChannelRequest' {tokenKeyId} -> tokenKeyId) (\s@APNSSandboxChannelRequest' {} a -> s {tokenKeyId = a} :: APNSSandboxChannelRequest)

-- | The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
aPNSSandboxChannelRequest_certificate :: Lens.Lens' APNSSandboxChannelRequest (Core.Maybe Core.Text)
aPNSSandboxChannelRequest_certificate = Lens.lens (\APNSSandboxChannelRequest' {certificate} -> certificate) (\s@APNSSandboxChannelRequest' {} a -> s {certificate = a} :: APNSSandboxChannelRequest)

instance Core.Hashable APNSSandboxChannelRequest

instance Core.NFData APNSSandboxChannelRequest

instance Core.ToJSON APNSSandboxChannelRequest where
  toJSON APNSSandboxChannelRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultAuthenticationMethod" Core..=)
              Core.<$> defaultAuthenticationMethod,
            ("TokenKey" Core..=) Core.<$> tokenKey,
            ("BundleId" Core..=) Core.<$> bundleId,
            ("TeamId" Core..=) Core.<$> teamId,
            ("Enabled" Core..=) Core.<$> enabled,
            ("PrivateKey" Core..=) Core.<$> privateKey,
            ("TokenKeyId" Core..=) Core.<$> tokenKeyId,
            ("Certificate" Core..=) Core.<$> certificate
          ]
      )
