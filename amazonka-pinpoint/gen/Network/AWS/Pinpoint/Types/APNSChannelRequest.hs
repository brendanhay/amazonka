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
-- Module      : Network.AWS.Pinpoint.Types.APNSChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSChannelRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) channel for an application.
--
-- /See:/ 'newAPNSChannelRequest' smart constructor.
data APNSChannelRequest = APNSChannelRequest'
  { -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with APNs, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Core.Maybe Core.Text,
    -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Core.Maybe Core.Text,
    -- | The identifier that\'s assigned to your Apple developer account team.
    -- This identifier is used for APNs tokens.
    teamId :: Core.Maybe Core.Text,
    -- | Specifies whether to enable the APNs channel for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The private key for the APNs client certificate that you want Amazon
    -- Pinpoint to use to communicate with APNs.
    privateKey :: Core.Maybe Core.Text,
    -- | The key identifier that\'s assigned to your APNs signing key, if you
    -- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
    tokenKeyId :: Core.Maybe Core.Text,
    -- | The APNs client certificate that you received from Apple, if you want
    -- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
    certificate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'APNSChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAuthenticationMethod', 'aPNSChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with APNs, key or certificate.
--
-- 'tokenKey', 'aPNSChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'bundleId', 'aPNSChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'teamId', 'aPNSChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'enabled', 'aPNSChannelRequest_enabled' - Specifies whether to enable the APNs channel for the application.
--
-- 'privateKey', 'aPNSChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with APNs.
--
-- 'tokenKeyId', 'aPNSChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
--
-- 'certificate', 'aPNSChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
newAPNSChannelRequest ::
  APNSChannelRequest
newAPNSChannelRequest =
  APNSChannelRequest'
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
-- when authenticating with APNs, key or certificate.
aPNSChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Text)
aPNSChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSChannelRequest_tokenKey :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Text)
aPNSChannelRequest_tokenKey = Lens.lens (\APNSChannelRequest' {tokenKey} -> tokenKey) (\s@APNSChannelRequest' {} a -> s {tokenKey = a} :: APNSChannelRequest)

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSChannelRequest_bundleId :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Text)
aPNSChannelRequest_bundleId = Lens.lens (\APNSChannelRequest' {bundleId} -> bundleId) (\s@APNSChannelRequest' {} a -> s {bundleId = a} :: APNSChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSChannelRequest_teamId :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Text)
aPNSChannelRequest_teamId = Lens.lens (\APNSChannelRequest' {teamId} -> teamId) (\s@APNSChannelRequest' {} a -> s {teamId = a} :: APNSChannelRequest)

-- | Specifies whether to enable the APNs channel for the application.
aPNSChannelRequest_enabled :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Bool)
aPNSChannelRequest_enabled = Lens.lens (\APNSChannelRequest' {enabled} -> enabled) (\s@APNSChannelRequest' {} a -> s {enabled = a} :: APNSChannelRequest)

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with APNs.
aPNSChannelRequest_privateKey :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Text)
aPNSChannelRequest_privateKey = Lens.lens (\APNSChannelRequest' {privateKey} -> privateKey) (\s@APNSChannelRequest' {} a -> s {privateKey = a} :: APNSChannelRequest)

-- | The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
aPNSChannelRequest_tokenKeyId :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Text)
aPNSChannelRequest_tokenKeyId = Lens.lens (\APNSChannelRequest' {tokenKeyId} -> tokenKeyId) (\s@APNSChannelRequest' {} a -> s {tokenKeyId = a} :: APNSChannelRequest)

-- | The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
aPNSChannelRequest_certificate :: Lens.Lens' APNSChannelRequest (Core.Maybe Core.Text)
aPNSChannelRequest_certificate = Lens.lens (\APNSChannelRequest' {certificate} -> certificate) (\s@APNSChannelRequest' {} a -> s {certificate = a} :: APNSChannelRequest)

instance Core.Hashable APNSChannelRequest

instance Core.NFData APNSChannelRequest

instance Core.ToJSON APNSChannelRequest where
  toJSON APNSChannelRequest' {..} =
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
