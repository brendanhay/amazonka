{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipChannelRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) VoIP channel for an application.
--
-- /See:/ 'newAPNSVoipChannelRequest' smart constructor.
data APNSVoipChannelRequest = APNSVoipChannelRequest'
  { -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with APNs, key or certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Prelude.Maybe Prelude.Text,
    -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The identifier that\'s assigned to your Apple developer account team.
    -- This identifier is used for APNs tokens.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to enable the APNs VoIP channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The private key for the APNs client certificate that you want Amazon
    -- Pinpoint to use to communicate with APNs.
    privateKey :: Prelude.Maybe Prelude.Text,
    -- | The key identifier that\'s assigned to your APNs signing key, if you
    -- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
    tokenKeyId :: Prelude.Maybe Prelude.Text,
    -- | The APNs client certificate that you received from Apple, if you want
    -- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
    certificate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'APNSVoipChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAuthenticationMethod', 'aPNSVoipChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with APNs, key or certificate.
--
-- 'tokenKey', 'aPNSVoipChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'bundleId', 'aPNSVoipChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'teamId', 'aPNSVoipChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'enabled', 'aPNSVoipChannelRequest_enabled' - Specifies whether to enable the APNs VoIP channel for the application.
--
-- 'privateKey', 'aPNSVoipChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with APNs.
--
-- 'tokenKeyId', 'aPNSVoipChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
--
-- 'certificate', 'aPNSVoipChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
newAPNSVoipChannelRequest ::
  APNSVoipChannelRequest
newAPNSVoipChannelRequest =
  APNSVoipChannelRequest'
    { defaultAuthenticationMethod =
        Prelude.Nothing,
      tokenKey = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      teamId = Prelude.Nothing,
      enabled = Prelude.Nothing,
      privateKey = Prelude.Nothing,
      tokenKeyId = Prelude.Nothing,
      certificate = Prelude.Nothing
    }

-- | The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with APNs, key or certificate.
aPNSVoipChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSVoipChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSVoipChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSVoipChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSVoipChannelRequest_tokenKey :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipChannelRequest_tokenKey = Lens.lens (\APNSVoipChannelRequest' {tokenKey} -> tokenKey) (\s@APNSVoipChannelRequest' {} a -> s {tokenKey = a} :: APNSVoipChannelRequest)

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSVoipChannelRequest_bundleId :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipChannelRequest_bundleId = Lens.lens (\APNSVoipChannelRequest' {bundleId} -> bundleId) (\s@APNSVoipChannelRequest' {} a -> s {bundleId = a} :: APNSVoipChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSVoipChannelRequest_teamId :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipChannelRequest_teamId = Lens.lens (\APNSVoipChannelRequest' {teamId} -> teamId) (\s@APNSVoipChannelRequest' {} a -> s {teamId = a} :: APNSVoipChannelRequest)

-- | Specifies whether to enable the APNs VoIP channel for the application.
aPNSVoipChannelRequest_enabled :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Bool)
aPNSVoipChannelRequest_enabled = Lens.lens (\APNSVoipChannelRequest' {enabled} -> enabled) (\s@APNSVoipChannelRequest' {} a -> s {enabled = a} :: APNSVoipChannelRequest)

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with APNs.
aPNSVoipChannelRequest_privateKey :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipChannelRequest_privateKey = Lens.lens (\APNSVoipChannelRequest' {privateKey} -> privateKey) (\s@APNSVoipChannelRequest' {} a -> s {privateKey = a} :: APNSVoipChannelRequest)

-- | The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
aPNSVoipChannelRequest_tokenKeyId :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipChannelRequest_tokenKeyId = Lens.lens (\APNSVoipChannelRequest' {tokenKeyId} -> tokenKeyId) (\s@APNSVoipChannelRequest' {} a -> s {tokenKeyId = a} :: APNSVoipChannelRequest)

-- | The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
aPNSVoipChannelRequest_certificate :: Lens.Lens' APNSVoipChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipChannelRequest_certificate = Lens.lens (\APNSVoipChannelRequest' {certificate} -> certificate) (\s@APNSVoipChannelRequest' {} a -> s {certificate = a} :: APNSVoipChannelRequest)

instance Prelude.Hashable APNSVoipChannelRequest

instance Prelude.NFData APNSVoipChannelRequest

instance Prelude.ToJSON APNSVoipChannelRequest where
  toJSON APNSVoipChannelRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DefaultAuthenticationMethod" Prelude..=)
              Prelude.<$> defaultAuthenticationMethod,
            ("TokenKey" Prelude..=) Prelude.<$> tokenKey,
            ("BundleId" Prelude..=) Prelude.<$> bundleId,
            ("TeamId" Prelude..=) Prelude.<$> teamId,
            ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("PrivateKey" Prelude..=) Prelude.<$> privateKey,
            ("TokenKeyId" Prelude..=) Prelude.<$> tokenKeyId,
            ("Certificate" Prelude..=) Prelude.<$> certificate
          ]
      )
