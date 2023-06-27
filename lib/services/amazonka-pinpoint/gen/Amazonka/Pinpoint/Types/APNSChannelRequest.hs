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
-- Module      : Amazonka.Pinpoint.Types.APNSChannelRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSChannelRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) channel for an application.
--
-- /See:/ 'newAPNSChannelRequest' smart constructor.
data APNSChannelRequest = APNSChannelRequest'
  { -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The APNs client certificate that you received from Apple, if you want
    -- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with APNs, key or certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to enable the APNs channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The private key for the APNs client certificate that you want Amazon
    -- Pinpoint to use to communicate with APNs.
    privateKey :: Prelude.Maybe Prelude.Text,
    -- | The identifier that\'s assigned to your Apple developer account team.
    -- This identifier is used for APNs tokens.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Prelude.Maybe Prelude.Text,
    -- | The key identifier that\'s assigned to your APNs signing key, if you
    -- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
    tokenKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APNSChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'aPNSChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'certificate', 'aPNSChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
--
-- 'defaultAuthenticationMethod', 'aPNSChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with APNs, key or certificate.
--
-- 'enabled', 'aPNSChannelRequest_enabled' - Specifies whether to enable the APNs channel for the application.
--
-- 'privateKey', 'aPNSChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with APNs.
--
-- 'teamId', 'aPNSChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'tokenKey', 'aPNSChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'tokenKeyId', 'aPNSChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
newAPNSChannelRequest ::
  APNSChannelRequest
newAPNSChannelRequest =
  APNSChannelRequest'
    { bundleId = Prelude.Nothing,
      certificate = Prelude.Nothing,
      defaultAuthenticationMethod = Prelude.Nothing,
      enabled = Prelude.Nothing,
      privateKey = Prelude.Nothing,
      teamId = Prelude.Nothing,
      tokenKey = Prelude.Nothing,
      tokenKeyId = Prelude.Nothing
    }

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSChannelRequest_bundleId :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Text)
aPNSChannelRequest_bundleId = Lens.lens (\APNSChannelRequest' {bundleId} -> bundleId) (\s@APNSChannelRequest' {} a -> s {bundleId = a} :: APNSChannelRequest)

-- | The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with APNs by using an APNs certificate.
aPNSChannelRequest_certificate :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Text)
aPNSChannelRequest_certificate = Lens.lens (\APNSChannelRequest' {certificate} -> certificate) (\s@APNSChannelRequest' {} a -> s {certificate = a} :: APNSChannelRequest)

-- | The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with APNs, key or certificate.
aPNSChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Text)
aPNSChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSChannelRequest)

-- | Specifies whether to enable the APNs channel for the application.
aPNSChannelRequest_enabled :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Bool)
aPNSChannelRequest_enabled = Lens.lens (\APNSChannelRequest' {enabled} -> enabled) (\s@APNSChannelRequest' {} a -> s {enabled = a} :: APNSChannelRequest)

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with APNs.
aPNSChannelRequest_privateKey :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Text)
aPNSChannelRequest_privateKey = Lens.lens (\APNSChannelRequest' {privateKey} -> privateKey) (\s@APNSChannelRequest' {} a -> s {privateKey = a} :: APNSChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSChannelRequest_teamId :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Text)
aPNSChannelRequest_teamId = Lens.lens (\APNSChannelRequest' {teamId} -> teamId) (\s@APNSChannelRequest' {} a -> s {teamId = a} :: APNSChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSChannelRequest_tokenKey :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Text)
aPNSChannelRequest_tokenKey = Lens.lens (\APNSChannelRequest' {tokenKey} -> tokenKey) (\s@APNSChannelRequest' {} a -> s {tokenKey = a} :: APNSChannelRequest)

-- | The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with APNs by using APNs tokens.
aPNSChannelRequest_tokenKeyId :: Lens.Lens' APNSChannelRequest (Prelude.Maybe Prelude.Text)
aPNSChannelRequest_tokenKeyId = Lens.lens (\APNSChannelRequest' {tokenKeyId} -> tokenKeyId) (\s@APNSChannelRequest' {} a -> s {tokenKeyId = a} :: APNSChannelRequest)

instance Prelude.Hashable APNSChannelRequest where
  hashWithSalt _salt APNSChannelRequest' {..} =
    _salt
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` defaultAuthenticationMethod
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` privateKey
      `Prelude.hashWithSalt` teamId
      `Prelude.hashWithSalt` tokenKey
      `Prelude.hashWithSalt` tokenKeyId

instance Prelude.NFData APNSChannelRequest where
  rnf APNSChannelRequest' {..} =
    Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf defaultAuthenticationMethod
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf privateKey
      `Prelude.seq` Prelude.rnf teamId
      `Prelude.seq` Prelude.rnf tokenKey
      `Prelude.seq` Prelude.rnf tokenKeyId

instance Data.ToJSON APNSChannelRequest where
  toJSON APNSChannelRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BundleId" Data..=) Prelude.<$> bundleId,
            ("Certificate" Data..=) Prelude.<$> certificate,
            ("DefaultAuthenticationMethod" Data..=)
              Prelude.<$> defaultAuthenticationMethod,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("PrivateKey" Data..=) Prelude.<$> privateKey,
            ("TeamId" Data..=) Prelude.<$> teamId,
            ("TokenKey" Data..=) Prelude.<$> tokenKey,
            ("TokenKeyId" Data..=) Prelude.<$> tokenKeyId
          ]
      )
