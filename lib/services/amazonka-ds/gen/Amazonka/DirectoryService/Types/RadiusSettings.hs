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
-- Module      : Amazonka.DirectoryService.Types.RadiusSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.RadiusSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.RadiusAuthenticationProtocol
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a Remote Authentication Dial In User Service
-- (RADIUS) server.
--
-- /See:/ 'newRadiusSettings' smart constructor.
data RadiusSettings = RadiusSettings'
  { -- | Not currently used.
    displayLabel :: Prelude.Maybe Prelude.Text,
    -- | The protocol specified for your RADIUS endpoints.
    authenticationProtocol :: Prelude.Maybe RadiusAuthenticationProtocol,
    -- | An array of strings that contains the fully qualified domain name (FQDN)
    -- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
    -- addresses of your RADIUS server load balancer.
    radiusServers :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of times that communication with the RADIUS server is
    -- attempted.
    radiusRetries :: Prelude.Maybe Prelude.Natural,
    -- | The port that your RADIUS server is using for communications. Your
    -- self-managed network must allow inbound traffic over this port from the
    -- Directory Service servers.
    radiusPort :: Prelude.Maybe Prelude.Natural,
    -- | Required for enabling RADIUS on the directory.
    sharedSecret :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Not currently used.
    useSameUsername :: Prelude.Maybe Prelude.Bool,
    -- | The amount of time, in seconds, to wait for the RADIUS server to
    -- respond.
    radiusTimeout :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadiusSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayLabel', 'radiusSettings_displayLabel' - Not currently used.
--
-- 'authenticationProtocol', 'radiusSettings_authenticationProtocol' - The protocol specified for your RADIUS endpoints.
--
-- 'radiusServers', 'radiusSettings_radiusServers' - An array of strings that contains the fully qualified domain name (FQDN)
-- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
-- addresses of your RADIUS server load balancer.
--
-- 'radiusRetries', 'radiusSettings_radiusRetries' - The maximum number of times that communication with the RADIUS server is
-- attempted.
--
-- 'radiusPort', 'radiusSettings_radiusPort' - The port that your RADIUS server is using for communications. Your
-- self-managed network must allow inbound traffic over this port from the
-- Directory Service servers.
--
-- 'sharedSecret', 'radiusSettings_sharedSecret' - Required for enabling RADIUS on the directory.
--
-- 'useSameUsername', 'radiusSettings_useSameUsername' - Not currently used.
--
-- 'radiusTimeout', 'radiusSettings_radiusTimeout' - The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
newRadiusSettings ::
  RadiusSettings
newRadiusSettings =
  RadiusSettings'
    { displayLabel = Prelude.Nothing,
      authenticationProtocol = Prelude.Nothing,
      radiusServers = Prelude.Nothing,
      radiusRetries = Prelude.Nothing,
      radiusPort = Prelude.Nothing,
      sharedSecret = Prelude.Nothing,
      useSameUsername = Prelude.Nothing,
      radiusTimeout = Prelude.Nothing
    }

-- | Not currently used.
radiusSettings_displayLabel :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Text)
radiusSettings_displayLabel = Lens.lens (\RadiusSettings' {displayLabel} -> displayLabel) (\s@RadiusSettings' {} a -> s {displayLabel = a} :: RadiusSettings)

-- | The protocol specified for your RADIUS endpoints.
radiusSettings_authenticationProtocol :: Lens.Lens' RadiusSettings (Prelude.Maybe RadiusAuthenticationProtocol)
radiusSettings_authenticationProtocol = Lens.lens (\RadiusSettings' {authenticationProtocol} -> authenticationProtocol) (\s@RadiusSettings' {} a -> s {authenticationProtocol = a} :: RadiusSettings)

-- | An array of strings that contains the fully qualified domain name (FQDN)
-- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
-- addresses of your RADIUS server load balancer.
radiusSettings_radiusServers :: Lens.Lens' RadiusSettings (Prelude.Maybe [Prelude.Text])
radiusSettings_radiusServers = Lens.lens (\RadiusSettings' {radiusServers} -> radiusServers) (\s@RadiusSettings' {} a -> s {radiusServers = a} :: RadiusSettings) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of times that communication with the RADIUS server is
-- attempted.
radiusSettings_radiusRetries :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusRetries = Lens.lens (\RadiusSettings' {radiusRetries} -> radiusRetries) (\s@RadiusSettings' {} a -> s {radiusRetries = a} :: RadiusSettings)

-- | The port that your RADIUS server is using for communications. Your
-- self-managed network must allow inbound traffic over this port from the
-- Directory Service servers.
radiusSettings_radiusPort :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusPort = Lens.lens (\RadiusSettings' {radiusPort} -> radiusPort) (\s@RadiusSettings' {} a -> s {radiusPort = a} :: RadiusSettings)

-- | Required for enabling RADIUS on the directory.
radiusSettings_sharedSecret :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Text)
radiusSettings_sharedSecret = Lens.lens (\RadiusSettings' {sharedSecret} -> sharedSecret) (\s@RadiusSettings' {} a -> s {sharedSecret = a} :: RadiusSettings) Prelude.. Lens.mapping Data._Sensitive

-- | Not currently used.
radiusSettings_useSameUsername :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Bool)
radiusSettings_useSameUsername = Lens.lens (\RadiusSettings' {useSameUsername} -> useSameUsername) (\s@RadiusSettings' {} a -> s {useSameUsername = a} :: RadiusSettings)

-- | The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
radiusSettings_radiusTimeout :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusTimeout = Lens.lens (\RadiusSettings' {radiusTimeout} -> radiusTimeout) (\s@RadiusSettings' {} a -> s {radiusTimeout = a} :: RadiusSettings)

instance Data.FromJSON RadiusSettings where
  parseJSON =
    Data.withObject
      "RadiusSettings"
      ( \x ->
          RadiusSettings'
            Prelude.<$> (x Data..:? "DisplayLabel")
            Prelude.<*> (x Data..:? "AuthenticationProtocol")
            Prelude.<*> (x Data..:? "RadiusServers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RadiusRetries")
            Prelude.<*> (x Data..:? "RadiusPort")
            Prelude.<*> (x Data..:? "SharedSecret")
            Prelude.<*> (x Data..:? "UseSameUsername")
            Prelude.<*> (x Data..:? "RadiusTimeout")
      )

instance Prelude.Hashable RadiusSettings where
  hashWithSalt _salt RadiusSettings' {..} =
    _salt `Prelude.hashWithSalt` displayLabel
      `Prelude.hashWithSalt` authenticationProtocol
      `Prelude.hashWithSalt` radiusServers
      `Prelude.hashWithSalt` radiusRetries
      `Prelude.hashWithSalt` radiusPort
      `Prelude.hashWithSalt` sharedSecret
      `Prelude.hashWithSalt` useSameUsername
      `Prelude.hashWithSalt` radiusTimeout

instance Prelude.NFData RadiusSettings where
  rnf RadiusSettings' {..} =
    Prelude.rnf displayLabel
      `Prelude.seq` Prelude.rnf authenticationProtocol
      `Prelude.seq` Prelude.rnf radiusServers
      `Prelude.seq` Prelude.rnf radiusRetries
      `Prelude.seq` Prelude.rnf radiusPort
      `Prelude.seq` Prelude.rnf sharedSecret
      `Prelude.seq` Prelude.rnf useSameUsername
      `Prelude.seq` Prelude.rnf radiusTimeout

instance Data.ToJSON RadiusSettings where
  toJSON RadiusSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayLabel" Data..=) Prelude.<$> displayLabel,
            ("AuthenticationProtocol" Data..=)
              Prelude.<$> authenticationProtocol,
            ("RadiusServers" Data..=) Prelude.<$> radiusServers,
            ("RadiusRetries" Data..=) Prelude.<$> radiusRetries,
            ("RadiusPort" Data..=) Prelude.<$> radiusPort,
            ("SharedSecret" Data..=) Prelude.<$> sharedSecret,
            ("UseSameUsername" Data..=)
              Prelude.<$> useSameUsername,
            ("RadiusTimeout" Data..=) Prelude.<$> radiusTimeout
          ]
      )
