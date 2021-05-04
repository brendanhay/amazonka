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
-- Module      : Network.AWS.DirectoryService.Types.RadiusSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusSettings where

import Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a Remote Authentication Dial In User Service
-- (RADIUS) server.
--
-- /See:/ 'newRadiusSettings' smart constructor.
data RadiusSettings = RadiusSettings'
  { -- | Not currently used.
    useSameUsername :: Prelude.Maybe Prelude.Bool,
    -- | Not currently used.
    displayLabel :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that contains the fully qualified domain name (FQDN)
    -- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
    -- addresses of your RADIUS server load balancer.
    radiusServers :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of times that communication with the RADIUS server is
    -- attempted.
    radiusRetries :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time, in seconds, to wait for the RADIUS server to
    -- respond.
    radiusTimeout :: Prelude.Maybe Prelude.Natural,
    -- | Required for enabling RADIUS on the directory.
    sharedSecret :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The port that your RADIUS server is using for communications. Your
    -- on-premises network must allow inbound traffic over this port from the
    -- AWS Directory Service servers.
    radiusPort :: Prelude.Maybe Prelude.Natural,
    -- | The protocol specified for your RADIUS endpoints.
    authenticationProtocol :: Prelude.Maybe RadiusAuthenticationProtocol
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RadiusSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useSameUsername', 'radiusSettings_useSameUsername' - Not currently used.
--
-- 'displayLabel', 'radiusSettings_displayLabel' - Not currently used.
--
-- 'radiusServers', 'radiusSettings_radiusServers' - An array of strings that contains the fully qualified domain name (FQDN)
-- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
-- addresses of your RADIUS server load balancer.
--
-- 'radiusRetries', 'radiusSettings_radiusRetries' - The maximum number of times that communication with the RADIUS server is
-- attempted.
--
-- 'radiusTimeout', 'radiusSettings_radiusTimeout' - The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
--
-- 'sharedSecret', 'radiusSettings_sharedSecret' - Required for enabling RADIUS on the directory.
--
-- 'radiusPort', 'radiusSettings_radiusPort' - The port that your RADIUS server is using for communications. Your
-- on-premises network must allow inbound traffic over this port from the
-- AWS Directory Service servers.
--
-- 'authenticationProtocol', 'radiusSettings_authenticationProtocol' - The protocol specified for your RADIUS endpoints.
newRadiusSettings ::
  RadiusSettings
newRadiusSettings =
  RadiusSettings'
    { useSameUsername = Prelude.Nothing,
      displayLabel = Prelude.Nothing,
      radiusServers = Prelude.Nothing,
      radiusRetries = Prelude.Nothing,
      radiusTimeout = Prelude.Nothing,
      sharedSecret = Prelude.Nothing,
      radiusPort = Prelude.Nothing,
      authenticationProtocol = Prelude.Nothing
    }

-- | Not currently used.
radiusSettings_useSameUsername :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Bool)
radiusSettings_useSameUsername = Lens.lens (\RadiusSettings' {useSameUsername} -> useSameUsername) (\s@RadiusSettings' {} a -> s {useSameUsername = a} :: RadiusSettings)

-- | Not currently used.
radiusSettings_displayLabel :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Text)
radiusSettings_displayLabel = Lens.lens (\RadiusSettings' {displayLabel} -> displayLabel) (\s@RadiusSettings' {} a -> s {displayLabel = a} :: RadiusSettings)

-- | An array of strings that contains the fully qualified domain name (FQDN)
-- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
-- addresses of your RADIUS server load balancer.
radiusSettings_radiusServers :: Lens.Lens' RadiusSettings (Prelude.Maybe [Prelude.Text])
radiusSettings_radiusServers = Lens.lens (\RadiusSettings' {radiusServers} -> radiusServers) (\s@RadiusSettings' {} a -> s {radiusServers = a} :: RadiusSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of times that communication with the RADIUS server is
-- attempted.
radiusSettings_radiusRetries :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusRetries = Lens.lens (\RadiusSettings' {radiusRetries} -> radiusRetries) (\s@RadiusSettings' {} a -> s {radiusRetries = a} :: RadiusSettings)

-- | The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
radiusSettings_radiusTimeout :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusTimeout = Lens.lens (\RadiusSettings' {radiusTimeout} -> radiusTimeout) (\s@RadiusSettings' {} a -> s {radiusTimeout = a} :: RadiusSettings)

-- | Required for enabling RADIUS on the directory.
radiusSettings_sharedSecret :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Text)
radiusSettings_sharedSecret = Lens.lens (\RadiusSettings' {sharedSecret} -> sharedSecret) (\s@RadiusSettings' {} a -> s {sharedSecret = a} :: RadiusSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | The port that your RADIUS server is using for communications. Your
-- on-premises network must allow inbound traffic over this port from the
-- AWS Directory Service servers.
radiusSettings_radiusPort :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusPort = Lens.lens (\RadiusSettings' {radiusPort} -> radiusPort) (\s@RadiusSettings' {} a -> s {radiusPort = a} :: RadiusSettings)

-- | The protocol specified for your RADIUS endpoints.
radiusSettings_authenticationProtocol :: Lens.Lens' RadiusSettings (Prelude.Maybe RadiusAuthenticationProtocol)
radiusSettings_authenticationProtocol = Lens.lens (\RadiusSettings' {authenticationProtocol} -> authenticationProtocol) (\s@RadiusSettings' {} a -> s {authenticationProtocol = a} :: RadiusSettings)

instance Prelude.FromJSON RadiusSettings where
  parseJSON =
    Prelude.withObject
      "RadiusSettings"
      ( \x ->
          RadiusSettings'
            Prelude.<$> (x Prelude..:? "UseSameUsername")
            Prelude.<*> (x Prelude..:? "DisplayLabel")
            Prelude.<*> ( x Prelude..:? "RadiusServers"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "RadiusRetries")
            Prelude.<*> (x Prelude..:? "RadiusTimeout")
            Prelude.<*> (x Prelude..:? "SharedSecret")
            Prelude.<*> (x Prelude..:? "RadiusPort")
            Prelude.<*> (x Prelude..:? "AuthenticationProtocol")
      )

instance Prelude.Hashable RadiusSettings

instance Prelude.NFData RadiusSettings

instance Prelude.ToJSON RadiusSettings where
  toJSON RadiusSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("UseSameUsername" Prelude..=)
              Prelude.<$> useSameUsername,
            ("DisplayLabel" Prelude..=) Prelude.<$> displayLabel,
            ("RadiusServers" Prelude..=)
              Prelude.<$> radiusServers,
            ("RadiusRetries" Prelude..=)
              Prelude.<$> radiusRetries,
            ("RadiusTimeout" Prelude..=)
              Prelude.<$> radiusTimeout,
            ("SharedSecret" Prelude..=) Prelude.<$> sharedSecret,
            ("RadiusPort" Prelude..=) Prelude.<$> radiusPort,
            ("AuthenticationProtocol" Prelude..=)
              Prelude.<$> authenticationProtocol
          ]
      )
