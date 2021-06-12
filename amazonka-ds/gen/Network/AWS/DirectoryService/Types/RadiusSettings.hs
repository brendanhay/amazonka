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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
import qualified Network.AWS.Lens as Lens

-- | Contains information about a Remote Authentication Dial In User Service
-- (RADIUS) server.
--
-- /See:/ 'newRadiusSettings' smart constructor.
data RadiusSettings = RadiusSettings'
  { -- | Not currently used.
    useSameUsername :: Core.Maybe Core.Bool,
    -- | Not currently used.
    displayLabel :: Core.Maybe Core.Text,
    -- | An array of strings that contains the fully qualified domain name (FQDN)
    -- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
    -- addresses of your RADIUS server load balancer.
    radiusServers :: Core.Maybe [Core.Text],
    -- | The maximum number of times that communication with the RADIUS server is
    -- attempted.
    radiusRetries :: Core.Maybe Core.Natural,
    -- | The amount of time, in seconds, to wait for the RADIUS server to
    -- respond.
    radiusTimeout :: Core.Maybe Core.Natural,
    -- | Required for enabling RADIUS on the directory.
    sharedSecret :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The port that your RADIUS server is using for communications. Your
    -- on-premises network must allow inbound traffic over this port from the
    -- AWS Directory Service servers.
    radiusPort :: Core.Maybe Core.Natural,
    -- | The protocol specified for your RADIUS endpoints.
    authenticationProtocol :: Core.Maybe RadiusAuthenticationProtocol
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
    { useSameUsername = Core.Nothing,
      displayLabel = Core.Nothing,
      radiusServers = Core.Nothing,
      radiusRetries = Core.Nothing,
      radiusTimeout = Core.Nothing,
      sharedSecret = Core.Nothing,
      radiusPort = Core.Nothing,
      authenticationProtocol = Core.Nothing
    }

-- | Not currently used.
radiusSettings_useSameUsername :: Lens.Lens' RadiusSettings (Core.Maybe Core.Bool)
radiusSettings_useSameUsername = Lens.lens (\RadiusSettings' {useSameUsername} -> useSameUsername) (\s@RadiusSettings' {} a -> s {useSameUsername = a} :: RadiusSettings)

-- | Not currently used.
radiusSettings_displayLabel :: Lens.Lens' RadiusSettings (Core.Maybe Core.Text)
radiusSettings_displayLabel = Lens.lens (\RadiusSettings' {displayLabel} -> displayLabel) (\s@RadiusSettings' {} a -> s {displayLabel = a} :: RadiusSettings)

-- | An array of strings that contains the fully qualified domain name (FQDN)
-- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
-- addresses of your RADIUS server load balancer.
radiusSettings_radiusServers :: Lens.Lens' RadiusSettings (Core.Maybe [Core.Text])
radiusSettings_radiusServers = Lens.lens (\RadiusSettings' {radiusServers} -> radiusServers) (\s@RadiusSettings' {} a -> s {radiusServers = a} :: RadiusSettings) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of times that communication with the RADIUS server is
-- attempted.
radiusSettings_radiusRetries :: Lens.Lens' RadiusSettings (Core.Maybe Core.Natural)
radiusSettings_radiusRetries = Lens.lens (\RadiusSettings' {radiusRetries} -> radiusRetries) (\s@RadiusSettings' {} a -> s {radiusRetries = a} :: RadiusSettings)

-- | The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
radiusSettings_radiusTimeout :: Lens.Lens' RadiusSettings (Core.Maybe Core.Natural)
radiusSettings_radiusTimeout = Lens.lens (\RadiusSettings' {radiusTimeout} -> radiusTimeout) (\s@RadiusSettings' {} a -> s {radiusTimeout = a} :: RadiusSettings)

-- | Required for enabling RADIUS on the directory.
radiusSettings_sharedSecret :: Lens.Lens' RadiusSettings (Core.Maybe Core.Text)
radiusSettings_sharedSecret = Lens.lens (\RadiusSettings' {sharedSecret} -> sharedSecret) (\s@RadiusSettings' {} a -> s {sharedSecret = a} :: RadiusSettings) Core.. Lens.mapping Core._Sensitive

-- | The port that your RADIUS server is using for communications. Your
-- on-premises network must allow inbound traffic over this port from the
-- AWS Directory Service servers.
radiusSettings_radiusPort :: Lens.Lens' RadiusSettings (Core.Maybe Core.Natural)
radiusSettings_radiusPort = Lens.lens (\RadiusSettings' {radiusPort} -> radiusPort) (\s@RadiusSettings' {} a -> s {radiusPort = a} :: RadiusSettings)

-- | The protocol specified for your RADIUS endpoints.
radiusSettings_authenticationProtocol :: Lens.Lens' RadiusSettings (Core.Maybe RadiusAuthenticationProtocol)
radiusSettings_authenticationProtocol = Lens.lens (\RadiusSettings' {authenticationProtocol} -> authenticationProtocol) (\s@RadiusSettings' {} a -> s {authenticationProtocol = a} :: RadiusSettings)

instance Core.FromJSON RadiusSettings where
  parseJSON =
    Core.withObject
      "RadiusSettings"
      ( \x ->
          RadiusSettings'
            Core.<$> (x Core..:? "UseSameUsername")
            Core.<*> (x Core..:? "DisplayLabel")
            Core.<*> (x Core..:? "RadiusServers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RadiusRetries")
            Core.<*> (x Core..:? "RadiusTimeout")
            Core.<*> (x Core..:? "SharedSecret")
            Core.<*> (x Core..:? "RadiusPort")
            Core.<*> (x Core..:? "AuthenticationProtocol")
      )

instance Core.Hashable RadiusSettings

instance Core.NFData RadiusSettings

instance Core.ToJSON RadiusSettings where
  toJSON RadiusSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UseSameUsername" Core..=)
              Core.<$> useSameUsername,
            ("DisplayLabel" Core..=) Core.<$> displayLabel,
            ("RadiusServers" Core..=) Core.<$> radiusServers,
            ("RadiusRetries" Core..=) Core.<$> radiusRetries,
            ("RadiusTimeout" Core..=) Core.<$> radiusTimeout,
            ("SharedSecret" Core..=) Core.<$> sharedSecret,
            ("RadiusPort" Core..=) Core.<$> radiusPort,
            ("AuthenticationProtocol" Core..=)
              Core.<$> authenticationProtocol
          ]
      )
