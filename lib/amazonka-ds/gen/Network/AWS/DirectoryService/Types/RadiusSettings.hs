{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RadiusSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusSettings
  ( RadiusSettings (..),

    -- * Smart constructor
    mkRadiusSettings,

    -- * Lenses
    rsDisplayLabel,
    rsRadiusRetries,
    rsAuthenticationProtocol,
    rsRadiusServers,
    rsUseSameUsername,
    rsSharedSecret,
    rsRadiusTimeout,
    rsRadiusPort,
  )
where

import Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a Remote Authentication Dial In User Service (RADIUS) server.
--
-- /See:/ 'mkRadiusSettings' smart constructor.
data RadiusSettings = RadiusSettings'
  { -- | Not currently used.
    displayLabel :: Lude.Maybe Lude.Text,
    -- | The maximum number of times that communication with the RADIUS server is attempted.
    radiusRetries :: Lude.Maybe Lude.Natural,
    -- | The protocol specified for your RADIUS endpoints.
    authenticationProtocol :: Lude.Maybe RadiusAuthenticationProtocol,
    -- | An array of strings that contains the fully qualified domain name (FQDN) or IP addresses of the RADIUS server endpoints, or the FQDN or IP addresses of your RADIUS server load balancer.
    radiusServers :: Lude.Maybe [Lude.Text],
    -- | Not currently used.
    useSameUsername :: Lude.Maybe Lude.Bool,
    -- | Required for enabling RADIUS on the directory.
    sharedSecret :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The amount of time, in seconds, to wait for the RADIUS server to respond.
    radiusTimeout :: Lude.Maybe Lude.Natural,
    -- | The port that your RADIUS server is using for communications. Your on-premises network must allow inbound traffic over this port from the AWS Directory Service servers.
    radiusPort :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RadiusSettings' with the minimum fields required to make a request.
--
-- * 'displayLabel' - Not currently used.
-- * 'radiusRetries' - The maximum number of times that communication with the RADIUS server is attempted.
-- * 'authenticationProtocol' - The protocol specified for your RADIUS endpoints.
-- * 'radiusServers' - An array of strings that contains the fully qualified domain name (FQDN) or IP addresses of the RADIUS server endpoints, or the FQDN or IP addresses of your RADIUS server load balancer.
-- * 'useSameUsername' - Not currently used.
-- * 'sharedSecret' - Required for enabling RADIUS on the directory.
-- * 'radiusTimeout' - The amount of time, in seconds, to wait for the RADIUS server to respond.
-- * 'radiusPort' - The port that your RADIUS server is using for communications. Your on-premises network must allow inbound traffic over this port from the AWS Directory Service servers.
mkRadiusSettings ::
  RadiusSettings
mkRadiusSettings =
  RadiusSettings'
    { displayLabel = Lude.Nothing,
      radiusRetries = Lude.Nothing,
      authenticationProtocol = Lude.Nothing,
      radiusServers = Lude.Nothing,
      useSameUsername = Lude.Nothing,
      sharedSecret = Lude.Nothing,
      radiusTimeout = Lude.Nothing,
      radiusPort = Lude.Nothing
    }

-- | Not currently used.
--
-- /Note:/ Consider using 'displayLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsDisplayLabel :: Lens.Lens' RadiusSettings (Lude.Maybe Lude.Text)
rsDisplayLabel = Lens.lens (displayLabel :: RadiusSettings -> Lude.Maybe Lude.Text) (\s a -> s {displayLabel = a} :: RadiusSettings)
{-# DEPRECATED rsDisplayLabel "Use generic-lens or generic-optics with 'displayLabel' instead." #-}

-- | The maximum number of times that communication with the RADIUS server is attempted.
--
-- /Note:/ Consider using 'radiusRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRadiusRetries :: Lens.Lens' RadiusSettings (Lude.Maybe Lude.Natural)
rsRadiusRetries = Lens.lens (radiusRetries :: RadiusSettings -> Lude.Maybe Lude.Natural) (\s a -> s {radiusRetries = a} :: RadiusSettings)
{-# DEPRECATED rsRadiusRetries "Use generic-lens or generic-optics with 'radiusRetries' instead." #-}

-- | The protocol specified for your RADIUS endpoints.
--
-- /Note:/ Consider using 'authenticationProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsAuthenticationProtocol :: Lens.Lens' RadiusSettings (Lude.Maybe RadiusAuthenticationProtocol)
rsAuthenticationProtocol = Lens.lens (authenticationProtocol :: RadiusSettings -> Lude.Maybe RadiusAuthenticationProtocol) (\s a -> s {authenticationProtocol = a} :: RadiusSettings)
{-# DEPRECATED rsAuthenticationProtocol "Use generic-lens or generic-optics with 'authenticationProtocol' instead." #-}

-- | An array of strings that contains the fully qualified domain name (FQDN) or IP addresses of the RADIUS server endpoints, or the FQDN or IP addresses of your RADIUS server load balancer.
--
-- /Note:/ Consider using 'radiusServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRadiusServers :: Lens.Lens' RadiusSettings (Lude.Maybe [Lude.Text])
rsRadiusServers = Lens.lens (radiusServers :: RadiusSettings -> Lude.Maybe [Lude.Text]) (\s a -> s {radiusServers = a} :: RadiusSettings)
{-# DEPRECATED rsRadiusServers "Use generic-lens or generic-optics with 'radiusServers' instead." #-}

-- | Not currently used.
--
-- /Note:/ Consider using 'useSameUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsUseSameUsername :: Lens.Lens' RadiusSettings (Lude.Maybe Lude.Bool)
rsUseSameUsername = Lens.lens (useSameUsername :: RadiusSettings -> Lude.Maybe Lude.Bool) (\s a -> s {useSameUsername = a} :: RadiusSettings)
{-# DEPRECATED rsUseSameUsername "Use generic-lens or generic-optics with 'useSameUsername' instead." #-}

-- | Required for enabling RADIUS on the directory.
--
-- /Note:/ Consider using 'sharedSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSharedSecret :: Lens.Lens' RadiusSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
rsSharedSecret = Lens.lens (sharedSecret :: RadiusSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sharedSecret = a} :: RadiusSettings)
{-# DEPRECATED rsSharedSecret "Use generic-lens or generic-optics with 'sharedSecret' instead." #-}

-- | The amount of time, in seconds, to wait for the RADIUS server to respond.
--
-- /Note:/ Consider using 'radiusTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRadiusTimeout :: Lens.Lens' RadiusSettings (Lude.Maybe Lude.Natural)
rsRadiusTimeout = Lens.lens (radiusTimeout :: RadiusSettings -> Lude.Maybe Lude.Natural) (\s a -> s {radiusTimeout = a} :: RadiusSettings)
{-# DEPRECATED rsRadiusTimeout "Use generic-lens or generic-optics with 'radiusTimeout' instead." #-}

-- | The port that your RADIUS server is using for communications. Your on-premises network must allow inbound traffic over this port from the AWS Directory Service servers.
--
-- /Note:/ Consider using 'radiusPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRadiusPort :: Lens.Lens' RadiusSettings (Lude.Maybe Lude.Natural)
rsRadiusPort = Lens.lens (radiusPort :: RadiusSettings -> Lude.Maybe Lude.Natural) (\s a -> s {radiusPort = a} :: RadiusSettings)
{-# DEPRECATED rsRadiusPort "Use generic-lens or generic-optics with 'radiusPort' instead." #-}

instance Lude.FromJSON RadiusSettings where
  parseJSON =
    Lude.withObject
      "RadiusSettings"
      ( \x ->
          RadiusSettings'
            Lude.<$> (x Lude..:? "DisplayLabel")
            Lude.<*> (x Lude..:? "RadiusRetries")
            Lude.<*> (x Lude..:? "AuthenticationProtocol")
            Lude.<*> (x Lude..:? "RadiusServers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UseSameUsername")
            Lude.<*> (x Lude..:? "SharedSecret")
            Lude.<*> (x Lude..:? "RadiusTimeout")
            Lude.<*> (x Lude..:? "RadiusPort")
      )

instance Lude.ToJSON RadiusSettings where
  toJSON RadiusSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayLabel" Lude..=) Lude.<$> displayLabel,
            ("RadiusRetries" Lude..=) Lude.<$> radiusRetries,
            ("AuthenticationProtocol" Lude..=) Lude.<$> authenticationProtocol,
            ("RadiusServers" Lude..=) Lude.<$> radiusServers,
            ("UseSameUsername" Lude..=) Lude.<$> useSameUsername,
            ("SharedSecret" Lude..=) Lude.<$> sharedSecret,
            ("RadiusTimeout" Lude..=) Lude.<$> radiusTimeout,
            ("RadiusPort" Lude..=) Lude.<$> radiusPort
          ]
      )
