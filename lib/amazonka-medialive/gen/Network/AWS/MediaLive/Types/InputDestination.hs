{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestination
  ( InputDestination (..),

    -- * Smart constructor
    mkInputDestination,

    -- * Lenses
    idURL,
    idIP,
    idVPC,
    idPort,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDestinationVPC
import qualified Network.AWS.Prelude as Lude

-- | The settings for a PUSH type input.
--
-- /See:/ 'mkInputDestination' smart constructor.
data InputDestination = InputDestination'
  { -- | This represents the endpoint that the customer stream will be
    --
    -- pushed to.
    url :: Lude.Maybe Lude.Text,
    -- | The system-generated static IP address of endpoint.
    --
    -- It remains fixed for the lifetime of the input.
    ip :: Lude.Maybe Lude.Text,
    vpc :: Lude.Maybe InputDestinationVPC,
    -- | The port number for the input.
    port :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDestination' with the minimum fields required to make a request.
--
-- * 'url' - This represents the endpoint that the customer stream will be
--
-- pushed to.
-- * 'ip' - The system-generated static IP address of endpoint.
--
-- It remains fixed for the lifetime of the input.
-- * 'vpc' -
-- * 'port' - The port number for the input.
mkInputDestination ::
  InputDestination
mkInputDestination =
  InputDestination'
    { url = Lude.Nothing,
      ip = Lude.Nothing,
      vpc = Lude.Nothing,
      port = Lude.Nothing
    }

-- | This represents the endpoint that the customer stream will be
--
-- pushed to.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idURL :: Lens.Lens' InputDestination (Lude.Maybe Lude.Text)
idURL = Lens.lens (url :: InputDestination -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: InputDestination)
{-# DEPRECATED idURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The system-generated static IP address of endpoint.
--
-- It remains fixed for the lifetime of the input.
--
-- /Note:/ Consider using 'ip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idIP :: Lens.Lens' InputDestination (Lude.Maybe Lude.Text)
idIP = Lens.lens (ip :: InputDestination -> Lude.Maybe Lude.Text) (\s a -> s {ip = a} :: InputDestination)
{-# DEPRECATED idIP "Use generic-lens or generic-optics with 'ip' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idVPC :: Lens.Lens' InputDestination (Lude.Maybe InputDestinationVPC)
idVPC = Lens.lens (vpc :: InputDestination -> Lude.Maybe InputDestinationVPC) (\s a -> s {vpc = a} :: InputDestination)
{-# DEPRECATED idVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The port number for the input.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idPort :: Lens.Lens' InputDestination (Lude.Maybe Lude.Text)
idPort = Lens.lens (port :: InputDestination -> Lude.Maybe Lude.Text) (\s a -> s {port = a} :: InputDestination)
{-# DEPRECATED idPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON InputDestination where
  parseJSON =
    Lude.withObject
      "InputDestination"
      ( \x ->
          InputDestination'
            Lude.<$> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "ip")
            Lude.<*> (x Lude..:? "vpc")
            Lude.<*> (x Lude..:? "port")
      )
