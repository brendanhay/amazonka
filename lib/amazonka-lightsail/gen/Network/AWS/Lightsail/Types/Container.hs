-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Container
  ( Container (..),

    -- * Smart constructor
    mkContainer,

    -- * Lenses
    cImage,
    cCommand,
    cEnvironment,
    cPorts,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContainerServiceProtocol
import qualified Network.AWS.Prelude as Lude

-- | Describes the settings of a container that will be launched, or that is launched, to an Amazon Lightsail container service.
--
-- /See:/ 'mkContainer' smart constructor.
data Container = Container'
  { image :: Lude.Maybe Lude.Text,
    command :: Lude.Maybe [Lude.Text],
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    ports ::
      Lude.Maybe (Lude.HashMap Lude.Text (ContainerServiceProtocol))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- * 'command' - The launch command for the container.
-- * 'environment' - The environment variables of the container.
-- * 'image' - The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
-- * 'ports' - The open firewall ports of the container.
mkContainer ::
  Container
mkContainer =
  Container'
    { image = Lude.Nothing,
      command = Lude.Nothing,
      environment = Lude.Nothing,
      ports = Lude.Nothing
    }

-- | The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cImage :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cImage = Lens.lens (image :: Container -> Lude.Maybe Lude.Text) (\s a -> s {image = a} :: Container)
{-# DEPRECATED cImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The launch command for the container.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommand :: Lens.Lens' Container (Lude.Maybe [Lude.Text])
cCommand = Lens.lens (command :: Container -> Lude.Maybe [Lude.Text]) (\s a -> s {command = a} :: Container)
{-# DEPRECATED cCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The environment variables of the container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnvironment :: Lens.Lens' Container (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cEnvironment = Lens.lens (environment :: Container -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: Container)
{-# DEPRECATED cEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The open firewall ports of the container.
--
-- /Note:/ Consider using 'ports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPorts :: Lens.Lens' Container (Lude.Maybe (Lude.HashMap Lude.Text (ContainerServiceProtocol)))
cPorts = Lens.lens (ports :: Container -> Lude.Maybe (Lude.HashMap Lude.Text (ContainerServiceProtocol))) (\s a -> s {ports = a} :: Container)
{-# DEPRECATED cPorts "Use generic-lens or generic-optics with 'ports' instead." #-}

instance Lude.FromJSON Container where
  parseJSON =
    Lude.withObject
      "Container"
      ( \x ->
          Container'
            Lude.<$> (x Lude..:? "image")
            Lude.<*> (x Lude..:? "command" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ports" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Container where
  toJSON Container' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("image" Lude..=) Lude.<$> image,
            ("command" Lude..=) Lude.<$> command,
            ("environment" Lude..=) Lude.<$> environment,
            ("ports" Lude..=) Lude.<$> ports
          ]
      )
